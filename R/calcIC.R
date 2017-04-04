#'
#'@title Calculate individual connectivity for one DisMELS model run
#'
#'@description Function to calculate individual connectivity based on one DisMELS model run.
#'
#' @param ibmResTbl - name of DisMELS connectivity results file or resulting dataframe (can be NULL)
#' @param cellsTbl - name of classified grid cells file or resulting dataframe (can be NULL)
#' @param lhsTypeInfo - list object with life stage info
#' @param spawningZones - vector of names of zones used as spawning areas in the IBM
#' @param writeCSV - flag (T/F) to write output file 
#' @param outIndivs - csv filename for individual connectivity (start and end locations)
#' 
#' @details 
#' If the ibmResTbl dataframe is NULL, the user can select the file using a 
#' file dialog box.\cr \cr
#' 
#' If the cellsTbl dataframe is NULL, the user can select the file using a 
#' file dialog box.\cr \cr
#' 
#' If the writeCSV flag is set, the inidividual connectivity data frame (dfrIC) 
#' is written to the file specified by 'outIndivs'.
#' 
#' @return  dataframe of start and end locations for each individual in the model run
#' 
#' @import sqldf 
#' @import wtsUtilities
#' 
#' @export
#'
#*********************************************************
#  Calculate connectivity matrix for one model run
#**********************************************************/
calcIC<-function(ibmResTbl=NULL,
                 cellsTbl=NULL, 
                 lhsTypeInfo=NULL,            
                 spawningZones=c("SpawningArea_300to600m"),                 #spawning area name(s)
                 writeCSV=FALSE,
                 outIndivs="IndivStartEndPositions.csv"                     #output indivs csv file
                 ){
  #create IBM connectivity results table (if not an input)
  if (!is.data.frame(ibmResTbl)){
    cat("Reading connectivity results file.\n")
    if (is.null(ibmResTbl)) {
        ibmResTbl<-getCSV(caption="Select DisMELS connectivity results file");
        if (is.null(ibmResTbl)) return(NULL);
    } else {
        ibmResTbl<-read.csv(ibmResTbl,stringsAsFactors=FALSE);
    }  
    cat("Done reading connectivity results file.\n")
  }
  
  #create classified cells table (if not an input)
  if (!is.data.frame(cellsTbl)){
    cat("Reading cells file.\n")
    if (is.null(cellsTbl)) {
        cellsTbl<-getCSV(caption="Select classified grid cells (csv) file");
        if (is.null(cellsTbl)) return(NULL);
    } else {
        cellsTbl<-read.csv(cellsTbl,stringsAsFactors=FALSE);
    }
    cat("Done reading cells file.\n")
  }
  
  #pull out life stage info
  lifeStages<-names(lhsTypeInfo$lifeStageTypes)
  lifeStages<-as.data.frame(list(stage=lifeStages,rank=1:length(lifeStages)));
  firstLHS  <-lifeStages$stage[1]; # name of first life stage
  
  #convert spawning zones to data frame
  spawningZones<-as.data.frame(list(zone=spawningZones));
  
  #drop columns NOT in stdVars
  stdVars<-getStdVars(lhsTypeInfo$resType);
  ibmResTbl<-ibmResTbl[,1:length(stdVars$vars)];
  
  #categorize starting points relative to spawning/source areas
  qry<-"select * 
          from ibmResTbl 
        where typeName='&&firstLHS' and ageInStage=0";
  qry<-gsub("&&firstLHS",firstLHS,qry);
  strtTbl1<-sqldf(qry);
  
  qry<-"select
          i.typeName as typeName,
          i.id as id,
          i.origID as origID,
          i.startTime as startTime,
          i.age as age,
          i.ageInStage as ageInStage,
          i.number as number,
          i.gridCellID as gridCellID,
          i.horizPos1 as lon,
          i.horizPos2 as lat,
          i.vertPos as depth,
          p.alongshorezone as alongshorezone,
          p.depthzone as depthzone
        from
          strtTbl1 as i,
          cellsTbl as p
        on
          i.gridCellID=p.ID and
          p.depthzone in spawningZones
        order by
          alongshorezone;" 
  strtTbl<-sqldf(qry); #this includes ONLY inidividuals released w/in defined spawning zones
  
  #select only inidividuals released in defined spawning areas
  qry<-"select * 
        from 
          ibmResTbl,
          (select id from strtTbl) s
        where 
          ibmResTbl.id = s.id";
  ibmResTbl<-sqldf(qry);
  #last column of ibmResTbl has duplicate name 'id', so drop it
  ibmResTbl<-ibmResTbl[,1:(ncol(ibmResTbl)-1)];
  
  #Find info on each individual at final age
  ##Do this in two steps and rank by life stage in first step because
  ##the ibm connectivity results file includes two entries for a given age, 
  ##corresponding to the before/after LHSs. Then can get final LHS in 
  ##second step according to ranking of LHS.
  ###Step 1: find oldest age by stage for each individual
  qry<-"select 
          x.id as ID,
          x.typename as typeName,
          l.rank as rank,
          x.age as age
        from
          (select 
             xx.id,
             xx.typename,
             xx.age
           from
             ibmResTbl xx,
             (select id, max(age) as mxage
              from ibmResTbl
              group by id) as mx
           where
             xx.id   = mx.id and
             xx.age  = mx.mxage) as x,
            lifeStages as l
        where
          x.typename = l.stage
        order by
          ID, rank;"
  tmpFinPts1<-sqldf(qry);
  ###Step 2: select by oldest life stage
  qry<-"select
          f.id as ID,
          f.typename as typeName,
          f.age as age
        from
          (select id,max(rank) as mxLHS
           from tmpFinPts1
           group by id) mx,
          tmpFinPts1 f
        where
          f.id   = mx.id and
          f.rank = mx.mxLHS
        order by 
          id;";
  tmpFinPts2<-sqldf(qry);
  
  #now extract info on ending locations
  qry<-"select
          i.typeName as typeName,
          i.ID as ID,
          i.origID as origID,
          i.time as endTime,
          i.age as age,
          i.ageInStage as ageInStage,
          i.number as number,
          i.horizPos1 as lon,
          i.horizPos2 as lat,
          i.vertPos as depth,
          i.gridCellID as gridCellID,
          p.alongshorezone as alongshorezone,
          p.depthzone as depthzone
      from
         (select x.typeName,x.ID,x.origID,x.time,x.age,
                 x.ageInStage,x.number,x.horizPos1,x.horizPos2,
                 x.vertPos,x.gridCellID 
          from 
              ibmResTbl x,
              tmpFinPts2 f
          where 
              x.typename = f.typeName and
              x.ID  = f.ID and
              x.age = f.age) i left join
          cellsTbl p
      on
          i.gridCellID=p.ID
      order by
          age, origID, ID;";  
  endTbl<-sqldf(qry);
  endTbl$alongshorezone[is.na(endTbl$alongshorezone)]<--1;
  endTbl$depthzone[is.na(endTbl$depthzone)]<-'exited grid';
  
  
  #construct connectivity table for individuals
    # note that this works only for ordinary indivs, not super individuals
  dfrIC<-sqldf("select
                s.ID as ID,
                s.origID as origID,
                s.typeName as start_typeName,
                e.typeName as end_typeName,
                s.startTime as start_time,
                e.endTime as end_time,
                e.age as age,
                e.ageInStage as ageInStage,
                s.number as numStart,
                e.number as numFinish,
                s.lon as startLon,
                s.lat as startLat,
                s.depth as startDepth,
                e.lon as endLon,
                e.lat as endLat,
                e.depth as endDepth,
                s.gridCellID as startGridCell,
                e.gridCellID as endGridCell,
                s.alongshorezone as start_alongshorezone,
                s.depthzone as start_depthzone,
                e.alongshorezone as end_alongshorezone,
                e.depthzone as end_depthzone
              from
                strtTbl as s,
                endTbl as e
              where
                s.origID=e.origID AND
                s.ID = e.ID
              order by
                origID,ID;");
  dfrIC$numFinish[is.na(dfrIC$numFinish)]<-0;
  
  if (writeCSV) write.csv(dfrIC,file=outIndivs,row.names=FALSE);

  return(dfrIC);
}

  