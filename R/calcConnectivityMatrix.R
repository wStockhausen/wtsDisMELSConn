#'
#'@title Calculate a connectivity matrix for one DisMELS model run.
#'
#'@description Function to calculate a connectivity matrix based on one DisMELS model run.
#'
#' @param ibmResTbl - name of DisMELS connectivity results file or resulting dataframe (can be NULL)
#' @param cellsTbl - name of classified grid cells file or resulting dataframe (can be NULL)
#' @param lhsTypeInfo - list object with life stage info
#' @param spawningZones - vector of names of zones used as spawning areas in the IBM
#' @param nurseryZones - vector of names of zones used as nursery areas in the IBM
#' @param writeCSVs - flag (T/F) to write output files 
#' @param outConn - csv filename for connectivity to nursery areas from spawning areas
#' @param outIndivs - csv filename for individual start and end locations
#' 
#' @details 
#' If the ibmResTbl dataframe is NULL, the user can select the file using a 
#' file dialog box).\cr \cr
#' 
#' If the cellsTbl dataframe is NULL, the user can select the file using a 
#' file dialog box).\cr \cr
#' 
#' If the writeCSVs flag is set, the two files specified by outConn and outIndivs are written before the function is exited.
#' 
#' @return list w/ 2 elements:\cr
#' prbSinkGivenSource - probability of transport to a sink (nursery area) conditioned on starting in a source (spawning area)\cr
#' numSource - numbers by source (spawning area)\cr
#' tblIndivConn - dataframe of start and end locations for each individual in the model run\cr
#' 
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities getCSV
#' 
#' @export
#'
#*********************************************************
#  Calculate connectivity matrix for one model run
#**********************************************************/
calcConnectivityMatrix<-function(ibmResTbl=NULL,
                                 cellsTbl=NULL, 
                                 lhsTypeInfo=getLifeStageInfo.ATF(),            
                                 spawningZones=c("SpawningArea_300to600m"),                       #spawning area name(s)
                                 nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), #nursery area name(s)
                                 writeCSVs=TRUE,
                                 outConn="ConnectivityToNurseryAreasFromSpawningAreas.csv", #output connectivity csv file
                                 outIndivs="IndivStartEndPositions.csv"                     #output indivs csv file
                                 ){
  #create IBM connectivity results table (if not an input)
  if (!is.data.frame(ibmResTbl)){
    cat("Reading connectivity results file.\n")
    if (is.null(ibmResTbl)) {
        ibmResTbl<-wtsUtilities::getCSV(caption="Select DisMELS connectivity results file");
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
        cellsTbl<-wtsUtilities::getCSV(caption="Select classified grid cells (csv) file");
        if (is.null(cellsTbl)) return(NULL);
    } else {
        cellsTbl<-read.csv(cellsTbl,stringsAsFactors=FALSE);
    }
    cat("Done reading cells file.\n")
  }
  
  #pull out life stage info
  lifeStages<-names(lhsTypeInfo$lifeStageTypes)
  firstLHS<-lifeStages[1];                 # name of first life stage
  lastLHS<-lifeStages[length(lifeStages)]; # name of final (recruiting) life stage
  cat("firstLHS = ",firstLHS,"\n")
  cat("lastLHS  = ",lastLHS,"\n")
  
  #turn these into dataframes for use in sql statements
  lifeStages   <-as.data.frame(list(stage=lifeStages,rank=1:length(lifeStages)));
  spawningZones<-as.data.frame(list(zone=spawningZones));
  nurseryZones <-as.data.frame(list(zone=nurseryZones));
  
  #create unique nursery zones table */
  uniqNurseryZones<-sqldf::sqldf("select distinct
                            depthzone      as depthzone,
                            alongshorezone as alongshorezone
                          from 
                            cellsTbl
                          where
                            depthzone in nurseryZones
                          order by
                            depthzone, alongshorezone;");
  uniqNurseryZones1<-rbind(uniqNurseryZones,list(depthzone='non-nursery areas',alongshorezone=-1));
  
  #drop columns NOT in stdVars
  stdVars<-getStdVars(lhsTypeInfo$resType=='NEW');
  ibmResTbl<-ibmResTbl[,1:length(stdVars$vars)];
  
  #categorize starting points relative to spawning/source areas
  qry<-"select * 
          from ibmResTbl 
        where typeName='&&firstLHS' and ageInStage=0";
  qry<-gsub("&&firstLHS",firstLHS,qry);
  strtTbl1<-sqldf::sqldf(qry);
  
  qry<-"select
          i.typeName as typeName,
          i.id as id,
          i.origID as origID,
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
  strtTbl<-sqldf::sqldf(qry); #this includes ONLY inidividuals released w/in defined spawning zones
  
  #create unique spawning zones table
  uniqSpawningZones<-sqldf::sqldf("select distinct
                              depthzone,
                              alongshorezone
                            from 
                              strtTbl
                            order by
                              depthzone, alongshorezone;");
  
  #create unique spawning/nursery zones table
  uniqSpawningNurseryZones1<-sqldf::sqldf("select
                                      s.depthzone      as spawning_depthzone,
                                      s.alongshorezone as spawning_alongshorezone,
                                      n.depthzone      as nursery_depthzone,
                                      n.alongshorezone as nursery_alongshorezone
                                    from 
                                      uniqSpawningZones as s,
                                      uniqNurseryZones1 as n
                                    order by
                                      spawning_depthzone, 
                                      spawning_alongshorezone,
                                      nursery_depthzone, 
                                      nursery_alongshorezone;");
  
  ibmResTbl1<-ibmResTbl;
  qry<-"select * 
        from 
          ibmResTbl1,
          (select id from strtTbl) s
        where 
          ibmResTbl1.id = s.id";
  ibmResTbl<-sqldf::sqldf(qry);
  #last column of ibmResTbl has duplicate name 'id'
  #need to replace it with unique name
  nms<-names(ibmResTbl)
  names(ibmResTbl)<-c(nms[1:(length(nms)-1)],'id1');
  
  
  #categorize ending points relative to nursery areas/sink polygons
  qry<-"select 
            x.id as ID,
            x.typeName as typeName,
            x.age as age,
            x.ageInStage as ageInStage
          from
            (select 
               xx.id,
               xx.typeName,
               xx.age,
               xx.ageInStage
             from
               ibmResTbl xx,
               (select id, typeName, max(ageInStage) as mxAgeInStage
                from ibmResTbl
                where typeName = '&&lastLHS'
                group by id, typeName) as mx
             where
               xx.typeName = mx.typeName and
               xx.id   = mx.id and
               xx.ageInStage  = mx.mxAgeInStage) as x
          where
            x.typeName = '&&lastLHS'
          order by
            id;"
  qry<-gsub("&&lastLHS",lastLHS,qry);
  tmpFinPts0<-sqldf::sqldf(qry);

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
  tmpFinPts1<-sqldf::sqldf(qry);
    
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
  tmpFinPts2<-sqldf::sqldf(qry);
  
  #table of ending locations
  endTbl0<-sqldf::sqldf("select
                    i.typeName as typeName,
                    i.ID as ID,
                    i.origID as origID,
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
                    (select 
                       x.typeName,x.ID,x.origID,x.age,x.ageInStage,
                       x.number,x.gridCellID,x.horizPos1,x.horizPos2,x.vertPos 
                     from 
                       ibmResTbl x,
                       tmpFinPts0 f
                     where 
                       x.typeName = f.typeName and
                       x.ID  = f.ID and
                       x.ageInStage = f.ageInStage) as i,
                    cellsTbl as p
                  where
                    i.gridCellID=p.ID and
                    p.depthzone in nurseryZones
                  order by
                    gridCellID;");

  qry<-"select
          i.typeName as typeName,
          i.ID as ID,
          i.origID as origID,
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
         (select x.typeName,x.ID,x.origID,x.age,
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
  endTbl<-sqldf::sqldf(qry);
  endTbl$alongshorezone[is.na(endTbl$alongshorezone)]<--1;
  endTbl$depthzone[is.na(endTbl$depthzone)]<-'exited grid';
  
  
  #construct connectivity table for individuals
    # note that this works only for ordinary indivs, not super individuals
  tblIndivConn<-sqldf::sqldf("select
                          s.ID as ID,
                          s.origID as origID,
                          s.typeName as start_typeName,
                          e.typeName as end_typeName,
                          e.age as age,
                          e.ageInStage as ageInStage,
                          s.number as numStart,
                          e.number as numFinish,
                          s.lon as startLon,
                          s.lat as startLat,
                          e.lon as endLon,
                          e.lat as endLat,
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
  tblIndivConn$numFinish[is.na(tblIndivConn$numFinish)]<-0;
  
    #****************************************************************************
    #                    doAnalysis                                             *
    # Purpose                                                                   *
    #   Analyze the individual connectivity data and construct various          *
    #   source-sink summary tables.                                             *
    #***************************************************************************/
  numSourceSink1<-sqldf::sqldf("select
                           start_depthzone,
                           start_alongshorezone,
                           end_depthzone,
                           end_alongshorezone,
                           start_typeName,
                           end_typeName,
                           sum(numStart) as totRel,
                           sum(numFinish) as totFin
                         from
                           tblIndivConn
                         group by
                           start_alongshorezone,
                           start_depthzone,
                           end_alongshorezone,
                           end_depthzone,                          
                           start_typeName,
                           end_typeName
                        order by
                          start_depthzone,
                          start_alongshorezone,
                          end_depthzone,
                          end_alongshorezone,
                          start_typeName,
                          end_typeName;");
  qry<-"select
          n.start_depthzone      as start_depthzone,
          n.start_alongshorezone as start_alongshorezone,
          u.depthzone            as end_depthzone,
          u.alongshorezone       as end_alongshorezone,
          sum(n.totRel)          as totRel,
          sum(n.totFin)          as totFin
        from
          (select *
           from numSourceSink1
           where end_typeName='&&lastLHS') as n left join
          uniqNurseryZones as u
        on
          n.end_depthzone        = u.depthzone      AND
          n.end_alongshorezone   = u.alongshorezone
        group by
           n.start_alongshorezone,
           n.start_depthzone,
           u.alongshorezone,
           u.depthzone
        order by
          start_depthzone,
          start_alongshorezone,
          end_depthzone,
          end_alongshorezone;";
  qry<-gsub("&&lastLHS",lastLHS,qry);
  numSourceSink2<-sqldf::sqldf(qry);
  numSourceSink2$end_depthzone[is.na(numSourceSink2$end_depthzone)]<-'non-nursery areas';
  numSourceSink2$end_alongshorezone[is.na(numSourceSink2$end_alongshorezone)]<--1;
  
  numSourceSink<-sqldf::sqldf("select
                            u.spawning_depthzone      as start_depthzone,
                            u.spawning_alongshorezone as start_alongshorezone,
                            u.nursery_depthzone       as end_depthzone,
                            u.nursery_alongshorezone  as end_alongshorezone,
                            n.totRel             as totRel,
                            n.totFin             as totFin
                          from
                            uniqSpawningNurseryZones1 as u left join
                            numSourceSink2 as n
                          on
                            n.start_depthzone      = u.spawning_depthzone      AND
                            n.start_alongshorezone = u.spawning_alongshorezone AND
                            n.end_depthzone        = u.nursery_depthzone      AND
                            n.end_alongshorezone   = u.nursery_alongshorezone
                          order by
                            start_depthzone,
                            start_alongshorezone,
                            end_depthzone,
                            end_alongshorezone;");
                          
  numSourceSink$totRel[is.na(numSourceSink$totRel)]<-0;
  numSourceSink$totFin[is.na(numSourceSink$totFin)]<-0;
  
  #***********************************************************
  #***********************************************************
  numSource<-sqldf::sqldf("select
                      r.start_depthzone as start_depthzone,
                      r.start_alongshorezone as start_alongshorezone,
                      r.totRel as totRel,
                      f.totSet as totSet,
                      f.totSet/r.totRel as pSrv
                    from
                      (select
                         start_depthzone,
                         start_alongshorezone,
                         sum(totRel) as totRel
                       from
                         numSourceSink1
                       group by
                         start_depthzone, start_alongshorezone) as r left join
                      (select
                         start_depthzone,
                         start_alongshorezone,
                         sum(totFin) as totSet
                       from
                         numSourceSink2
                       where
                         end_alongshorezone>0
                       group by
                         start_depthzone, start_alongshorezone) as f
                    on
                      r.start_depthzone      = f.start_depthzone AND
                      r.start_alongshorezone = f.start_alongshorezone
                    order by
                      start_depthzone, start_alongshorezone;");
  numSource$totSet[is.na(numSource$totSet)]<-0;
  numSource$pSrv[is.na(numSource$pSrv)]<-0;
  
  #***********************************************************
  #***********************************************************
  totals<-sqldf::sqldf("select
                    sum(totRel) as totRel,
                    sum(totSet) as totSet,
                    sum(totSet)/sum(totRel) as pSrv
                  from
                    numSource;");
  
  #***********************************************************
  #***********************************************************
  prbSinkGivenSource<-sqldf::sqldf("select
                                u.start_depthzone      as start_depthzone,
                                u.start_alongshorezone as start_alongshorezone,
                                u.end_depthzone        as end_depthzone,
                                u.end_alongshorezone   as end_alongshorezone,
                                s.prSetBySrc           as prSetBySrc
                            from
                                (select 
                                    sz.depthzone      as start_depthzone,
                                    sz.alongshorezone as start_alongshorezone,
                                    nz.depthzone      as end_depthzone,
                                    nz.alongshorezone as end_alongshorezone
                                 from uniqNurseryZones as nz, uniqSpawningZones as sz) as u left join
                                (select
                                    ss.start_depthzone,
                                    ss.start_alongshorezone,
                                    ss.end_depthzone,
                                    ss.end_alongshorezone,
                                    ss.totFin/src.totRel as prSetBySrc
                                from
                                    numSourceSink2 as ss,
                                    numSource as src
                                where
                                    ss.start_depthzone      = src.start_depthzone AND
                                    ss.start_alongshorezone = src.start_alongshorezone) as s
                            on
                                s.start_depthzone      = u.start_depthzone AND
                                s.start_alongshorezone = u.start_alongshorezone AND
                                s.end_depthzone        = u.end_depthzone AND
                                s.end_alongshorezone   = u.end_alongshorezone
                            order by
                                start_depthzone,start_alongshorezone,
                                end_depthzone,end_alongshorezone;");
                            
  prbSinkGivenSource$prSetBySrc[is.na(prbSinkGivenSource$prSetBySrc)]<-0;
  prbSinkGivenSource$prSetBySrc<-as.numeric(prbSinkGivenSource$prSetBySrc);#not sure why this is non-numeric
  
  if (writeCSVs){
    write.csv(prbSinkGivenSource,file=outConn);
    write.csv(tblIndivConn,      file=outIndivs);
  }
  
  return(list(prbSinkGivenSource=prbSinkGivenSource,numSource=numSource,tblIndivConn=tblIndivConn));
}

# #An example of how to run the program
# connResFile<-"C:\\Projects\\GOA_IERP\\IBM_Runs\\ATF\\2011\\03-15\\ConnectivityAnalysis\\Results01Conn.csv";
# cellsFile<-"C:\\Projects\\GOA_IERP\\IBM_Runs\\ATF\\2011\\03-15\\ConnectivityAnalysis\\ATF_ClassifiedCGOAGridCells.csv";
# spawningZones<-c("SpawningArea_300to600m");
# nurseryZones<-c("NurseryArea_000to050m","NurseryArea_050to150m");
# lifeStages<-c("egg01","small.yolk.sac.larva","large.yolk.sac.larva",
#               "small.feeding.preflexion.larva","large.feeding.preflexion.larva",
#               "postflexion.larva","settlement.stage.larva","benthic.juvenile");
# 
# res<-calcConnectivityMatrix(connResFile=connResFile,                                   # filename for connectivity results csv file
#                             cellsFile=cellsFile,                                       # filename for classified grid cells csv file
#                             lifeStages=lifeStages,                                     # vector of life stage names
#                             spawningZones=spawningZones,                               #spawning area name(s)
#                             nurseryZones=nurseryZones,                                 #nursery area name(s)
#                             writeCSVs=TRUE,                                            #flag to write csv files
#                             outConn="ConnectivityToNurseryAreasFromSpawningAreas.csv", #output connectivity csv file
#                             outIndivs="IndivStartEndPositions.csv");                   #output csv file w/ individuals' start/end locations

  