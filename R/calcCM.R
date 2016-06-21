#'
#'@title Calculate a connectivity matrix for one DisMELS model run
#'
#'@description Function to calculate a connectivity matrix based on one DisMELS model run.
#'
#' @param ibmResTbl - name of DisMELS connectivity results file or resulting dataframe (can be NULL)
#' @param cellsTbl - name of classified grid cells file or resulting dataframe (can be NULL)
#' @param lhsTypeInfo - list object with life stage info
#' @param spawningZones - vector of names of zones used as spawning areas in the IBM
#' @param nurseryZones - vector of names of zones used as nursery areas in the IBM
#' @param where_clause - sql "where" clause to determine final state for individuals included in connectivity calculations
#' @param writeCSVs - flag (T/F) to write output files 
#' @param outNumRel - csv filename for initial numbers released from spawning areas
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
#' Note: this function calculates successful individuals using the dfrIC dataframe based on their final location
#' being in on of the defined nursery zones, not on reaching the final life stage.
#' 
#' Uses \code{wtsUtilities::getCSV} and \code{sqldf::sqldf}.
#' 
#' @return list w/ 2 elements:\cr
#' dfrCM - dataframe with probability of transport to a sink (nursery area) conditioned on starting in a source (spawning area)\cr
#' dfrNR - numbers by source (spawning area)\cr
#' dfrIC - dataframe of start and end locations for each individual in the model run\cr
#' 
#' @export
#'
#*********************************************************
#  Calculate connectivity matrix for one model run
#**********************************************************/
calcCM<-function(ibmResTbl=NULL,
                 cellsTbl=NULL, 
                 lhsTypeInfo=getLifeStageInfo.ATF(),            
                 spawningZones=c("SpawningArea_300to600m"),                       #spawning area name(s)
                 nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), #nursery area name(s)
                 where_clause="",                                           #sql clause to select end state
                 writeCSVs=FALSE,
                 outNumRel="NumbersReleasedFromSpawningAreas.csv",          #initial numbers released csv file 
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
  
  #calculate individual connectivity matrix
  dfrIC<-calcIC(ibmResTbl,
                cellsTbl,
                lhsTypeInfo=lhsTypeInfo,
                spawningZones=spawningZones,
                writeCSV=writeCSVs,
                outIndivs=outIndivs);
  
  #pull out life stage info
  lifeStages<-names(lhsTypeInfo$lifeStageTypes)
  firstLHS<-lifeStages[1];                 # name of first life stage
  lastLHS<-lifeStages[length(lifeStages)]; # name of final (recruiting) life stage
  cat("firstLHS = ",firstLHS,"\n")
  cat("lastLHS  = ",lastLHS,"\n")
  
  #turn these into dataframes for use in sql statements
  lifeStages   <-as.data.frame(list(stage=lifeStages,rank=1:length(lifeStages)));
  nurseryZones <-as.data.frame(list(zone=nurseryZones));
  
  #create unique spawning zones table
  uniqSpawningZones<-sqldf::sqldf("select distinct
                              start_depthzone as depthzone,
                              start_alongshorezone as alongshorezone
                            from 
                              dfrIC
                            order by
                              depthzone, alongshorezone;");
  
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

  #****************************************************************************
  #                    doAnalysis                                             *
  # Purpose                                                                   *
  #   Analyze the individual connectivity data and construct various          *
  #   source-sink summary tables.                                             *
  #***************************************************************************/
  ##total number released, by start zone 
  qry<-"select
           start_depthzone,
           start_alongshorezone,
           sum(numStart) as numRel
         from
           dfrIC
         group by
           start_alongshorezone,
           start_depthzone
        order by
          start_depthzone,
          start_alongshorezone;";
  numRel<-sqldf::sqldf(qry);
  
  ##number "successful", by start/end zones, start/end typeNames
  qry<-"select
           start_depthzone,
           start_alongshorezone,
           end_depthzone,
           end_alongshorezone,
           start_typeName,
           end_typeName,
           sum(numFinish) as numFin
         from
           dfrIC
         &&where_clause
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
          end_typeName;";
  qry<-gsub("&&where_clause",where_clause,qry,fixed=TRUE);
  numSS1<-sqldf::sqldf(qry);
  
  ##number "successful", by start/end zones (integrated over typeNames)
  qry<-"select
           start_depthzone,
           start_alongshorezone,
           end_depthzone,
           end_alongshorezone,
           sum(numFin) as numFin
         from
           numSS1
         group by
           start_alongshorezone,
           start_depthzone,
           end_alongshorezone,
           end_depthzone                          
        order by
          start_depthzone,
          start_alongshorezone,
          end_depthzone,
          end_alongshorezone;";
  numSS2<-sqldf::sqldf(qry);
  
  ##calculate non-zero connectivity
  qry<-"select
          e.start_depthzone      as start_depthzone,
          e.start_alongshorezone as start_alongshorezone,
          e.end_depthzone        as end_depthzone,
          e.end_alongshorezone   as end_alongshorezone,
          e.numFin               as numFin,
          e.numFin/r.numRel      as prbCon
        from
          numRel r, numSS2 e
        where
          r.start_depthzone       = e.start_depthzone      AND
          r.start_alongshorezone  = e.start_alongshorezone AND
          e.end_depthzone in nurseryZones
        order by
          start_depthzone,
          start_alongshorezone,
          end_depthzone,
          end_alongshorezone;";
  numSS3<-sqldf::sqldf(qry);


  ##expand to unique spzawning zones x nursery zones
  qry<-"select
          u.start_depthzone      as start_depthzone,
          u.start_alongshorezone as start_alongshorezone,
          u.end_depthzone        as end_depthzone,
          u.end_alongshorezone   as end_alongshorezone,
          s.numFin               as numFin,
          s.prbCon               as prbCon
        from
          (select 
              sz.depthzone      as start_depthzone,
              sz.alongshorezone as start_alongshorezone,
              nz.depthzone      as end_depthzone,
              nz.alongshorezone as end_alongshorezone
           from uniqNurseryZones as nz, uniqSpawningZones as sz) as u left join
          numSS3 as s
           on
            s.start_depthzone      = u.start_depthzone AND
            s.start_alongshorezone = u.start_alongshorezone AND
            s.end_depthzone        = u.end_depthzone AND
            s.end_alongshorezone   = u.end_alongshorezone
          order by
            start_depthzone,start_alongshorezone,
            end_depthzone,end_alongshorezone;";
  prbSS1<-sqldf::sqldf(qry);  
  prbSS1$numFin[is.na(prbSS1$numFin)]<-0;
  prbSS1$prbCon[is.na(prbSS1$prbCon)]<-0;

  
  ##include number released by starting location
  qry<-"select
          r.start_depthzone      as start_depthzone,
          r.start_alongshorezone as start_alongshorezone,
          p.end_depthzone        as end_depthzone,
          p.end_alongshorezone   as end_alongshorezone,
          r.numRel               as numRel,
          p.numFin               as numFin,
          p.prbCon               as prbCon
        from
          numRel as r left join
          prbSS1 as p
        where
          r.start_depthzone = p.start_depthzone and
          r.start_alongshorezone = p.start_alongshorezone
        order by
          start_depthzone,start_alongshorezone,
          end_depthzone,end_alongshorezone;";
  dfrCon<-sqldf::sqldf(qry);  

  if (writeCSVs){
    write.csv(numRel,file=outNumRel);
    write.csv(dfrCon,file=outConn);
  }
  
  return(list(dfrCM=dfrCon,dfrNR=numRel,dfrIC=dfrIC));
}

  