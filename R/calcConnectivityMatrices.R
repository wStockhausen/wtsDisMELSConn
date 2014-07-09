#'
#'@title Calculate connectivity matrices for a set of DisMELS model runs.
#'
#'@description Function to calculate a set of connectivity matrices corresponding to
#'a set of DisMELS model runs.
#'
#' @param resDir - path to base directory for DisMELS results files
#' @param connResBase - file name template for connectivity results files from DisMELS (e.g., "ConnYYYYMMDD.csv") 
#' @param cellsTbl - name of classified grid cells file or dataframe based on classified grid cells file (can be NULL)
#' @param years - vector of years corresponding to model simulation names (YYYY)
#' @param months - vector of months corresponding to model simulation names (MM)
#' @param days - vector of days corresponding to model simulation names (DD)
#' @param spawningZones - vector of names of zones used as spawning areas in the IBM
#' @param nurseryZones - vector of names of zones used as nursery areas in the IBM
#' @param lifeStages - vector of names of life stages in the IBM
#' 
#' @details If the 'cellsTbl' is a filename,it will be read to create an associated dataframe. If cellsTbl is null, the user can select
#' the classified grid cells file using a file dialog.\cr\cr
#' 
#' Connectivity results files from the DisMELS model runs should be of the form "ConnYYYYMMDD.csv",
#' where "Conn" is arbitrary (but consistent among results files) and YYYY indicates the year (if applicable), MM the month (if applicable),
#' and DD the day (if applicable) for a particular model run. If YYYY, MM, or DD is **not** part of the file name, use "" as the 
#' function input for the corresponding years, months, or days.
#' 
#' @return list w/ 2 elements:\cr
#' allRes - list of connectivity matrix results\cr
#' zmax   - max connectivity value (useful for scaling plots)
#' 
#' @export
#' 
#' @importFrom wtsUtilities getCSV
#'
#source("calcConnectivityMatrix.R",chdir=TRUE);
################################################################################
# Calculate connectivity matrices for a set of IBM runs
################################################################################
calcConnectivityMatrices<-function(resDir="C:\\Projects\\GOA_IERP\\IBM_Runs\\ATF\\FullSeries",
                                   connResBase="ConnYYYYMMDD.csv",
                                   cellsTbl=file.path(resDir,'ATF_ClassifiedCGOAGridCells.csv'),
                                   years=as.character(1996:2011),
                                   months=c("01"),
                                   days=c("01"),
                                   spawningZones=c("SpawningArea_300to600m"),
                                   nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"),
                                   lifeStages=c("egg01","small.yolk.sac.larva","large.yolk.sac.larva",
                                                 "small.feeding.preflexion.larva","large.feeding.preflexion.larva",
                                                 "postflexion.larva","settlement.stage.larva","benthic.juvenile")){
                                   
  #read in classified cells table
  if (!is.data.frame(cellsTbl)){
      if (is.null(cellsTbl)) {
        cellsTbl = wtsUtilities::getCSV(caption="Select classified grid cells (csv) file");
        if (is.null(cellsTbl)) return(NULL);
      } else {
        cellsTbl<-read.csv(cellsTbl,stringsAsFactors=FALSE);
      }
  }
  
  #process all files
  mx<-0;
  allRes<-list();
  for (year in years){
    yrBase<-gsub('YYYY',year,connResBase);
    for (month in months){
      mnBase<-gsub('MM',month,yrBase);
      for (day in days){ 
        cat('Processing',year,month,day,'\n',sep=' ')
        dyBase<-gsub('DD',day,mnBase);
        connResFile<-file.path(resDir,dyBase);
        if (file.exists(connResFile)){
          ibmResTbl<-read.csv(connResFile,stringsAsFactors=FALSE);
          
          res<-calcConnectivityMatrix(ibmResTbl=ibmResTbl,                # table for connectivity results
                                      cellsTbl=cellsTbl,                  # table for classified grid cells
                                      lifeStages=lifeStages,              # vector of life stage names
                                      spawningZones=spawningZones,        # spawning area name(s)
                                      nurseryZones=nurseryZones,          # nursery area name(s)
                                      writeCSVs=FALSE);                   # flag to write results to csv files
          allRes[[paste(year,month,day,sep='.')]]<-res;
          mx<-max(mx,res$prbSinkGivenSource$prSetBySrc,na.rm=TRUE);
        }
      }
    }
  }
  return (list(allRes=allRes,zmax=mx));
}
