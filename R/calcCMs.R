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
#' @param lhsTypeInfo - list object with life stage info
#' @param writeCSVs - flag (T/F) to write concatenated dataframes to csv
#' @param folder   - folder to write csv files to
#' @param basename - base name for output csv files
#' 
#' @details If the 'cellsTbl' is a filename,it will be read to create an associated dataframe. If cellsTbl is null, the user can select
#' the classified grid cells file using a file dialog.\cr\cr
#' 
#' Connectivity results files from the DisMELS model runs should be of the form "ConnYYYYMMDD.csv",
#' where "Conn" is arbitrary (but consistent among results files) and YYYY indicates the year (if applicable), MM the month (if applicable),
#' and DD the day (if applicable) for a particular model run. If YYYY, MM, or DD is **not** part of the file name, use "" as the 
#' function input for the corresponding years, months, or days.
#' 
#' @return list w/ 3 elements:
#' \itemize{
#' \item dfrCMs - dataframe with connectivity matrices results
#' \item dfrICs - dataframe with individual connectivity results
#' \item dfrNRs - dataframe with numbers released
#'}
#' 
#' @importFrom wtsUtilities getCSV
#' 
#' @export
#'
#source("calcConnectivityMatrix.R",chdir=TRUE);
################################################################################
# Calculate connectivity matrices for a set of IBM runs
################################################################################
calcCMs<-function(resDir="C:\\Projects\\GOA_IERP\\IBM_Runs\\ATF\\FullSeries",
                   connResBase="ConnYYYYMMDD.csv",
                   cellsTbl=file.path(resDir,'ATF_ClassifiedCGOAGridCells.csv'),
                   years=as.character(1996:2011),
                   months=c("01"),
                   days=c("01"),
                   spawningZones=c("SpawningArea_300to600m"),
                   nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"),
                   lhsTypeInfo=getLifeStageInfo.ATF(),
                   writeCSVs=TRUE,
                   folder=getwd(),
                   basename=""){
                                   
  #read in classified cells table
  if (!is.data.frame(cellsTbl)){
      if (is.null(cellsTbl)) {
        cellsTbl = getCSV(caption="Select classified grid cells (csv) file");
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
          
          res<-calcCM(ibmResTbl=ibmResTbl,                # table for connectivity results
                      cellsTbl=cellsTbl,                  # table for classified grid cells
                      lhsTypeInfo=lhsTypeInfo,            # list object with life stage info
                      spawningZones=spawningZones,        # spawning area name(s)
                      nurseryZones=nurseryZones,          # nursery area name(s)
                      writeCSVs=FALSE);                   # flag to write results to csv files
          allRes[[paste(year,month,day,sep='.')]]<-res;
          mx<-max(mx,res$dfrCM$prFin,na.rm=TRUE);
        }
      }
    }
  }
  
  dfrCMs<-concatAnnualTables(allRes,type="dfrCM",writeCSV=writeCSVs,folder=folder,basename=paste(basename,'ConnMats',sep=''));
  dfrICs<-concatAnnualTables(allRes,type="dfrIC",writeCSV=writeCSVs,folder=folder,basename=paste(basename,'IndivConns',sep=''));
  dfrNRs<-concatAnnualTables(allRes,type="dfrNR",writeCSV=writeCSVs,folder=folder,basename=paste(basename,'NumReleased',sep=''));
  return (list(dfrCMs=dfrCMs,dfrICs=dfrICs,dfrNRs=dfrNRs));
}
