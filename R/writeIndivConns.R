#'
#'@title Write dataframe with individual connectivity results to files by date.
#'
#'@description Funcrion to write dataframe with individual connectivity results to files by date.
#'
#'@param dfrICs - dataframe
#'@param folder - folder to write to
#'@param basename - base name for csv files
#'
#'@export
#'
writeIndivConns<-function(dfrICs,
                          folder=getwd(),
                          basename='IndivConn'){
    if (!dir.exists(folder)) dir.create(folder,recursive=TRUE);
    dates<-unique(dfrICs$date);
    for (date in dates){
      dfrIC<-dfrICs[dfrICs$date==date,];
      fn<-file.path(folder,paste(basename,date,'.csv',sep=''));
      write.csv(dfrIC,file=fn,row.names=FALSE);
    }
}