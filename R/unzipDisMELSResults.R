#'
#'@title Unzips and renames a series of DisMELS results or connectivity files.
#'
#'@description Function to unzip and rename a series of DisMELS results or connectivity files.
#'
#'@param YYYYs = years to extract (can be NULL)
#'@param MMs = months to extract (can be NULL)
#'@param DDs = days to extract (can be NULL)
#'@param basename =  base name for zipped/unzipped files
#'@param dir.inp = folder containing the zip files
#'@param dir.out = folder where unzipped files will be written
#' 
#'@details
#'This function assumes zip files are named something like 'basenameYYYYMMDD.csv.zip' and
#'that the associated zipped csv file is 'basename.csv'. When the csv file is unzipped
#'in the folder specified by dir.out, it will be renamed 'basenameYYYYMMDD.csv'. Note
#'that at least one of YYYYs, MMs, and DDs must be a non-null vector.
#'
#'@export
#'
unzipDisMELSResults<-function(YYYYs=NULL,
                              MMs=NULL,
                              DDs=NULL,
                              basename='Results',
                              dir.inp=getwd(),
                              dir.out=getwd()){
    
    if (is.null(YYYYs)) {
        YYYYs<-'';
    } else {
        YYYYs<-formatC(YYYYs,width=4,mode="integer")
    }
    if (is.null(MMs)) {
        MMs<-'';
    } else {
        MMs<-formatC(MMs,width=2,mode="integer",flag="0")
    }
    if (is.null(DDs)) {
        DDs<-'';
    } else {
        DDs<-formatC(DDs,width=2,mode="integer",flag="0")
    }
    
    for (YYYY in YYYYs) {
        for (MM in MMs) {
            for (DD in DDs) {
                cat("Processing",YYYY,MM,DD,"\n")
                id<-paste(YYYY,MM,DD,sep='');
                zipf<-paste(basename,id,sep='');
                zipf<-file.path(dir.inp,paste(zipf,"csv","zip",sep='.'));
                if (file.exists(zipf)){
                    cat("Unzipping file     : ",zipf,"\n",sep='');
                    unzip(zipf,exdir=dir.out)
                    csvi<-file.path(dir.out,paste(basename,'.csv',sep=''))
                    csvf<-file.path(dir.out,paste(basename,id,'.csv',sep=''))
                    file.rename(csvi,csvf);
                    cat("unzipped file: ",csvf,"\n",sep='');
                } else {
                    cat("Skipping '",zipf,"'.  File does not exist\n",sep='')
                }
            }
        }
    }
}

#unzipDisMELSResults(dir.out='../CSVs',YYYYs=2001:2011)
