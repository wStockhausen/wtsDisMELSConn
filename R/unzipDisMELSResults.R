#'
#'@title Unzips and renames a series of DisMELS results or connectivity files.
#'
#'@description Function to unzip and rename a series of DisMELS results or connectivity files.
#'
#'@param dir.inp = folder containing the zip files
#'@param dir.out = folder where unzipped files will be written
#'@param YYYY = years to extract (can be NULL)
#'@param MM = months to extract (can be NULL)
#'@param DD = days to extract (can be NULL)
#'@param basename =  base name for zipped/unzipped files
#' 
#'@details
#'This function assumes zip files are named something like 'basenameYYYYMMDD.csv.zip' and
#'that the associated zipped csv file is 'basename.csv'. When the csv file is unzipped
#'in the folder specified by dir.out, it will be renamed 'basenameYYYYMMDD.csv'. Note
#'that at least one of YYYY, MM, and DD must be a non-null vector.
#'
#'@export
#'
unzipDisMELSResults<-function(dir.inp=getwd(),
                       dir.out=getwd(),
                       YYYY=NULL,
                       MM=NULL,
                       DD=NULL,
                       basename='Results'){
    
    ids<-paste(YYYY,MM,DD,sep='');
    cat("IDs = ",ids,sep='\n');
    
    for (id in ids){
        zipf<-paste(basename,id,sep='');
        zipf<-file.path(dir.inp,paste(zipf,"csv","zip",sep='.'));
        cat("zip file     : ",zipf,"\n",sep='');
        unzip(zipf,exdir=dir.out)
        csvi<-file.path(dir.out,paste(basename,'.csv',sep=''))
        csvf<-file.path(dir.out,paste(basename,id,'.csv',sep=''))
        file.rename(csvi,csvf);
        cat("unzipped file: ",csvf,"\n",sep='');
    }
}

#unzipDisMELSResults(dir.out='../CSVs',YYYY=2001:2011)
