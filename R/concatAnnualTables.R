#'
#'@title Concatenate annual tables (connectivity matrices, individual connectivities, numbers by source), write to csv files.
#'
#'@title Function to concatenate annual tables (connectivity matrices, individual connectivities, numbers by source), write to csv files.
#'
#'@param allRes - the allRes list from running calcConnectivityMatrices(...)
#'@param type - type of table to concatenate
#'@param writeCSV - flag (T/F) to write concatenated table to csv
#'@param folder - folder to write csv file
#'@param basename - base name for output csv file
#'
#'@return dataframe with concatenated matrices
#'
#'@export
#'
concatAnnualTables<-function(allRes=NULL,
                           type=c("prbSinkGivenSource","tblIndivConn","numSource"),
                           writeCSV=TRUE,
                           folder=getwd(),
                           basename=c('ConnMats','IndivConns','numsBySource')){
    nms<-names(allRes);
    #create filename
    outCSV<-file.path(folder,paste(basename,'.csv',sep=''));

    dfrp<-NULL;
    for (nm in nms){
        #create output filename
        nmp<-strsplit(nm,'.',fixed=TRUE);
        nmp<-paste(nmp[[1]],collapse='');
        res<-allRes[[nm]];
        dfr<-res[[type]];
        dt<-list(date=rep(nmp,length.out=nrow(dfr)));
        dfr<-cbind(dt,dfr);
        dfrp<-rbind(dfrp,dfr);
    }
    write.csv(dfrp,file=outCSV,row.names=FALSE);
    return(invisible(dfrp));
}

#dfrConMats  <-concatAnnualTables(res.1996to2011$allRes,type="prbSinkGivenSource",basename='ATF.ConnMats');
#dfrIndivConn<-concatAnnualTables(res.1996to2011$allRes,type="tblIndivConn",basename='ATF.IndivConns');
#dfrNumBySrc <-concatAnnualTables(res.1996to2011$allRes,type="numSource",   basename='ATF.NumBySource');
