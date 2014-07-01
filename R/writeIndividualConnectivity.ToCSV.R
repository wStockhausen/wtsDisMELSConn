#'
#'@title Write individual connectivity results to csv files.
#'
#'@param allRes - the allRes list from running calcConnectivityMatrices(...)
#'@param concat - flag (T/F) to concatenate individual connectivity results into 1 output csv file
#'@param folder - folder to write csv file(s) to
#'@param basename - base name for output csv files
#'
#'@export
#'
writeIndividualConnectivity.ToCSV<-function(allRes=NULL,
                                            concat=FALSE,
                                            folder=getwd(),
                                            basename='IndivConn'){
    nms<-names(allRes);
    if (concat){
        #create filename
        outCSV<-file.path(folder,paste(basename,'.csv',sep=''));
        #create csv file, if it doesn't exist, and truncate it if it does exist
        file.create(outCSV);
    }
    
    col.names<-TRUE;#write col.names 1st time
    for (nm in nms){
        #create output filename
        nmp<-strsplit(nm,'.',fixed=TRUE);
        nmp<-paste(nmp[[1]],collapse='');
        dfr<-allRes[[nm]]$tblIndivConn;
        if (concat){
            dt<-list(date=rep(nmp,length.out=nrow(dfr)));
            dfr<-cbind(dt,dfr);
            write.table(dfr,file=outCSV,append=TRUE,row.names=FALSE,col.names=col.names,sep=',');
            col.names<-FALSE;#write col.names 1st time only
        } else {
            #create output filename based on nmp
            outCSV<-file.path(folder,paste(basename,nmp,'.csv',sep=''));
            #write indiv conn info to output csv file
            write.csv(dfr,file=outCSV,row.names=FALSE);
        }
    }
}

#writeIndividualConnectivity.ToCSV(res.1996to2011$allRes,basename='ATF.IndivConn',concat=FALSE);
#writeIndividualConnectivity.ToCSV(res.1996to2011$allRes,basename='POP.IndivConn',concat=FALSE);
