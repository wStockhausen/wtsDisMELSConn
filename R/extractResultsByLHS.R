#'
#'@title Extract results from DisMELS model run to individual files by life stage.
#'
#'@description Function to extract results from DisMELS model run to individual files by life stage.
#'
#'@param results - filename for results 
#'@param lhsTypeInfo - list with life stage info for the model that created the results
#'@param lhsTypes - life stage types (names) to process
#'@param returnList - flag to return list of output (does nothing now)
#'@param writeOutput - flag to write output to file
#'@param outDir - folder to write output csv files
#'@param outBaseCSV - base name for output csv files
#'
#'@export
#'
extractResultsByLHS<-function(results,
                              lhsTypeInfo=getLifeStageInfo.ATF(),    
                              lhsTypes=NULL,    
                              returnList=TRUE,
                              writeOutput=TRUE,
                              outDir=dirname(results),
                              outBaseCSV="Results"){
    #get standard variable names/order using the results file type
    stdVars <- getStdVars(lhsTypeInfo$resType);
    
    #define life stage types
    typeNames=names(lhsTypeInfo$lifeStageTypes);
    if (is.null(lhsTypes)) lhsTypes<-typeNames;#process all typeNames
    
    #open connection to results file
    resConn<-file(results,open="rt");
    
    #open connections to files for extracted results
    ns<-list()
    lhsConns<-list();
    ctr<-0 ;
    for (typeName in typeNames){
        ctr<-ctr+1;
        if (typeName %in% lhsTypes){
            outFN<-file.path(outDir,paste(outBaseCSV,ctr,typeName,'csv',sep='.'));
            lhsConns[[typeName]]<-file(outFN,open='wt');
            lhsVars<-lhsTypeInfo$lifeStageTypes[[typeName]]$info$vars
            hdr<-paste(c(stdVars$vars,lhsVars),sep='',collapse=',')
            writeLines(hdr,con=lhsConns[[typeName]],sep='\n');
            ns[[typeName]]<-0;
        }
    }
    
    #read results file and write results to appropriate files
    str <- readLines(resConn,n=1); #read header line and discard
    str <- readLines(resConn,n=1); #read 1st results line
    ctr<-1
    while (length(str)>0){
        res<-strsplit(str,',',fixed=TRUE)
        typeName<-res[[1]][1]
        if (typeName %in% lhsTypes){
            #cat('typeName = ',typeName,'\n')
            writeLines(str,con=lhsConns[[typeName]],sep='\n');
            ns[[typeName]]<-ns[[typeName]]+1;
        }
        str <- readLines(resConn,n=1); #read next line
        ctr<-ctr+1;
        if ((ctr%%10000)==0) cat('processing ',ctr,'\n'); 
        if ((ctr%%100000)==0) {
            cat('Flushing connections\n'); 
            for (lhsConn in lhsConns) flush(lhsConn);
        }
        #if (ctr>220000) break;
    }
    
    close(resConn);
    for (lhsConn in lhsConns) close(lhsConn);
    for (typeName in names(ns)){
        cat(typeName,'had',ns[[typeName]],'rows\n')
    }
    return(list(lhsConns=lhsConns,ns=ns))
}

# resDir<-'G:/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/CSVs.ModelResults'
# resFN <-'Results1997'
# results<-file.path(resDir,paste(resFN,'csv',sep='.'))
# lhsRes<-extractResultsByLHS(results,
#                             lhsTypeInfo=getLifeStageInfo.ATF(),    
#                             lhsTypes=NULL,    
#                             returnList=TRUE,
#                             writeOutput=TRUE,
#                             outDir=resDir,
#                             outBaseCSV=resFN);
# 
