#'
#'@title Extracts and re-exports a series of DisMELS model results files by life stage.
#'
#'@description Function to extract and re-export a series of DisMELS model results by life stage.
#'
#'@param YYYYs - years to extract (can be NULL)
#'@param MMs - months to extract (can be NULL)
#'@param DDs - days to extract (can be NULL)
#'@param lhsTypeInfo - life stage info list for IBM
#'@param lhsTypes - lhs types to keep for output (NULL = all)
#'@param inpDir.Results - folder with input DisMELS model results files
#'@param basename.Results - base name for DisMELS model results files
#'@param outDir - output folder
#'@param basename.Out - base name for output files
#'
#'@return no return value
#'
#'@details
#'This function assumes files are named something like 'basenameYYYYMMDD.csv'. 
#'Note that at least one of YYYYs, MMs, and DDs must be a non-null vector.
#'
#'@importFrom tcltk tk_choose.dir
#'
#'@export
#'
extractResultsByLHS.MultipleRuns<-function(YYYYs=NULL,
                                           MMs=NULL,
                                           DDs=NULL,
                                           lhsTypeInfo=NULL,    
                                           lhsTypes=NULL,
                                           inpDir.Results=NULL,
                                           basename.Results="Results",
                                           outDir=NULL,
                                           basename.Out="Results"){
    if (is.null(inpDir.Results)){
        inpDir.Results<-tcltk::tk_choose.dir(default=inpDir.IndivConn,caption='Select input directory for DisMELS model results (csv) files.')
    }
    if (is.null(outDir)){
        outDir<-tcltk::tk_choose.dir(default=inpDir.Results,caption='Select output directory.')
    }
    
    #format YYYYs, MMs, DDs
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
    
    #loop over values
    for (YYYY in YYYYs) {
        for (MM in MMs) {
            for (DD in DDs) {
                cat("Processing",YYYY,MM,DD,"\n");
                id<-paste(YYYY,MM,DD,sep='');
                results<-file.path(inpDir.Results,paste(basename.Results,id,'.csv',sep=''))
                if (file.exists(results)){
                    outBaseCSV<-paste(basename.Out,id,sep='');
                    extractResultsByLHS(results=results,
                                        lhsTypeInfo=lhsTypeInfo,
                                        lhsTypes=lhsTypes,
                                        returnList=FALSE,
                                        writeOutput=TRUE,
                                        outDir=outDir,
                                        outBaseCSV=outBaseCSV);
                } else {
                    cat("could not process '",results,"'. File does not exist.\n",sep='')
                }
            }#DDs
        }#MMs
    }#YYYYs
}

# extractResultsByLHS.MultipleRuns(YYYYs=1996:2011,
#                                      lhsTypeInfo=getLifeStageInfo.ATF(),
#                                      inpDir.IndivConn='G:/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/ConnectivityResults/IndivConns',
#                                      basename.IndivConn='ATF.IndivConn',
#                                      inpDir.Results='G:/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/CSVs.ModelResults',
#                                      basename.Results='Results',
#                                      outDir='G:/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/CSVs.SuccessfulIndivs');
