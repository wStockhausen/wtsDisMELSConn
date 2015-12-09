#'
#'@title Extracts and re-orders a series of DisMELS model results files by individual.
#'
#'@description Function to extract and re-order a series of DisMELS model results by individual.
#'
#'@param YYYYs - years to extract (can be NULL)
#'@param MMs - months to extract (can be NULL)
#'@param DDs - days to extract (can be NULL)
#'@param lhsTypeInfo - life stage info list for IBM
#'@param lhsTypes - lhs types to keep for output (NULL = all)
#'@param nurseryZones - vector of nursery area names
#'@param onlySuccessful   - flag (T/F) to extract only successful indivs (if TRUE)
#'@param onlyUnsuccessful - flag (T/F) to extract only unsuccessful indivs (if TRUE)
#'@param inpDir.IndivConn - folder with input individual connectivity matrix files
#'@param basename.IndivConn - base name for individual connectivity matrix files
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
extractIndivs.MultipleRuns<-function(YYYYs=NULL,
                                       MMs=NULL,
                                       DDs=NULL,
                                       lhsTypeInfo=NULL,    
                                       lhsTypes=NULL,
                                       nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), #nursery area name(s)
                                       onlySuccessful=TRUE,
                                       onlyUnsuccessful=FALSE,
                                       inpDir.IndivConn=NULL,
                                       basename.IndivConn="IndivConn",
                                       inpDir.Results=NULL,
                                       basename.Results="Results",
                                       outDir=NULL,
                                       basename.Out="SuccessfulIndivs"){
    if (is.null(inpDir.IndivConn)){
        inpDir.IndivConn<-tcltk::tk_choose.dir(caption='Select input directory for csv files with Individual Connectivity matrices.')
    }
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
                indivConn<-file.path(inpDir.IndivConn,paste(basename.IndivConn,id,'.csv',sep=''))
                if (file.exists(indivConn)){
                    results<-file.path(inpDir.Results,paste(basename.Results,id,'.csv',sep=''))
                    if (file.exists(results)){
                        outBaseCSV<-paste(basename.Out,id,sep='');
                        extractIndivs(indivConn=indivConn,
                                      results=results,
                                      lhsTypeInfo=lhsTypeInfo,
                                      lhsTypes=lhsTypes,
                                      onlySuccessful=onlySuccessful,
                                      onlyUnsuccessful=onlyUnsuccessful,
                                      nurseryZones=nurseryZones,
                                      returnList=FALSE,
                                      writeOutput=TRUE,
                                      outDir=outDir,
                                      outBaseCSV=outBaseCSV);
                        gc(verbose=TRUE);#force garbage collection
                    } else {
                        cat("could not process '",results,"'. File does not exist.\n",sep='')
                    }
                } else {
                    cat("could not process '",indivConn,"'. File does not exist.\n",sep='')
                }
            }
        }
    }
}

# extractIndivs.MultipleRuns(YYYYs=1996:2011,
#                                      newResType=TRUE,
#                                      lhsTypeInfo=getLifeStageInfo.ATF(),
#                                      inpDir.IndivConn='G:/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/ConnectivityResults/IndivConns',
#                                      basename.IndivConn='ATF.IndivConn',
#                                      inpDir.Results='G:/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/CSVs.ModelResults',
#                                      basename.Results='Results',
#                                      outDir='G:/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/CSVs.SuccessfulIndivs');
# extractIndivs.MultipleRuns(YYYYs=1996:2011,
#                                      newResType=FALSE,
#                                      lhsTypeInfo=getLifeStageInfo.POP(),
#                                      inpDir.IndivConn='G:/cDrive.GOA_IERP/IBM_Runs/POP/FullSeriesAprMayJun/ConnectivityAnalysis/IndivConns',
#                                      basename.IndivConn='POP.IndivConn',
#                                      inpDir.Results='G:/cDrive.GOA_IERP/IBM_Runs/POP/FullSeriesAprMayJun/CSVs.ModelResults',
#                                      basename.Results='Results',
#                                      outDir='G:/cDrive.GOA_IERP/IBM_Runs/POP/FullSeriesAprMayJun/CSVs.SuccessfulIndivs');
