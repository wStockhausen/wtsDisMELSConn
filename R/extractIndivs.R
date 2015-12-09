#'
#'@title Extract tracks of individuals from a DisMELS model run.
#'
#'@description Function to extract tracks of individuals from a DisMELS model run.
#'
#'@param indivConn - individual connectivity file (or dataframe)
#'@param results - DisMELS results file (or dataframe)
#'@param lhsTypeInfo - life stage info list for IBM
#'@param lhsTypes - vector of lhs types to process (NULL = all)
#'@param onlySuccessful   - flag (T/F) to extract only successful indivs (if TRUE)
#'@param onlyUnsuccessful - flag (T/F) to extract only unsuccessful indivs (if TRUE)
#'@param nurseryZones - vector of names of zones used as nursery areas in the IBM
#'@param returnList - flag (T/F) indicating whether a list should be returned (otherwise NULL--helps with memory)
#'@param writeOutput - flag (T/F) indicating whether output files should be written
#'@param outDir - folder for output
#'@param outBaseCSV - base name for output csv files
#'
#'@return list of dataframes with indiv results by lhs type name if returnList=TRUE, otherwise NULL.
#'
#'@details If indivConn or results are NULL, the user will be prompted to select the 
#'corresponding file(s) using a file chooser dialog.
#'
#'@importFrom sqldf sqldf
#'@importFrom wtsUtilities getCSV
#'
#'@export
#'
extractIndivs<-function(indivConn=NULL,    #individual connectivity results file (or dataframe)
                          results=NULL,    #DisMELS results file (or dataframe)
                          lhsTypeInfo=getLifeStageInfo.ATF(),    
                          lhsTypes=NULL,    
                          onlySuccessful=FALSE,
                          onlyUnsuccessful=FALSE,
                          nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), #nursery area name(s)
                          returnList=TRUE,
                          writeOutput=TRUE,
                          outDir='./',
                          outBaseCSV="allIndivs"
                          ){

    retIndivConn<-FALSE;
    if (!is.data.frame(indivConn)){
        #read in individual connectivity results from csv file
        if (is.null(indivConn)){
            indivConn<-wtsUtilities::getCSV(caption='Select individual connectivity results file');
            if (is.null(indivConn)) return(NULL);#user aborted
        } else {
            indivConn<-read.csv(indivConn,stringsAsFactors=FALSE);
        }
        retIndivConn<-TRUE;
    }
    
    retRes<-FALSE;
    if (!is.data.frame(results)){
        #read in full results of DisMELS model run from csv file
        if (is.null(results)){
            results<-wtsUtilities::getCSV(caption='Select full DisMELS results file');
            if (is.null(results)) {
                if (retIndivConn) return(indivConn);
                return(NULL);#user aborted
            }
        } else {
            results<-read.csv(results,stringsAsFactors=FALSE);
        }
        retRes<-TRUE;
    }

    #define life stage types
    typeNames=names(lhsTypeInfo$lifeStageTypes);
    if (is.null(lhsTypes)) lhsTypes<-typeNames;#process all typeNames
    
    #define variables
    conVars <- c('start_typeName','start_depthzone','start_alongshorezone',
                  'end_typeName','end_depthzone','end_alongshorezone');
    stdVars <-getStdVars(lhsTypeInfo$resType);
    stdVarsOut<-c('typeName','id','time',
                  'horizPos1','horizPos2','vertPos','track',
                  'alive','age','ageInStage','number');
    
    #pull out IDs for indviduals from indivConn
    indivIDs<-extractIndivIDs(indivConn=indivConn,
                              lhsTypeInfo=lhsTypeInfo,
                              onlySuccessful=onlySuccessful,
                              onlyUnsuccessful=onlyUnsuccessful,
                              nurseryZones=nurseryZones);
    cat('Will extract results for ',nrow(indivIDs),' individuals\n',sep='')
    qry<-"select distinct ID from indivIDs order by ID;"
    uids<-sqldf::sqldf(qry);
    cat("Number of unique ids in indivIDs =",nrow(uids),'\n');
    print(uids$ID[1:10]);
    
    #loop over type names and extract results for indivs
    if (returnList) indivsLst<-list();
    for (ctr in 1:length(typeNames)){
        typeName<-typeNames[ctr];
        if (typeName %in% lhsTypes){
            
            #extract indivs from results
            resVars<-paste('r',names(results),sep='.',collapse=',');
            qry<-"select
                    i.start_typeName,
                    i.start_depthzone,
                    i.start_alongshorezone,
                    i.end_typeName,
                    i.end_depthzone,
                    i.end_alongshorezone,
                    &&resVars
                  from
                    results r,
                    indivIDs i
                  where
                    r.id=i.ID and 
                    r.typeName='&&typeName'
                  order by
                    r.id,r.age;";
            qry<-gsub("&&resVars",resVars,qry); 
            qry<-gsub("&&typeName",typeName,qry); 
            cat("query = ",qry,sep='\n');
            indivsTmp<-sqldf::sqldf(qry);    
            
            #check on indivs
            qry<-"select distinct id from indivsTmp order by id;"
            uids<-sqldf::sqldf(qry);
            cat("Number of unique ids in indivsTmp =",nrow(uids),'\n');
            print(uids$id[1:10]);
            cat('names(indivsTmp)= [',paste(names(indivsTmp),collapse=','),']\n')

            #assign correct names to columns for life stage
            lhsVars<-lhsTypeInfo$lifeStageTypes[[typeName]]$info$vars;
            allVars<-c(conVars,stdVars$vars,lhsVars);
            names(indivsTmp)<-paste('tmp',1:ncol(indivsTmp),sep='');#assign temporary names
            nms<-names(indivsTmp);
            nms[1:length(allVars)]<-allVars; #substitute allVars
            names(indivsTmp)<-nms;           #assign names to extracted columns
            
            #determine output column names for life stage
            varsOutStr<-paste(allVars,sep='',collapse=',')
    
            qry<-"select
                    &&varsOut
                  from
                    indivsTmp
                  order by
                    id,age;";
            qry<-gsub("&&varsOut",varsOutStr,qry);  
            cat("query = ",qry,sep='\n');
            indivsType<-sqldf::sqldf(qry);
            
            #write indivs to csv file
            if (writeOutput) write.csv(indivsType,file=file.path(outDir,paste(outBaseCSV,ctr,typeName,'csv',sep='.')),row.names=FALSE);
            if (returnList) indivsLst[[typeName]]<-indivsType;
        }#if typeName in lhsTypes
    }#loop over typeNames
    
#     if (retRes&&retIndivConn) return(invisible(list(indivConn=indivConn,results=results)));
#     if (retRes)               return (invisible(results));
#     if (retIndivConn)         return(invisible(indivConn));
    if (returnList) return(invisible(indivsLst));
    return(NULL);
}

#icDir<-'~/Programming/R/GitPackages/wtsDisMELSConn'
#indivConn<-file.path(icDir,'IndivConn.1997.csv')
#resDir<-'/Volumes/Iomega HDD/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/CSVs.ModelResults'
#results<-file.path(resDir,'Results1997.csv')
#sIndivs<-extractIndivs(indivConn,results,onlySuccessful  =TRUE,lhsTypes='egg01',outBaseCSV='ExtractedIndivs.Successful',  lhsTypeInfo=getLifeStageInfo.ATF(),returnList=FALSE);
#uIndivs<-extractIndivs(indivConn,results,onlyUnsuccessful=TRUE,lhsTypes='egg01',outBaseCSV='ExtractedIndivs.Unsuccessful',lhsTypeInfo=getLifeStageInfo.ATF(),returnList=FALSE);
