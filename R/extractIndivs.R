#'
#'@title Function to extract tracks of individuals from a DisMELS model run.
#'
#'@param indivConn - individual connectivity file (or dataframe)
#'@param results - DisMELS results file (or dataframe)
#'@param lhsTypeInfo - life stage info list for IBM
#'@param lhsTypes - vector of lhs types to process (NULL = all)
#'@param newResType - flag (T/F) indicating whether (TRUE) or not (FALSE) results are from a "new"-type IBM
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
                          lhsTypeInfo=NULL,    
                          lhsTypes=NULL,    
                          newResType=FALSE,
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
    stdVarsAll<-getStdVars(newResType);
    stdVarsOut<-c('typeName','id','parentID','origID',
                  'time','horizPos1','horizPos2','vertPos','track',
                  'alive','age','ageInStage','number');
    
    #pull out IDs for indviduals from indivConn
    indivIDs<-extractIndivIDs(indivConn=indivConn,
                              lhsTypeInfo=lhsTypeInfo,
                              onlySuccessful=onlySuccessful,
                              onlyUnsuccessful=onlyUnsuccessful,
                              nurseryZones=nurseryZones);
    cat('Will Extract results for ',nrow(indivIDs),' individuals\n',sep='')
    
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
            r.id=i.ID
          order by
            r.id;";
    qry<-gsub("&&resVars",resVars,qry); 
    cat("query = ",qry,sep='\n');
    indivs<-sqldf::sqldf(qry);    
    
    #check on indivs
    qry<-"select distinct id from indivs order by id;"
    uids<-sqldf::sqldf(qry);
    cat("Number of unique ids in indivs =",nrow(uids),'\n');
    print(uids$id[1:10]);
    cat('names(indivs)= ',paste(names(indivs,collapse=',')),'\n')
    
    #assign temporary names to columns
    nc<-length(names(indivs));
    names(indivs)<-paste('c',1:nc,sep='');
    names(indivs)[7:8]<-c('typeName','id')
    cat('names(indivs)= ',paste(names(indivs,collapse=',')),'\n')
    
    #loop over type names and extract results for indivs
    if (returnList) indivsLst<-list();
    for (ctr in 1:length(typeNames)){
        typeName<-typeNames[ctr];
        if (typeName %in% lhsTypes){
            qry<-"select
                    *
                  from
                    indivs i
                  where
                    i.typeName='&&typeName';";
            qry<-gsub("&&typeName",typeName,qry); 
            cat("query = ",qry,sep='\n');
            indivsTmp<-sqldf::sqldf(qry);
            
            #assign correct names to columns for life stage
            lhsVars<-lhsTypeInfo$lifeStageTypes[[typeName]]$info;
            allVars<-c(conVars,stdVarsAll,lhsVars);
            names(indivsTmp)[1:length(allVars)]<-allVars;
            
            #determine output column names for life stage
            varsOutStr<-paste(c(conVars,stdVarsOut,lhsVars),sep='',collapse=',')
    
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

#sIndivs<-extractIndivs(indivConn,results,onlySuccessful  =TRUE,lhsTypes='benthic.juvenile',outBaseCSV=  'SuccessfulIndivs',newResType=TRUE,lhsTypeInfo=getLifeStageInfo.ATF(),returnList=FALSE);
#uIndivs<-extractIndivs(indivConn,results,onlyUnsuccessful=TRUE,lhsTypes='benthic.juvenile',outBaseCSV='UnsuccessfulIndivs',newResType=TRUE,lhsTypeInfo=getLifeStageInfo.ATF(),returnList=FALSE);
# successfulIndivs<-extractIndivs(newResType=FALSE,lhsTypeInfo=getLifeStageInfo.POP());
