#'
#'@title Function to extract tracks of successful individuals from a DisMELS model run.
#'
#'@param indivConn = individual connectivity file (or dataframe)
#'@param results = DisMELS results file (or dataframe)
#'@param newResType - flag (T/F) indicating whether (T) or not (F) results are from a "new"-type IBM
#'@param nurseryZones - vector of names of zones used as nursery areas in the IBM
#'@param lhsTypeInfo = life stage info list for IBM
#'@params stdVars = vector of standard model variables
#'@param outDir = folder for output
#'@param outBaseCSV = base name for output csv file
#'
#'@import sqldf
#'@importFrom wtsUtilities getCSV
#'
#'@export
#'
extractSuccessfulIndivs<-function(indivConn=NULL,  #individual connectivity file (or dataframe)
                                  results=NULL,    #DisMELS results file (or dataframe)
                                  newResType=FALSE,
                                  nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), #nursery area name(s)
                                  lhsTypeInfo=NULL,    
                                  outDir='./',
                                  outBaseCSV="successfulIndivs"
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

    #define variables
    if (newResType){
          stdVarsAll<-c('typeName','id','parentID','origID','startTime','time',
                        'horizType','vertType','horizPos1','horizPos2','vertPos','gridCellID','track',
                        'active','alive','attached','age','ageInStage','number');
    } else {
          stdVarsAll<-c('typeName','id','parentID','origID','horizType','vertType','active','alive','attached',
                        'startTime','time','age','ageInStage','size','number','horizPos1','horizPos2','vertPos',
                        'temp','salinity','gridCellID','track');
          lhsVars<-c('size','temp','salinity');
    }
    stdVarsOut<-c('typeName','id','parentID','origID',
                  'time','horizPos1','horizPos2','vertPos','track',
                  'alive','age','ageInStage','number');
    
    #pull out IDs for successful indviduals from indivConn
    typeNames=names(lhsTypeInfo$lifeStageTypes);
    lastLHS<-typeNames[length(typeNames)];
    nurseryZones<-as.data.frame(list(zone=nurseryZones));
    qry<-"select
            ID
          from
            indivConn i,
            nurseryZones z
          where
            i.end_depthzone=z.zone and 
            i.end_typeName='&&lastLHS'
          order by
            ID;";
    qry<-gsub("&&lastLHS",lastLHS,qry);   
    cat(qry,'\n');
    indivIDs<-sqldf(qry);
    
    #extract successful indivs from results
    resVars<-paste('r',names(results),sep='.',collapse=',');
    qry<-"select
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
    indivs<-sqldf(qry);    
    
    #assign temporary names to columns
    nc<-length(names(indivs));
    names(indivs)<-paste('c',1:nc,sep='');
    names(indivs)[1:2]<-c('typeName','id')
    
    #loop over type names and extract results for successful indivs
    indivsLst<-list();
    for (ctr in 1:length(typeNames)){
        typeName<-typeNames[ctr];
        qry<-"select
                *
              from
                indivs i
              where
                i.typeName='&&typeName';";
        qry<-gsub("&&typeName",typeName,qry); 
        cat("query = ",qry,sep='\n');
        indivsTmp<-sqldf(qry);
        
        #assign correct names to columns
        if (newResType) {
            lhsVars<-lhsTypeInfo$lifeStageTypes[[typeName]]$info;
            allVars<-c(stdVarsAll,lhsVars);
        } else {
            allVars<-stdVarsAll;#don't need to add in lhsVars, they're already there
        }
        names(indivsTmp)[1:length(allVars)]<-allVars;
        
        varsOutStr<-paste(c(stdVarsOut,lhsVars),sep='',collapse=',')

        qry<-"select
                &&varsOut
              from
                indivsTmp
              order by
                id,age;";
        qry<-gsub("&&varsOut",varsOutStr,qry);  
        cat("query = ",qry,sep='\n');
        indivsType<-sqldf(qry);
        
        #write indivs to csv file
        write.csv(indivsType,file=file.path(outDir,paste(outBaseCSV,ctr,typeName,'csv',sep='.')),row.names=FALSE);
        indivsLst[[typeName]]<-indivsType;
    }
    
#     if (retRes&&retIndivConn) return(invisible(list(indivConn=indivConn,results=results)));
#     if (retRes)               return (invisible(results));
#     if (retIndivConn)         return(invisible(indivConn));
    return(invisible(indivsLst));
}

# indivConn<-extractSuccessfulIndivs();
# results<-extractSuccessfulIndivs(indivConn);
# successfulIndivs<-extractSuccessfulIndivs(newResType=TRUE,lhsTypeInfo=getLifeStageInfo.ATF());
# successfulIndivs<-extractSuccessfulIndivs(newResType=FALSE,lhsTypeInfo=getLifeStageInfo.POP());
