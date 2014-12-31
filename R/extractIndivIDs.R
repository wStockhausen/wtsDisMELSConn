#'
#'@title Function to extract unique IDs for individuals from a DisMELS model run.
#'
#'@param indivConn        - individual connectivity file (or dataframe)
#'@param lhsTypeInfo      - life stage info list for IBM (req'd if onlySuccessful=TRUE or onlyUnsuccessful=TRUE)
#'@param onlySuccessful   - flag (T/F) to pull out only successful individuals
#'@param onlyUnsuccessful - flag (T/F) to pull out only unsuccessful individuals
#'@param nurseryZones     - vector of names of zones used as nursery areas in the IBM (req'd if onlySuccessful=TRUE)
#'
#'@return data frame with columns 
#'ID, start_typeName,start_depthzone,start_alongshorezone,end_typeName,end_depthzone,end_alongshorezone
#'
#'@details
#'If onlySuccessful=TRUE, then it is assumed that 'successful' individuals are those which are in
#'one of the nursery areas in the final life stage.\cr
#'\cr
#'If onlyUnsuccessful=TRUE, then it is assumed that 'unsuccessful' individuals are those which 
#'don't make it to the final life stage.
#'
#'@importFrom sqldf sqldf
#'@importFrom wtsUtilities getCSV
#'
#'@export
#'
extractIndivIDs<-function(indivConn=NULL,  
                          lhsTypeInfo=NULL,
                          onlySuccessful=FALSE,
                          onlyUnsuccessful=FALSE,
                          nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m")){
    
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
    
    cat('----running extractIndivIDs(...)----\n')
    
    #pull out distinct IDs 
    if (!onlySuccessful&!onlyUnsuccessful) {
        cat('--Selecting all individuals\n')
        qry<-"select distinct
                ID,
                start_typeName,
                start_depthzone,
                start_alongshorezone,
                end_typeName,
                end_depthzone,
                end_alongshorezone
              from
                indivConn i              
              order by
                ID;";
    } else {
        typeNames=names(lhsTypeInfo$lifeStageTypes);
        lastLHS<-typeNames[length(typeNames)];
        nurseryZones<-as.data.frame(list(zone=nurseryZones));
        if (onlySuccessful){
            #successful indivs are in nursery zones in last stage
            cat('--Selecting only successful individuals\n')
            qry<-"select
                    ID,
                    start_typeName,
                    start_depthzone,
                    start_alongshorezone,
                    end_typeName,
                    end_depthzone,
                    end_alongshorezone
                  from
                    indivConn i,
                    nurseryZones z
                  where
                    i.end_depthzone = z.zone and 
                    i.end_typeName = '&&lastLHS'
                  order by
                    ID;";
            qry<-gsub("&&lastLHS",lastLHS,qry);   
        } else {
            #unsuccessful indivs don't reach the last stage
            cat('--Selecting only unsuccessful individuals\n')
            qry<-"select distinct
                    ID,
                    start_typeName,
                    start_depthzone,
                    start_alongshorezone,
                    end_typeName,
                    end_depthzone,
                    end_alongshorezone
                  from
                    indivConn i
                  where
                    i.end_typeName != '&&lastLHS'
                  order by
                    ID;";
            qry<-gsub("&&lastLHS",lastLHS,qry);   
        }
    }
    cat(qry,'\n');
    indivIDs<-sqldf::sqldf(qry);
    cat('----finished running extractIndivIDs(...)----\n')
    return(invisible(indivIDs));
}

#dfr.suc<-extractIndivIDs(indivConn,onlySuccessful=TRUE,lhsTypeInfo=getLifeStageInfo.ATF())
#dfr.uns<-extractIndivIDs(indivConn,onlyUnsuccessful=TRUE,lhsTypeInfo=getLifeStageInfo.ATF())
#dfr.all<-extractIndivIDs(indivConn,lhsTypeInfo=getLifeStageInfo.ATF())
