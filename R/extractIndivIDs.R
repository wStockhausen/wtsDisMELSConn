#'
#'@title Extract unique IDs for individuals from a DisMELS model run
#'
#'@description Function to extract unique IDs for individuals from a DisMELS model run.
#'
#'@param indivConn        - individual connectivity file (or dataframe)
#'@param lhsTypeInfo      - life stage info list for IBM (req'd if onlySuccessful=TRUE or onlyUnsuccessful=TRUE)
#'@param onlySuccessful   - flag (T/F) to pull out only successful individuals
#'@param onlyUnsuccessful - flag (T/F) to pull out only unsuccessful individuals
#'@param successful - sql "where" clause to determine final state for individuals regarded as successful
#'@param nurseryZones     - vector of names of zones used as nursery areas in the IBM (req'd if onlySuccessful or onlyUnsuccessful=TRUE)
#'@param verbose - flag (T/F) to print debugging info
#'
#'@return data frame with columns 
#'ID, start_typeName,start_depthzone,start_alongshorezone,end_typeName,end_depthzone,end_alongshorezone
#'
#'@details
#'If onlySuccessful=TRUE, then it is assumed that 'successful' individuals are those which are in
#'one of the nursery areas and meet the 'successful' criteria.\cr
#'\cr
#'If onlyUnsuccessful=TRUE, then it is assumed that 'unsuccessful' individuals are those which 
#'don't meet the 'successful' criteria.
#'
#'@import sqldf
#'@import wtsUtilities
#'
#'@export
#'
extractIndivIDs<-function(indivConn=NULL,  
                          lhsTypeInfo=NULL,
                          onlySuccessful=FALSE,
                          onlyUnsuccessful=FALSE,
                          successful='where (end_typeName="benthic.juvenile")',
                          nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"),
                          verbose=FALSE){
    
    retIndivConn<-FALSE;
    if (!is.data.frame(indivConn)){
        #read in individual connectivity results from csv file
        if (is.null(indivConn)){
            indivConn<-getCSV(caption='Select individual connectivity results file');
            if (is.null(indivConn)) return(NULL);#user aborted
        } else {
            indivConn<-read.csv(indivConn,stringsAsFactors=FALSE);
        }
        retIndivConn<-TRUE;
    }
    
    if (verbose) cat('----running extractIndivIDs(...)----\n')
    
    #pull out distinct IDs 
    if (!onlySuccessful&!onlyUnsuccessful) {
        if (verbose) cat('--Selecting all individuals\n')
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
        if (verbose) cat(qry,'\n');
        dfrIDs<-sqldf(qry);
    } else {
        nurseryZones<-as.data.frame(list(zone=nurseryZones));
        #successful indivs are in nursery zones and meet success criteria
        if (verbose) cat('--Selecting only successful individuals\n')
        qry<-"select
                ID,
                start_typeName,
                start_depthzone,
                start_alongshorezone,
                end_typeName,
                end_depthzone,
                end_alongshorezone
              from
                (select * from indivConn &&successful) i,
                nurseryZones z
              where
                i.end_depthzone = z.zone
              order by
                ID;";
        qry<-gsub("&&successful",successful,qry);   
        if (verbose) cat(qry,'\n');
        sIDs<-sqldf(qry);
        if (onlySuccessful){
          dfrIDs<-sIDs;
        } else {
            #unsuccessful indivs don't reach the last stage
            if (verbose) cat('--Now selecting only unsuccessful individuals\n')
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
                    ID not in (select ID from sIDs)
                  order by
                    ID;";
            if (verbose) cat(qry,'\n');
            dfrIDs<-sqldf(qry);   
        }
    }
    if (verbose) cat('----finished running extractIndivIDs(...)----\n')
    return(invisible(dfrIDs));
}

#dfr.suc<-extractIndivIDs(indivConn,onlySuccessful=TRUE,lhsTypeInfo=getLifeStageInfo.ATF())
#dfr.uns<-extractIndivIDs(indivConn,onlyUnsuccessful=TRUE,lhsTypeInfo=getLifeStageInfo.ATF())
#dfr.all<-extractIndivIDs(indivConn,lhsTypeInfo=getLifeStageInfo.ATF())
