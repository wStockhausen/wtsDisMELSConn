#'
#'@title Function to extract unique IDs for individuals from a DisMELS model run.
#'
#'@param indivConn = individual connectivity file (or dataframe)
#'@param onlySuccessful = flag (T/F) to pull out only successful individuals
#'@param nurseryZones = vector of names of zones used as nursery areas in the IBM (req'd if onlySuccessful=TRUE)
#'@param lhsTypeInfo = life stage info list for IBM (req'd if onlySuccessful=TRUE)
#'
#'@return data frame with single column (ID) of unique ids.
#'
#'@details
#'If onlySuccessful=TRUE, then it is assumed that 'successful' individuals are those which are in
#'one of the nursery areas in the final life stage.
#'
#'@import sqldf
#'@importFrom wtsUtilities getCSV
#'
#'@export
#'
extractIndivIDs<-function(indivConn=NULL,  #individual connectivity file (or dataframe)
                          onlySuccessful=FALSE,
                          nurseryZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), #nursery area name(s)
                          lhsTypeInfo=NULL){
    
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
    
    
    #pull out distinct IDs 
    if (onlySuccessful) {
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
    } else {
        qry<-"select distinct
                ID
              from
                indivConn i              
              order by
                ID;";
    }
    cat(qry,'\n');
    indivIDs<-sqldf(qry);
    return(invisible(indivIDs));
}

dfr<-extractIndivIDs(onlySuccessful=TRUE,lhsTypeInfo=getLifeStageInfo.ATF())
