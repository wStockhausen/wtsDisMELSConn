#'
#'@title Aggregate numbers successful .
#'
#'@description Function to aggregate dataframe according to "factor" levels.
#'
#'@param dfrNbySS - dataframe with numbers by date and source/sink pairs (e.g., from calcNumBySrcSink())
#'@param factors - factors to aggregate by
#'@param vars - variable(s) to aggregate
#'
#'@return dataframe with vars aggregated by levels of "factor" columns
#'
#'@import wtsUtilities
#'
#'@export
#'
aggregateNumBySrcSink<-function(dfrNbySS,
                                factors=c("date","start_alongshorezone","end_alongshorezone","totRel"),
                                vars=c("totSet")){
    require(wtsUtilities);
    agg<-wtsUtilities::aggregateDataframe(dfrNbySS,factors,vars);
    return(agg);
}
    
#aggNumBySrc<-aggregateNumBySrcSink(dfrNbySS);
#View(aggNumBySrc);