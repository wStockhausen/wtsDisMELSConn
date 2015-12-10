#'
#'@title Calculate time series of the total fraction recruiting by source.
#'
#'@description Function to calculate  time series of the total fraction recruiting by source.
#'
#' @param allRes - results list object from calcConnectivityMatrices(...)
#' 
#' @return matrix of fractions recruiting, with times as rows and sources as columns
#' 
#'@importFrom sqldf sqldf
#'
#' @export
#' 
doAnalysis.FractionRecruitingBySource<-function(allRes){
    nObs<-length(allRes);                     #number of observations (times) for connectivity results
    dfr<-allRes[[1]]$prbSinkGivenSource;      #connectivity matrix for 1st time
    src_dzs<-unique(dfr$start_depthzone);     #spawning (source) depth zones 
    src_azs<-unique(dfr$start_alongshorezone);#spawning (source) alongshore zones
    nSrc<-length(src_dzs)*length(src_azs);    #total number of spawning (source) zones

    #compute total fraction recruiting to all nursery areas by spawning area for each time
    times<-names(allRes); #observations (times) will be rows
    srcs<-c();            #spawning areas (sources) will be columns
    for (src_dz in src_dzs){srcs<-c(srcs,paste(src_dz,src_azs,sep='.'))}
    prbRec.Source<-matrix(nrow=nObs,ncol=nSrc);
    rownames(prbRec.Source)<-times;
    colnames(prbRec.Source)<-srcs;
    qry<-'select 
            start_depthzone||"."||start_alongshorezone as src,
            sum(prSetBySrc) as frRec
          from prbSGS
          group by src
          order by src;';
    for (time in times){
        prbSGS<-allRes[[time]]$prbSinkGivenSource;
        prbRS<-sqldf(qry);
        for (src in srcs){
            prbRec.Source[time,src]<-prbRS[prbRS$src==src,"frRec"];
        }
    }
    return(prbRec.Source)
}
