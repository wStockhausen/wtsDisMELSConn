#'
#' @title Calculate an "average" connectivity matrix.
#'
#' @description Calculates an "average" connectivity matrix from 
#' results of a connectivity analysis. 
#'
#' @param allRes - the results list from a connectivity analysis
#' 
#' @return list with 5 elements:
#' \itemize{
#'  \item avg  - the average connectivity matrix 
#'  \item stdv - the standard deviations in connectivity matrix form
#'  \item devs - the deviations in connectivity matrix form
#'  \item dmin - the minimum value in the devs
#'  \item dmax - the maximum value in the devs
#'}
#'  
#'  @export
#'  
calcAvgCM<-function(allRes){
  
  nRes<-length(allRes)
  avg<-0*allRes[[1]]$prbSinkGivenSource$prSetBySrc
  for (res in allRes){
    avg<-avg+res$prbSinkGivenSource$prSetBySrc;
  }
  avg<-avg/nRes;
  avgRes<-list();
  avgRes$prbSinkGivenSource<-allRes[[1]]$prbSinkGivenSource;
  avgRes$prbSinkGivenSource$prSetBySrc<-avg;
  
  dmin<-Inf;
  dmax<--Inf;
  stdv<-0*avg;
  devs<-vector(mode='list',length=nRes);
  for (i in 1:nRes){
    res<-allRes[[i]]
    devs[[i]]<-list();
    devs[[i]]$prbSinkGivenSource<-res$prbSinkGivenSource;
    devs[[i]]$prbSinkGivenSource$prSetBySrc<-res$prbSinkGivenSource$prSetBySrc-avg;
    dmin<-min(dmin,devs[[i]]$prbSinkGivenSource$prSetBySrc,na.rm=TRUE)
    dmax<-max(dmax,devs[[i]]$prbSinkGivenSource$prSetBySrc,na.rm=TRUE)
    stdv<-stdv+devs[[i]]$prbSinkGivenSource$prSetBySrc^2;
  }
  stdv<-sqrt(stdv/(nRes-1));
  stdRes<-list();
  stdRes$prbSinkGivenSource<-allRes[[1]]$prbSinkGivenSource;
  stdRes$prbSinkGivenSource$prSetBySrc<-stdv;
  
  names(devs)<-names(allRes);
  
  return(list(avg=avgRes,stdv=stdRes,devs=devs,dmin=dmin,dmax=dmax));
}