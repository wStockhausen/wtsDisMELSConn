#'
#'@title Aggregate dataframe with connectivity matrices according to new dates, spawning areas, nursery areas.
#'
#'@description Function to aggregate dataframe with connectivity matrices according to new dates, spawning areas, nursery areas.
#'
#'@param dfrCMs - dataframe with connectivity matrices, with numbers released and finished
#'@param newDates - vector of reclassified dates to aggregate to (or NULL to keep originals)
#'@param newSAs - vector of reclassified spawning areas to aggregate to (or NULL to keep originals)
#'@param newNAs - vector of reclassified nursery areas to aggregate to (or NULL to keep originals)
#'@param debug - flag to turn on debugging output
#'
#'@return dataframe with same column names as dfrCMs, but aggregated to new levels
#'
#'@details input dataframe ("dfrCMs") is assumed to have columns in the order 
#' (date, spawning area, nursery area, numRel, numFin, and prFin)
#'although the names associated with these columns are arbitrary.
#'
#'@import sqldf
#'
#'@export
#'
aggregateCMs<-function(dfrCMs,
                       newDates=NULL,
                       newSAs=NULL,
                       newNAs=NULL,
                       debug=FALSE){
  dfr<-dfrCMs;
  ##asign names to variables
  nms<-names(dfr);
  dt<-nms[1];
  sa<-nms[2];
  na<-nms[3];
  nr<-nms[4];
  nf<-nms[5];
  pf<-nms[6];
  
  ##add new versions to dataframe
  if (is.null(newDates)) {
    dfr$newDT<-dfr[[dt]];
  } else {
    dfr$newDT<-newDates;
  }
  if (is.null(newSAs)) {
    dfr$newSA<-dfr[[sa]];
  } else {
    dfr$newSA<-newSAs;
  }
  if (is.null(newNAs)) {
    dfr$newNA<-dfr[[na]];
  } else {
    dfr$newNA<-newNAs;
  }

  ##pull out total released by date, spawning area
  qry<-"select distinct
         &&dt,&&sa,newDT,newSA,&&nr
        from dfr;";
  qry<-gsub("&&dt",paste("`",dt,"`",sep=''),qry,fixed=TRUE);
  qry<-gsub("&&sa",paste("`",sa,"`",sep=''),qry,fixed=TRUE);
  qry<-gsub("&&nr",paste("`",nr,"`",sep=''),qry,fixed=TRUE);
  if (debug) cat(qry,"\n");
  dfrNRp<-sqldf(qry);
  ##aggregate by newDT, newSA 
  qry<-"select
          newDT,newSA,sum(&&nr) as &&nr
        from dfrNRp
        group by newDT, newSA
        order by newDT, newSA;";
  qry<-gsub("&&nr",paste("`",nr,"`",sep=''),qry,fixed=TRUE);
  if (debug) cat(qry,"\n");
  dfrNR<-sqldf(qry);
  if (debug) View(dfrNR);

  ##get total finished by date, spawning area, nursery area
  qry<-"select distinct
         &&dt,&&sa,&&na,newDT,newSA,newNA,&&nf
        from dfr;";
  qry<-gsub("&&dt",paste("`",dt,"`",sep=''),qry,fixed=TRUE);
  qry<-gsub("&&sa",paste("`",sa,"`",sep=''),qry,fixed=TRUE);
  qry<-gsub("&&na",paste("`",na,"`",sep=''),qry,fixed=TRUE);
  qry<-gsub("&&nf",paste("`",nf,"`",sep=''),qry,fixed=TRUE);
  if (debug) cat(qry,"\n");
  dfrNFp<-sqldf(qry);
  ##aggregate by newDT, newSA, newNA 
  qry<-"select
          newDT,newSA,newNA,sum(&&nf) as &&nf
        from dfrNFp
        group by newDT, newSA, newNA
        order by newDT, newSA, newNA;";
  qry<-gsub("&&nf",paste("`",nf,"`",sep=''),qry,fixed=TRUE);
  if (debug) cat(qry,"\n");
  dfrNF<-sqldf(qry);
  if (debug) View(dfrNF);
  
  ##combine dfrNR and dfrNF
  qry<-"select
          f.newDT, 
          f.newSA, 
          newNA,
          r.&&nr as &&nr, 
          &&nf
        from
          dfrNR r, dfrNF f
        where
          r.newDT=f.newDT and r.newSA=f.newSA
        order by f.newDT, f.newSA, newNA;"
  qry<-gsub("&&nr",paste("`",nr,"`",sep=''),qry,fixed=TRUE);
  qry<-gsub("&&nf",paste("`",nf,"`",sep=''),qry,fixed=TRUE);
  if (debug) cat(qry,'\n');
  dfrNRF<-sqldf(qry);
  dfrNRF[[pf]]<-dfrNRF[[nf]]/dfrNRF[[nr]];
  names(dfrNRF)[1:3]<-nms[1:3];
  if (debug) View(dfrNRF);
  
  return(dfrNRF);
}

# ##aggregate over new spawning and nursery areas
# tst<-aggregateCMs(dfrCMs,newSAs=newSAs,newNAs=newNAs);
# View(tst);
# ##aggregate over all nursery areas
# tst<-aggregateCMs(dfrCMs,newSAs=NULL,newNAs=0*dfrCMs$`nursery area`);
# View(tst);
# ##aggregate over all spawning areas
# tst<-aggregateCMs(dfrCMs,newSAs=0*dfrCMs$`spawning area`,newNAs=NULL);
# View(tst);
