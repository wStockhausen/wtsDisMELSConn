#'
#'@title Calculate numbers of successful individuals by source, sink areas.
#'
#'@description Function to calculate numbers of successful individuals by source, sink areas.
#'
#'@param dfrNumBySrc
#'@param dfrConMats
#'
#'@import sqldf
#'
#'@export
#'
calcNumBySrcSink<-function(dfrNumBySrc,dfrConMats){
    require(sqldf);
    dfrN<-dfrNumBySrc;
    names(dfrN)<-c("d","sd","sa","tR","tS","pS");
    dfrC<-dfrConMats;
    names(dfrC)<-c("d","sd","sa","ed","ea","pSbS");
    qry<-"select
            n.d,n.sd,n.sa,c.ed,c.ea,
            n.tR as totRel,
            n.tR*c.pSbS as totSet
         from dfrN n left join dfrC c
         on
            n.d=c.d and
            n.sd=c.sd and n.sa=c.sa;"
    dfrNbySS<-sqldf(qry);
    names(dfrNbySS)<-c("date","start_depthzone","start_alongshorezone",
                       "end_depthzone","end_alongshorezone","totRel","totSet");
    return(dfrNbySS);
}

#dfrNbySS<-calcNumBySrcSink(dfrNumBySrc,dfrConMats);
#View(dfrNbySS);
