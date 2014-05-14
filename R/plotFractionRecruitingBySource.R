#'
#'@title Produce plots based on the time series of the total fraction recruiting by source 
#'       from doAnalysis.FractionRecruitingBySource(...)
#'
#'@description Function to produce plots based on time series of the total fraction recruiting by source.
#'
#' @param prbRS - results object from doAnalysis.FractionRecruitingBySource(...)
#' 
#' @param srcLbls - character vector with labels to use for sources in plots
#' 
#' @param xTicks - ??
#' 
#' @param pal - name of color palette from which to create colors for plot (if clrs=NULL)
#' 
#' @param clrs - colors to use for lot (NULL to use pal)
#' 
#' @param devtype - device type on which to plot
#' 
#' @param basename - base filename for saving plots (ignored if devtype='win')
#' 
#' @param width - figure width (ignored if devtype='win'; pixels if devtype='png', inches if devtype='pdf')
#' 
#' @param height - figure height (ignored if devtype='win'; pixels if devtype='png', inches if devtype='pdf')
#' 
#' 
#' @export
#' 
#' @import RColorBrewer
#' @import graphics
#' @importFrom wtsUtilities computeStats
#'
plotFractionRecruitingBySource<-function(prbRS,
                                          srcLbls=NULL,
                                          xTicks=NULL,
                                          pal=c('Spectral','BrBG','PiYG','PRGn','PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn'),
                                          clrs=NULL,
                                          devtype='win',
                                          basename="FractionRecruitingBySource",
                                          width=935,
                                          height=571){
    nt<-nrow(prbRS);
    ns<-ncol(prbRS);
    
    x<-1:nt;
    if (is.null(xTicks)) xTicks<-as.numeric(substr(rownames(prbRS),1,4));
    ymx<-max(prbRS,na.rm=TRUE);
    
    srcs<-colnames(prbRS);
    if (is.null(srcLbls)){
        tmp<-strsplit(srcs,'.',fixed=TRUE);
        srcLbls<-vector(mode='character',length=ns);
        for (s in 1:ns){
            srcLbls[s]<-tmp[[s]][2];
        }
    }
    
    #create colors for each spawning area (source)
    if (is.null(clrs)) {
        clrs<-brewer.pal(min(ns,11),pal[1]);
    }
    clrs<-rep(clrs,length.out=ns);
    names(clrs)<-srcs;
    
    #create point types for each spawning area (source)
    pchs<-c(21,22,23);
    pchs<-rep(pchs,length.out=ns,each=min(ns,11))
    names(pchs)<-srcs
    
    if (devtype=='pdf') {
        pdf(file=paste(basename,"pdf",sep='.'),width,height,onefile=TRUE);
        old.par<par(mfrow=c(3,1));
        on.exit(par(old.par));
    }
    
    #plot time series of fractions recruiting by spawning area
    if (devtype=='png') png(filename=paste(basename,'TimeSeries','png',sep='.'),width=width,height=height);
    plot(x,ymx+0*x,ylim=c(0,ymx),type='n',
         xlab='',xaxt='n',
         ylab='Fraction Recruiting');
    axis(1,at=x,labels=xTicks);
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = gray(0.75))
    for (src in srcs){
        lines(x,prbRS[,src],lwd=2,col=clrs[src]);
        points(x,prbRS[,src],pch=pchs[src],col=clrs[src],bg=clrs[src]);
    }
    legend("topleft",srcLbls,
           col=clrs,pch=pchs,pt.bg=clrs,lwd=2,
           cex=0.8,ncol=floor((ns-1)/6+1));
    if (devtype=='png') dev.off();
    
    #make box-and-whiskers plot of fraction recruiting by spawning area
    dfr<-as.data.frame(prbRS);
    names(dfr)<-srcLbls;
    if (devtype=='png') png(filename=paste(basename,'BoxPlot','png',sep='.'),width=width,height=height);
    boxplot(as.list(dfr),xlab='Spawning Area',ylab='Fraction Recruiting',col=clrs)
    if (devtype=='png') dev.off();
    
    #plot time series of standardized residuals for spawning areas
    stats<-wtsUtilities::computeStats(prbRS);
    devs<-0*prbRS;
    for (src in srcs) {
        if(stats[src,"stdev"]>0) devs[,src]<-(prbRS[,src]-stats[src,"mean"])/(stats[src,"stdev"])
    }
    
    #plot time series of devs recruiting by spawning area
    ymx<-max(abs(devs),na.rm=TRUE);
    if (devtype=='png') png(filename=paste(basename,'StdDevs','png',sep='.'),width=width,height=height);
    plot(x,0*x,ylim=ymx*c(-1,1),type='n',
         xlab='',xaxt='n',
         ylab='Standardized Dev.s (fraction recruiting)');
    axis(1,at=x,labels=xTicks);
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = gray(0.75))
    for (src in srcs){
        lines(x,devs[,src],lwd=2,col=clrs[src]);
        points(x,devs[,src],pch=pchs[src],col=clrs[src],bg=clrs[src]);
    }
    legend("bottomleft",srcLbls,
           col=clrs,pch=pchs,pt.bg=clrs,lwd=2,
           cex=0.8,ncol=floor((ns-1)/6+1));
    if (devtype=='png') dev.off();
    
    if (devtype=='pdf') dev.off();
    
}
