#'
#'@title Plot a connectivity matrix from a matrix representation.
#'
#'@description Plot a connectivity matrix from a matrix representation.
#'
#'@param prbMat - matrix representation of a connectivity matrix (can be NULL)
#'@param uNAZs - character vector of unique nursery alongshore zones
#'@param uNDZs - character vector of unique nursery depth zones
#'@param uNSZs - character vector of unique spawning alongshore zones
#'@param uSDZs - character vector of unique spawning depth zones
#'@param zmax - max value for color scale
#'@param zmin - min value for color scale
#'@param title - title for plot
#'@param xlab - label for x axis
#'@param ylab - label for y axis
#'@param cex - character expansion factor for plot
#'@param colorscale - color scale to use for plot ('hot', 'cold' or 'coldhot')
#'@param plotfile - file to save plot to (if devtype != 'win')
#'@param plotsize - 2 element vector giving plot size in pixels (c(W,H))
#'@param devtype - 'win' or 'png'
#'
#'@import graphics
#'@importFrom wtsUtilities createColorScale
#'
#source("../Plots/createColorScale.R",chdir=TRUE);
#----------------------------------------------------------
plotConnectivityMatrix.matrix<-function(prbMat,
                                        uNAZs=NULL,
                                        uNDZs=NULL,
                                        uSAZs=NULL,
                                        uSDZs=NULL,
                                        xlab="Nursery Areas/Alongshore Zones",
                                        ylab="Spawning Areas/Alongshore Zones",
                                        zmax=NULL,
                                        zmin=0,
                                        title=NULL,
                                        devtype='win',
                                        cex=1*(devtype=='win')+1.5*(devtype!='win'),
                                        colorscale=c("hot","cold","coldhot"),
                                        plotfile="ConnectivityMatrix.png",
                                        plotsize=c(970,780)){
  if (devtype=='win'){}
  else if (devtype=='png'){
      png(filename=plotfile,width=plotsize[1],height=plotsize[2])
  }
  
  nrow<-nrow(prbMat);
  ncol<-ncol(prbMat);
  
  ncolors<-100;
  if (is.null(zmax)) {zmax<-max(prbMat,na.rm=TRUE);}
  if (is.null(zmin)) {zmin<-min(prbMat,na.rm=TRUE);}
  
  nf<-layout(as.matrix(t(1:2)),widths=c(6,1),heights=1);
#  if (devtype=='win') {layout.show(2)}
  if (is.null(uNAZs)){
      old.par<-par(mar=c(3,3,3,1),oma=c(2,2,0,0));
  } else {
      old.par<-par(mar=c(5,5,5,5),oma=c(3,3,0,0));
  }
  on.exit(par(old.par))
  
  cat("zmin = ",zmin,"\n")
  cat("zmax = ",zmax,"\n")
  colorScale<-wtsUtilities::createColorScale(name=colorscale[1]);
  pal<-colorScale((1:ncolors)/ncolors)
#  image(1:ncol,1:nrow,t(prbMat),col=rev(heat.colors(ncolors)),
  image(1:ncol,1:nrow,t(prbMat),col=pal,
        xlab="",ylab="",bty="o",xaxt="n",yaxt="n",zlim=c(zmin,zmax));
  for (i in 2:nrow){ 
    lines(c(1,ncol+1)-0.5,c(i,i)-0.5,lwd=1,col="grey75");
  }
  for (i in 2:ncol){ 
    lines(c(i,i)-0.5,c(1,nrow+1)-0.5,lwd=1,col="grey75");
  }
  
  axis(1,at=1:(ncol+1)-0.5,labels=FALSE,cex.axis=cex*0.7,line=0,lwd.ticks=0);
  if (is.null(uNDZs)){
      axis(1,at=1:ncol,labels=colnames(prbMat),cex.axis=cex*0.7,line=0);
  } else {
      axis(1,at=1:ncol,labels=rep(1:length(uNAZs),times=length(uNDZs)),
           cex.axis=cex*0.7,line=0);
      axis(1,at=(1:length(uNDZs)-1)*length(uNAZs)+0.5*length(uNAZs),labels=uNDZs,
           cex.axis=cex*1.0,line=1.5,lwd=0);
  }
  
  axis(2,at=1:(nrow+1)-0.5,labels=FALSE,cex.axis=cex*0.7,line=0,lwd.ticks=0);
  if (is.null(uSDZs)){
      axis(2,at=1:nrow,labels=rownames(prbMat),cex.axis=cex*0.7,line=0);
  } else {
      axis(2,at=1:nrow,labels=rep(1:length(uSAZs),times=length(uSDZs)),
           cex.axis=cex*0.7,line=0);
      axis(2,at=(1:length(uSDZs)-1)*length(uSAZs)+0.5*length(uSAZs),labels=uSDZs,
           cex.axis=cex*1.0,line=1.5,lwd=0);
  }
  
  axis(3,at=1:(ncol+1)-0.5,labels=FALSE,cex.axis=cex*0.7,line=0,lwd.ticks=0);
  if (is.null(uNDZs)){
      axis(3,at=1:ncol,labels=colnames(prbMat),cex.axis=cex*0.7,line=0);
  } else {
      axis(3,at=1:ncol,labels=rep(1:length(uNAZs),times=length(uNDZs)),
           cex.axis=cex*0.7,line=0);
      axis(3,at=(1:length(uNDZs)-1)*length(uNAZs)+0.5*length(uNAZs),labels=uNDZs,
           cex.axis=cex*1.0,line=1.5,lwd=0);
  }
  
  axis(4,at=1:(nrow+1)-0.5,labels=FALSE,cex.axis=cex*0.7,line=0,lwd.ticks=0);
  if (is.null(uSDZs)){
      axis(2,at=1:nrow,labels=rownames(prbMat),cex.axis=cex*0.7,line=0);
  } else {
      axis(4,at=1:nrow,labels=rep(1:length(uSAZs),times=length(uSDZs)),
           cex.axis=cex*0.7,line=0);
      axis(4,at=(1:length(uSDZs)-1)*length(uSAZs)+0.5*length(uSAZs),labels=uSDZs,
           cex.axis=cex*1.0,line=1.5,lwd=0);
  }
  
  if (!is.null(title)){mtext(title,side=3,outer=TRUE,cex=cex,line=-1.5,adj=0.02);}
  mtext(xlab,side=1,outer=TRUE,cex=cex);
  mtext(ylab,side=2,outer=TRUE,cex=cex);
  
  par(mar=c(6,0,6,4))
  ticks<-pretty(c(zmin,zmax),min.n=2);
  zmin<-min(ticks)
  zmax<-max(ticks)
  clrmat<-matrix(data=zmin+(zmax-zmin)*(1:ncolors)/ncolors,nrow=1,ncol=ncolors)
  image(1,1:ncolors,clrmat,col=pal,xlab="",xaxt="n",ylab="",yaxt="n")
  axis(2,at=(1:(ncolors+1))-0.5,labels=FALSE,line=0,lwd.ticks=0);
  axis(3,at=(1:2)-0.5,labels=FALSE,line=0,lwd.ticks=0);
  axis(4,at=ncolors*(ticks-zmin)/(zmax-zmin),labels=ticks,cex.axis=cex*1.0)
  mtext("Pr(Settlement)",side=4,line=2.2,cex=cex)
   
  if (devtype!='win') {dev.off();}
}
