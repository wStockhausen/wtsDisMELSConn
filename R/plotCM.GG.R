#'
#'@title Plot connectivity matrices using ggplot2
#'
#'@description Function to plot connectivity matrices using ggplot2.
#'
#'@param dfr - dataframe to plot
#'@param x - column name for x values
#'@param y - column name for y values
#'@param z - column name for z values
#'@param title - plot title
#'@param xLab - x axis label
#'@param yLab - y axis label
#'@param zLab - z label
#'@param xMin - x axis minimum
#'@param xMax - x axis maximum
#'@param yMin - y axis minimum
#'@param yMax - y axis maximum
#'@param zMin - z axis minimum
#'@param zMax - z axis maximum
#'@param facetWrap - formula for ggplot2::facet_wrap
#'@param facetGrid - formula for ggplot2::facet_grid
#'@param ncol - number of columns for ggplot2::facet_wrap
#'@param reverseY - flag to reverse y axis
#'@param reverseX - flag to reverse x axis
#'@param useGrad2 - flag to use reshape2::scale_fill_gradient2 for fill axis
#'
#'@return a ggplot2 object
#'
#'@details none.
#'
#'@import ggplot2
#'
#'@export
#'
plotCM.GG<-function(dfr,
                     x='nursery area',
                     y='spawning area',
                     z='Pr(N|S)',
                    title=NULL,
                     xLab=x,
                     yLab=y,
                     zLab=z,
                     xMin=NULL,
                     xMax=NULL,
                     yMin=NULL,
                     yMax=NULL,
                     zMin=NULL,
                     zMax=NULL,
                     facetWrap=NULL,
                     facetGrid=NULL,
                     ncol=4,
                     reverseX=FALSE,
                     reverseY=FALSE,
                     useGrad2=FALSE){
  #create dataframe to be plotted
  dfrp <- dfr;
  if (is.null(zMax)) zMax<-max(abs(dfrp[[z]]),na.rm=TRUE);
  dfrp[["zp"]]<-dfrp[[z]];
  if (!is.null(zMin)) dfrp[["zp"]]<-ifelse(abs(dfrp[[z]])<=zMin,NA,dfrp[[z]]);
  
  #determine axis limits, breaks, and labels
  dfrp[[x]]<-as.numeric(dfrp[[x]]);
  uX<-unique(dfrp[[x]]);
  if (is.null(xMin)) xMin<-min(uX,na.rm=TRUE)-0.5;
  if (is.null(xMax)) xMax<-max(uX,na.rm=TRUE)+0.5;
  dfrp[[y]]<-as.numeric(dfrp[[y]]);
  uY<-unique(dfrp[[y]]);
  if (is.null(yMin)) yMin<-min(uY,na.rm=TRUE)-0.5;
  if (is.null(yMax)) yMax<-max(uY,na.rm=TRUE)+0.5;
  cat('uX =',uX,'\n')
  cat('xMin=',xMin,', xMax=',xMax,"\n",sep='')
  cat('uY =',uY,'\n')
  cat('yMin=',yMin,', yMax=',yMax,"\n",sep='')
  
  #replace spaces in column names with underscores
  names(dfrp)<-gsub(' ','_',names(dfrp),fixed=TRUE);
  xp<-gsub(' ','_',x,fixed=TRUE);
  yp<-gsub(' ','_',y,fixed=TRUE);
  
  #make plot
  p1 <- ggplot(dfrp,aes_string(x=xp,y=yp,fill='zp'));
  p1 <- p1 + geom_tile(linetype=1,colour='black',size=0.5);
  if (!reverseX) {
    p1 <- p1 + scale_x_continuous(limits=c(xMin,xMax),breaks=uX,labels=uX,expand=c(0,0));
  } else {
    p1 <- p1 + scale_x_reverse(limits=c(xMax,xMin),breaks=uX,labels=uX,expand=c(0,0));
  }
  if (!reverseY) {
    p1 <- p1 + scale_y_continuous(limits=c(yMin,yMax),breaks=uY,labels=uY,expand=c(0,0));
  } else {
    p1 <- p1 + scale_y_reverse(limits=c(yMax,yMin),breaks=uY,labels=uY,expand=c(0,0));
  }
  if (!useGrad2){
    p1 <- p1 + scale_fill_gradient(name=zLab,low="lightyellow",high="red",limits=c(0,zMax),na.value='grey50');
  } else {
    p1 <- p1 + scale_fill_gradient2(name=zLab,low="blue",mid='white',high="red",midpoint=0,limits=c(-zMax,zMax),na.value='grey50');
  }
  p1 <- p1 + xlab(xLab);
  p1 <- p1 + ylab(yLab);
  if (!is.null(facetWrap)) p1 <- p1 + facet_wrap(facetWrap,ncol=ncol);
  if (!is.null(facetGrid)) p1 <- p1 + facet_grid(facetGrid);
  if (!is.null(title))     p1 <- p1 + ggtitle(title) + theme(plot.title=element_text(size=rel(0.8)));
  
  return(p1)
}