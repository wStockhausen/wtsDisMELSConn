#'
#'@title Plot a connectivity matrix from a dataframe.
#'
#'@description Plot a connectivity matrix from a dataframe.
#'
#'@param tbl - dataframe or matrix representation of a connectivity matrix (can be NULL)
#'@param zmax - max value for color scale
#'@param zmin - min value for color scale
#'@param title - title for plot
#'@param xlab - label for x axis
#'@param ylab - label for y axis
#'@param cex - character expansion factor for plot
#'@param colorscale - color scale to use for plot ('hot', 'cold' or 'coldhot')
#'@param plotfile - file to save plot to (if devtype != 'win')
#'@param plotsize - 2 element vector giving plot size in pixels (c(W,H))
#'@param devtype - device type ('win' or 'png')
#'
#----------------------------------------------------------
plotCM.dataframe<-function(tbl,
                           zmax=NULL,
                           zmin=0,
                           title=NULL,
                           xlab=xlab,
                           ylab=ylab,
                           devtype='win',
                           cex=1*(devtype=='win')+1.5*(devtype!='win'),
                           colorscale=c("hot","cold","coldhot"),
                           plotfile="CM.png",
                           plotsize=c(970,780)){
  uSDZs<-unique(tbl[["start_depthzone"]]);
  uSAZs<-unique(tbl[["start_alongshorezone"]]);
  uNDZs<-unique(tbl[["end_depthzone"]]);
  uNAZs<-unique(tbl[["end_alongshorezone"]]);
  
  nrow<-length(uSDZs)*length(uSAZs);
  ncol<-length(uNDZs)*length(uNAZs);
  prbMat<-matrix(data=0,nrow=nrow,ncol=ncol);
  
  for (h in 1:length(uSDZs)){
    for (i in 1:length(uSAZs)){
      for (j in 1:length(uNDZs)){
        for (k in 1:length(uNAZs)){
          idx<-which((tbl[["start_depthzone"]]==uSDZs[h])&(tbl[["start_alongshorezone"]]==uSAZs[i])&(tbl[["end_depthzone"]]==uNDZs[j])&(tbl[["end_alongshorezone"]]==uNAZs[k]));
#          cat(uSDZs[h],uSAZs[i],uNDZs[j],uNAZs[k],idx,tbl$prSetBySrc[idx],"\n");
          prbMat[i+(h-1)*length(uSAZs),k+(j-1)*length(uNAZs)]<-tbl$prSetBySrc[idx];
        }
      }
    }
  }

  plotCM.matrix(prbMat,
                uNAZs=uNAZs,
                uNDZs=uNDZs,
                uSAZs=uSAZs,
                uSDZs=uSDZs,
                xlab="Nursery Areas/Alongshore Zones",
                ylab="Spawning Areas/Alongshore Zones",
                zmax=zmax,
                zmin=zmin,
                title=title,
                devtype=devtype,
                cex=cex,
                colorscale=colorscale,
                plotfile=plotfile,
                plotsize=plotsize);
}