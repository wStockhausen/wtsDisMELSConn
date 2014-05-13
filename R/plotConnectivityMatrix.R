#'
#'@title Plot a connectivity matrix.
#'
#'@description Plot a connectivity matrix (from a matrix, dataframe, or csv file).
#'
#'@param tbl - dataframe or matrix representation of a connectivity matrix (can be NULL)
#'@param csvfile - csv file to read connectivity matrix from (can be NULL)
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
#'@export
#'@importFrom wtsUtilities getCSV
#' 

#source("../Plots/createColorScale.R",chdir=TRUE);
#source("../Utilities/addFilter.R",chdir=TRUE)
#W:970, H:780 for output png
#----------------------------------------------------------
plotConnectivityMatrix<-function(tbl=NULL,
                                 csvfile=NULL,
                                 zmax=NULL,
                                 zmin=0,
                                 title=NULL,
                                 xlab="Nursery Areas/Alongshore Zones",
                                 ylab="Spawning Areas/Alongshore Zones",
                                 devtype='win',
                                 cex=1*(devtype=='win')+1.5*(devtype!='win'),
                                 colorscale=c("hot","cold","coldhot"),
                                 plotfile="ConnectivityMatrix.png",
                                 plotsize=c(970,780)){
  
  if (is.null(tbl)){
    tbl<-wtsUtilities::getCSV(csvfile,caption="Select DisMELS connectivity matrix file");
  }
  
  if (is.data.frame(tbl)) plotConnectivityMatrix.dataframe(tbl,zmax=zmax,zmin=zmin,
                                                           title=title,xlab=xlab,ylab=ylab,
                                                           devtype=devtype,cex=cex,colorscale=colorscale,
                                                           plotfile=plotfile,plotsize=plotsize);
  if (is.matrix(tbl))     plotConnectivityMatrix.matrix(tbl,zmax=zmax,zmin=zmin,
                                                           title=title,xlab=xlab,ylab=ylab,
                                                           devtype=devtype,cex=cex,colorscale=colorscale,
                                                           plotfile=plotfile,plotsize=plotsize);
}
