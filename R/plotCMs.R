#'
#'@title Plot a set of connectivity matrices
#'
#'@description Plot a set of connectivity matrices (an allRes output from \code{calcConnectivityMatrices})
#'
#'@param allRes - allRes element in the output list from \code{calcConnectivityMatrices}
#'@param zmax - max value for color scale
#'@param zmin - min value for color scale
#'@param colorscale - color scale to use for plots ('hot', 'cold' or 'coldhot')
#'@param plotBase - base name for saved plots (should include YYYY, MM, and DD; e.g. "ConnectivityMatrixYYYYMMDD.png")
#'@param plotDir - directory for saved plots (devtype != 'win')
#'@param years - vector of years of results to plot (YYYY)
#'@param months - vector of months to plot (MM)
#'@param days - vector of days to plot (DD)
#'@param devtype - 'win' or 'png'
#'
#'@details calls \code{plotCM} to plot individual connectivity matrices.
#'
#'@export
#' 
#source("plotConnectivityMatrix.R",chdir=TRUE);
################################################################################
# Plot series of connectivity matrices
################################################################################
plotCMs<-function(allRes=NULL,
                   zmax=NULL,
                   zmin=0,
                   colorscale=c("hot","cold","coldhot"),
                   plotBase="ConnectivityMatrixYYYYMMDD.png",
                   plotDir='',
                   years=as.character(1996:2011),
                   months=c("01"),
                   days=c("01"),
                   devtype='win'){
  
  #process all matrices  

  for (year in years){
    yrBase<-gsub('YYYY',year,plotBase);
    for (month in months){
      mnBase<-gsub('MM',month,yrBase);
      for (day in days){ 
        cat('Processing',year,month,day,'\n',sep=' ')
        dyBase<-gsub('DD',day,mnBase);
        res<-allRes[[paste(year,month,day,sep='.')]];
        if (!is.null(res)){
            if (devtype=='win'){
              plotCM(res$prbSinkGivenSource,
                     zmax=zmax,
                     zmin=zmin,
                     colorscale=colorscale,
                     title=paste(day,month,year,sep='-'),
                     devtype='win')
            } else {
              plotCM(res$prbSinkGivenSource,
                     zmax=zmax,
                     zmin=zmin,
                     colorscale=colorscale,
                     title=paste(day,month,year,sep='-'),
                     plotfile=file.path(plotDir,dyBase),
                     devtype='png')
            }
        }
      }
    }
  }
}
