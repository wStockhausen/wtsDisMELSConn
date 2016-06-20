#'
#'@title Plot a histogram for a column in a dataframe
#'
#'@description Function to plot a histogram for a column in a dataframe.
#'
#'@param dfr - dataframe
#'@param x - name of column for histogram
#'@param label - bin axis label
#'@param fill - histogram bin colour
#'@param binwidth - bin width for histogram
#'@param lines - vector of (nominally vertical) reference line positions
#'@param colour - colour for reference lines
#'@param linetype - type for reference lines
#'@param linesize - size for reference lines
#'@param showPct - flag to include \% observations falling outside lines as text on plot
#'@param reverse - reverse bin order
#'@param flip - flag to flip orientation
#'@param facets - expression for faceting using facet_grid()
#'@param showPlot - flag to print plot
#'
#'@return a ggplot2 object with an extra list element, 'xtra'. 'xtra' is a 
#'list with one element: pct.outside, which gives the \% observations falling
#'outside the range of the 'lines' (if specified), or NULL.
#'
#'@details none.
#'
#'@import grid
#'@import ggplot2
#'
#'@export
#'
plotHistogram<-function(dfr,
                        x,
                        label="",
                        fill='grey50',
                        binwidth=NULL,
                        lines=NULL,
                        colour='black',
                        linetype=2,
                        linesize=1.25,
                        showPct=FALSE,
                        reverse=FALSE,
                        flip=TRUE,
                        facets=NULL,
                        showPlot=FALSE){
  p <- ggplot(dfr,aes_string(x=x));
  p <- p + geom_histogram(binwidth=binwidth,fill=fill);
  pct <- NULL;
  if (!is.null(lines)){
    for (i in 1:length(lines)) p <- p + geom_vline(xintercept=lines[i],colour=colour,linetype=linetype,size=linesize);
    if (length(lines)>=2){
      rng<-range(lines,na.rm=TRUE);
      pct<-0.1*floor(1000*sum((dfr[[x]]<rng[1])|(dfr[[x]]>rng[2]))/length(dfr[[x]]));
      if (showPct){
        tg<-grobTree(textGrob(paste(pct,"% outside range",sep=''), x=0.1,  y=0.95, hjust=0, gp=gpar(col="blue", fontsize=15, fontface="italic")));
        p <- p + annotation_custom(tg);
      }
    }
  }
  p <- p + xlab(label);
  if (reverse) p <- p + scale_x_reverse();
  if (flip)    p <- p + coord_flip();
  if (!is.null(facets)) p <- p + facet_grid(facets);
  if (showPlot) print(p);
  
  p$xtra <- list(pct.outside=pct);
  return(p);
}