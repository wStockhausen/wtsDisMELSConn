#'
#'@title Plot dataframe with connectivity info aggregated over sources or sinks as time series and boxplots
#'
#'@description Function to plot dataframe with connectivity info aggregated over sources or sinks as time series and boxplots.
#'
#'@param dfr - dataframe with conncetivity info aggregated over sources (spawning areas) or sinks (nursery areas)
#'@param time - name of column associated with times
#'@param area - name fo column associated with areas
#'@param y - name of column to plot as y axis
#'@param timeLab - label for times
#'@param areaLab - label for areas
#'@param yLab - label for y axis
#'@param facets - formula to facet by
#'
#'@return list with ggplot2 elements pTS (time series) and pBP (boxplot)
#'
#'@details none.
#'
#'@import ggplot2
#'
#'@export
#'
plotConnInfoByArea<-function(dfr,
                             time='year',
                             area='spawning area',
                             y='Pr(.|S)',
                             timeLab=time,
                             areaLab=area,
                             yLab=y,
                             facets=EW~.){
    #reformat dataframe
    dfrp<-dfr;
    if (is.numeric(dfrp[[area]])) dfrp[[area]]<-formatZeros(dfrp[[area]]);
    #time series plot
    pTS <- ggplot(dfrp,mapping=aes_string(x=time,y=y,colour=area,group=area));
    pTS <- pTS + geom_line(size=1.25);
    if (!is.null(facets)) pTS <- pTS + facet_grid(facets);
    pTS <- pTS + xlab(timeLab);
    pTS <- pTS + ylab(yLab);
    #print(pTS);
    
    #box plot
    pBP <- ggplot(dfrp,mapping=aes_string(x=area,y=y));
    pBP <- pBP + geom_boxplot();
    pBP <- pBP + xlab(areaLab);
    pBP <- pBP + ylab(yLab);
    #print(pBP); 
    
    return(invisible(list(pTS=pTS,pBP=pBP)));
}
# 
# plotAggCon(dfrNumBySrc,
#            time='date',timeLab='year',
#            area='start_alongshorezone',areaLab='spawning area',
#            y='pSrv',yLab='Pr(successful|S)',
#            facets=EW~.)
