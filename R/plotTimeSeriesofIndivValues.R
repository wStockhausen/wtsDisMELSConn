#'
#'@title Plot time series of a variable for individuals.
#'
#'@description Funcrtion to plot time series of a variable for individuals.
#'
#'@param dfr - dataframe or filename (or NULL) from call to extractIndivs(...)
#'@param vars - name(s) of variable(s) (columns) to plot
#'@param by - name of x-axis variable to plot by
#'@param lhsTypeInfo - life stage info list for IBM (req'd if onlySuccessful=TRUE or onlyUnsuccessful=TRUE)
#'@param nurseryAlongshoreZones  - alongshore id's for nursery zones to include
#'@param nurseryDepthZones       - vector of names of depth zones used as nursery areas in the IBM (req'd if onlySuccessful=TRUE)
#'@param spawningAlongshoreZones - alongshore id's for nursery zones to include
#'@param spawningDepthZones      - vector of names of zones used as spawning areas in the IBM (req'd if onlySuccessful=TRUE)
#'@param xlab   - x axis label
#'@param ylabs  - y axis labels (character vector or list of expressions/character vectors)
#'@param titles - plot titles (character vector or list of expressions/character vectors)
#'@param subsets - list with subsets of alongshore spawning zones to include in plots
#'@param facets - expression for creating facets (use bquote(...))
#'@param ggtheme - ggplot2 theme for plot
#'
#'@importFrom sqldf sqldf
#'@importFrom wtsUtilities getCSV
#'@import ggplot2
#' 
#'@export
#'
plotTimeSeriesofIndivValues<-function(dfr=NULL,
                                   vars=NULL,
                                   by='age',
                                   lhsTypeInfo=getLifeStageInfo.ATF(),
                                   nurseryAlongshoreZones=NULL,
                                   nurseryDepthZones=NULL, 
                                   spawningAlongshoreZones=NULL,
                                   spawningDepthZones=NULL,
                                   xlab=NULL,
                                   ylabs=NULL,
                                   titles=NULL,
                                   subsets=NULL,
                                   facets=NULL,
                                   ggtheme=theme_grey()
                                   ){
    if (!is.data.frame(dfr)){
        #read in extracted individuals csv file
        if (is.null(dfr)){
            dfr<-getCSV(caption='Select extracted individuals results file');
            if (is.null(dfr)) return(NULL);#user aborted
        } else {
            dfr<-read.csv(dfr,stringsAsFactors=FALSE);
        }
    }
    
    dfr$vertPos<- -dfr$vertPos;#make depths negative for plots
    
    if (is.null(nurseryAlongshoreZones)) nurseryAlongshoreZones<-unique(dfr[["end_alongshorezone"]]);
    if (is.null(nurseryDepthZones))      nurseryDepthZones<-unique(dfr[["end_depthzone"]]);
    if (is.null(spawningAlongshoreZones)) spawningAlongshoreZones<-unique(dfr[["start_alongshorezone"]]);
    if (is.null(spawningDepthZones))      spawningDepthZones<-unique(dfr[["start_depthzone"]]);
    
    #extract time series of var for each indiv by zones
    qry<-"select
            start_depthzone,
            start_alongshorezone,
            end_depthzone,
            end_alongshorezone,
            id,
            &&by,
            &&vars
          from
            dfr
          where 
            start_depthzone      in ('&&start_depthzones') and
            start_alongshorezone in (&&start_alongshorezones) and
            end_depthzone        in ('&&end_depthzones') and
            end_alongshorezone   in (&&end_alongshorezones)
          order by
            start_depthzone,start_alongshorezone,
            end_depthzone,end_alongshorezone,
            id,age;";
    qry<-gsub("&&by",by,qry);
    qry<-gsub("&&vars",paste(vars,collapse=','),qry);
    qry<-gsub("&&start_depthzones",     paste(spawningDepthZones,     collapse="','"),qry);
    qry<-gsub("&&start_alongshorezones",paste(spawningAlongshoreZones,collapse=","),qry);
    qry<-gsub("&&end_depthzones",       paste(nurseryDepthZones,      collapse="','"),qry);
    qry<-gsub("&&end_alongshorezones",  paste(nurseryAlongshoreZones, collapse=","),qry);
    cat(qry,"\n");
    dfr<-sqldf(qry);
    dfr$xp<-as.factor(as.integer(round(dfr[[by]])));
    dfr$end_alongshorezone<-as.factor(as.integer(dfr$end_alongshorezone))
    dfr$start_alongshorezone<-as.factor(as.integer(dfr$start_alongshorezone))
#    xrng1<-range(dfr$xp,na.rm=TRUE,finite=TRUE);
    
    q10 <- seq(0.05, 0.95, by=0.05)
    
    if (is.null(ylabs)) ylabs<-vars;
    names(ylabs)<-vars;
    
    if (length(titles)<length(vars)){
        titles<-rep(titles,length.out=length(vars));
    }
    names(titles)<-vars;

    if (is.null(subsets)) subsets<-list(`All GOA Spawning Zones`=c(unique(dfr$start_alongshorezone)));

    pvs<-list();
    for (var in vars){
        cat("Plotting variable '",var,"'\n",sep='')
        ylab<-ylabs[var];
        if (is.list(ylab)) ylab<-ylab[[1]];
        title<-titles[var];
        if (is.list(title)) title<-title[[1]];
        ps<-list();
        yrng1<-range(dfr[[var]],na.rm=TRUE,finite=TRUE);
        for (subset in names(subsets)){
            cat("\tPlotting subset '",subset,"'\n",sep='')
            p <- ggplot(mapping=aes_string(x="xp",y=var,
                                           color="start_alongshorezone"),
                        data=dfr[dfr$start_alongshorezone %in% subsets[[subset]],]);
            p <- p + geom_point(position='jitter',alpha=0.3,size=2);
            p <- p + guides(color=guide_legend(title="Alongshore\nSpawning Zone",ncol=2,
                                               override.aes=list(size=4,alpha=1)));
            p <- p + xlab(xlab);
            p <- p + ylab(ylab);
            p <- p + ylim(yrng1);
        #     p <- p + stat_quantile(aes(x=age,colour=..quantile..),quantiles=q10) +
        #              scale_colour_gradient2(midpoint=0.5);
            if (!is.null(facets)) p <- p + facet_wrap(facets,nrow=2);
            if (!is.null(title))  p <- p + ggtitle(title);
            print(p)
            ps[[subset]]<-p;
        }#subset
        pvs[[var]]<-ps;
    }#var
    return(invisible(pvs));
}

# plotTimeSeriesofIndivValues(dfr=res.fracs.s$dfr,
#                            vars=c('temperature','vertPos'),
#                            by='age',
#                            xlab='age (days)',
#                            ylabs=c(quote(temperature~degree*C),'depth (m)'),
#                            title='1997: Survivors',
#                            lhsTypeInfo=getLifeStageInfo.ATF())
#                                    
# plotTimeSeriesofIndivValues(dfr=res.fracs.u$dfr,
#                            vars=c('temperature','vertPos'),
#                            by='age',
#                            xlab='age (days)',
#                            ylabs=c(quote(temperature~degree*C),'depth (m)'),
#                            title='1997: Diers',
#                            lhsTypeInfo=getLifeStageInfo.ATF())
                                   
