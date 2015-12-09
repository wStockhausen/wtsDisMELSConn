#'
#'@title Plot fraction remaining by age-in-stage.
#'
#'@description Function to plot fraction remaining by age-in-stage.
#'
#'@param dfr - dataframe or filename (or NULL) from call to extractIndivs(...)
#'@param subsets - list with subsets of alongshore spawning zones to include in plots
#'@param lhsTypeInfo - life stage info list for IBM (req'd if onlySuccessful=TRUE or onlyUnsuccessful=TRUE)
#'@param nurseryAlongshoreZones  - vector of id's for alongshore nursery zones to include
#'@param nurseryDepthZones       - vector of names for depth zones used as nursery areas in the IBM (req'd if onlySuccessful=TRUE)
#'@param spawningAlongshoreZones - vector of id's for alongshore spawning zones to include
#'@param spawningDepthZones      - vector of names for depth zones used as spawning areas in the IBM (req'd if onlySuccessful=TRUE)
#'@param title   - plot title
#'@param ggtheme - ggplot2 theme for plot
#'
#'@importFrom sqldf sqldf
#'@importFrom wtsUtilities getCSV
#'@import ggplot2
#' 
#'@export
#'
plotFractionRemaining<-function(dfr=NULL,
                                subsets=NULL,
                                lhsTypeInfo=getLifeStageInfo.ATF(),
                                nurseryAlongshoreZones=NULL,
                                nurseryDepthZones=NULL, 
                                spawningAlongshoreZones=NULL,
                                spawningDepthZones=NULL,
                                title=NULL,
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
    
    if (is.null(nurseryAlongshoreZones)) nurseryAlongshoreZones<-unique(dfr[["end_alongshorezone"]]);
    if (is.null(nurseryDepthZones))      nurseryDepthZones<-unique(dfr[["end_depthzone"]]);
    if (is.null(spawningAlongshoreZones)) spawningAlongshoreZones<-unique(dfr[["start_alongshorezone"]]);
    if (is.null(spawningDepthZones))      spawningDepthZones<-unique(dfr[["start_depthzone"]]);
    
    dfr$ageInStage<-round(dfr$ageInStage)#round to nearest unit
    cat('got here 1\n')
    
    #extract time series of var for each indiv by zones
    qry<-"select
            start_depthzone,
            start_alongshorezone,
            ageInStage,
            count(distinct id) as numIndivs
          from
            dfr
          where 
            start_depthzone      in ('&&start_depthzones') and
            start_alongshorezone in (&&start_alongshorezones) and
            end_depthzone        in ('&&end_depthzones') and
            end_alongshorezone   in (&&end_alongshorezones)
          group by
            start_depthzone,start_alongshorezone,ageInStage
          order by
            start_depthzone,start_alongshorezone,ageInStage;";
    qry<-gsub("&&start_depthzones",     paste(spawningDepthZones,     collapse="','"),qry);
    qry<-gsub("&&start_alongshorezones",paste(spawningAlongshoreZones,collapse=","),qry);
    qry<-gsub("&&end_depthzones",       paste(nurseryDepthZones,      collapse="','"),qry);
    qry<-gsub("&&end_alongshorezones",  paste(nurseryAlongshoreZones, collapse=","),qry);
    cat(qry,"\n");
    dfrp<-sqldf(qry);
    
    #normalize by numbers at 
    qry<-"select
            d.start_depthzone,
            d.start_alongshorezone,
            d.ageInStage,
            (1.0*d.numIndivs)/(1.0*z.numIndivs) as fraction
          from
            dfrp d,
            (select 
                p.start_depthzone as sdz, 
                p.start_alongshorezone as saz, 
                p.numIndivs
             from 
                dfrp p,
                (select 
                    start_depthzone, 
                    start_alongshorezone,
                    min(ageInStage) as minAge
                 from dfrp
                 group by start_depthzone, start_alongshorezone) m
             where 
                p.ageInStage=m.minAge and 
                p.start_depthzone=m.start_depthzone and
                p.start_alongshorezone=m.start_alongshorezone) z
          where
            d.start_depthzone=sdz and
            d.start_alongshorezone=saz
        order by
            d.start_depthzone,d.start_alongshorezone,d.ageInStage;";
    cat(qry,"\n");
    dfr1<-sqldf(qry);
    names(dfr1)<-c('start_depthzone','start_alongshorezone','ageInStage','fraction')
    xrng1<-range(dfr1$ageInStage,na.rm=TRUE,finite=TRUE);
    yrng1<-range(dfr1[['fraction']],na.rm=TRUE,finite=TRUE);
    
    if (is.null(subsets)) subsets<-list(`All GOA Spawning Zones`=c(unique(dfr1$start_alongshorezone)));

    ps<-list();
    for (subset in names(subsets)){
        cat("Plotting subset '",subset,"'\n",sep='')
        p <- ggplot(mapping=aes(x=ageInStage,y=fraction,
                                color=as.factor(start_alongshorezone)),
                    data=dfr1[dfr1$start_alongshorezone %in% subsets[[subset]],]);
        p <- p + geom_line(size=2);
        p <- p + guides(color=guide_legend(title="Alongshore \nSpawning Zone",
                                           ncol=2,alpha=1))
        p <- p + labs(x='age in stage (days)',y='fraction remaining in stage');
        if (!is.null(title)) p <- p + ggtitle(title);
        p <- p + ggtheme;
        print(p)
        ps[[subset]]<-p;
    }
    return(invisible(list(dfr=dfr,dfr.fracs=dfr1,ps=ps)))
}

# subsets<-list(`Eastern GOA Spawning Zones`=1:6,
#               `Western GOA Spawning Zones`=7:12)
# res<-plotFractionRemaining(dfr=NULL,
#                           subsets=subsets,
#                           lhsTypeInfo=getLifeStageInfo.ATF(),
#                           nurseryAlongshoreZones=NULL,
#                           nurseryDepthZones=NULL, 
#                           spawningAlongshoreZones=NULL,
#                           spawningDepthZones=NULL,
#                           ggtheme=theme_grey())
                                   
