#'
#'@title Compare individual trajectories
#'
#'@param dfr - dataframe or filename (or NULL) from call to extractIndivs(...) (?)
#'@param var - name of variable (column) to plot
#'@param lhsTypeInfo - life stage info list for IBM (req'd if onlySuccessful=TRUE or onlyUnsuccessful=TRUE)
#'@param lhsTypes - vector of lhs types to process (NULL = all)
#'@param nurseryAlongshoreZones  - alongshore id's for nursery zones to include
#'@param nurseryDepthZones       - vector of names of depth zones used as nursery areas in the IBM (req'd if onlySuccessful=TRUE)
#'@param spawningAlongshoreZones - alongshore id's for nursery zones to include
#'@param spawningDepthZones      - vector of names of zones used as spawning areas in the IBM (req'd if onlySuccessful=TRUE)
#'
#'@importFrom sqldf sqldf
#'@importFrom wtsUtilities getCSV
#'@import ggplot2
#' 
#'@export
#'
compareIndivTrajectories<-function(dfr=NULL,
                                   var=NULL,
                                   lhsTypeInfo=getLifeStageInfo.ATF(),
                                   nurseryAlongshoreZones=NULL,
                                   nurseryDepthZones=NULL, 
                                   spawningAlongshoreZones=NULL,
                                   spawningDepthZones=NULL
                                   ){
    if (!is.data.frame(dfr)){
        #read in extracted individuals csv file
        if (is.null(dfr)){
            dfr<-wtsUtilities::getCSV(caption='Select extracted individuals results file');
            if (is.null(dfr)) return(NULL);#user aborted
        } else {
            dfr<-read.csv(dfr,stringsAsFactors=FALSE);
        }
    }
    
    if (is.null(nurseryAlongshoreZones)) nurseryAlongshoreZones<-unique(dfr[["end_alongshorezone"]]);
    if (is.null(nurseryDepthZones))      nurseryDepthZones<-unique(dfr[["end_depthzone"]]);
    if (is.null(spawningAlongshoreZones)) spawningAlongshoreZones<-unique(dfr[["start_alongshorezone"]]);
    if (is.null(spawningDepthZones))      spawningDepthZones<-unique(dfr[["start_depthzone"]]);
    
    #extract time series of var for each indiv by zones
    qry<-"select
            start_depthzone as spawningDepthZone,
            start_alongshorezone as spawningAlongshoreZone,
            end_depthzone as nurseryDepthZone,
            end_alongshorezone as nurseryAlongshoreZone,
            id,
            age,
            &&var
          from
            dfr
          where 
            start_depthzone      in ('&&start_depthzones') and
            start_alongshorezone in (&&start_alongshorezones) and
            end_depthzone        in ('&&end_depthzones') and
            end_alongshorezone   in (&&end_alongshorezones)
          order by
            spawningDepthZone,spawningAlongshoreZone,
            nurseryDepthZone,nurseryAlongshoreZone,
            id,age;";
    qry<-gsub("&&var",var,qry);
    qry<-gsub("&&start_depthzones",     paste(spawningDepthZones,     collapse="','"),qry);
    qry<-gsub("&&start_alongshorezones",paste(spawningAlongshoreZones,collapse=","),qry);
    qry<-gsub("&&end_depthzones",       paste(nurseryDepthZones,      collapse="','"),qry);
    qry<-gsub("&&end_alongshorezones",  paste(nurseryAlongshoreZones, collapse=","),qry);
    cat(qry,"\n");
    dfr<-sqldf::sqldf(qry);
    dfr$agep<-as.factor(as.integer(round(dfr$age)));
    dfr$nurseryAlongshoreZone<-as.factor(as.integer(dfr$nurseryAlongshoreZone))
    dfr$spawningAlongshoreZone<-as.factor(as.integer(dfr$spawningAlongshoreZone))
    xrng1<-range(dfr$age,na.rm=TRUE,finite=TRUE);
    yrng1<-range(dfr[[var]],na.rm=TRUE,finite=TRUE);
    
    p <- ggplot(mapping=aes_string(x="agep",y=var,
                                   color="spawningAlongshoreZone"),
                data=dfr);
    p <- p + geom_point(position='jitter',alpha=0.2)
#    p <- p + geom_boxplot()
    p <- p + labs(x='age (days)',y=bquote(temperature~degree*C))
#    p <- p + facet_wrap(~spawningAlongshoreZone,nrow=2)
    print(p)
}

# compareIndivTrajectories(dfr=NULL,
#                            var='temperature',
#                            lhsTypeInfo=getLifeStageInfo.ATF(),
#                            nurseryAlongshoreZones=1:13,
#                            nurseryDepthZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), 
#                            spawningAlongshoreZones=1:12,
#                            spawningDepthZones=c("SpawningArea_300to600m"))
                                   
