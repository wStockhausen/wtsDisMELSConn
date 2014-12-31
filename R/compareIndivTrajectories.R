#'
#'@title Compare individual trajectories
#'
#'@param dfr - dataframe or filename (or NULL) from call to extractIndivs(...) (?)
#'@param indivConn        - individual connectivity file (or dataframe)
#'@param lhsTypeInfo      - life stage info list for IBM (req'd if onlySuccessful=TRUE or onlyUnsuccessful=TRUE)
#'@param onlySuccessful   - flag (T/F) to pull out only successful individuals
#'@param onlyUnsuccessful - flag (T/F) to pull out only unsuccessful individuals
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
                                   indivConn=NULL,
                                   var=NULL,
                                   lhsTypeInfo=getLifeStageInfo.ATF(),
                                   onlySuccessful=TRUE,
                                   onlyUnsuccessful=FALSE,
                                   nurseryAlongshoreZones=1:13,
                                   nurseryDepthZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), 
                                   spawningAlongshoreZones=1:12,
                                   spawningDepthZones=c("SpawningArea_300to600m")
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
    
    if (!is.data.frame(indivConn)){
        #read in individual connectivity results from csv file
        if (is.null(indivConn)){
            indivConn<-wtsUtilities::getCSV(caption='Select individual connectivity results file');
            if (is.null(indivConn)) return(NULL);#user aborted
        } else {
            indivConn<-read.csv(indivConn,stringsAsFactors=FALSE);
        }
    }
    
    #extract ids for indivs
    ids<-extractIndivIDs(indivConn=indivConn,
                          onlySuccessful=onlySuccessful,
                          onlyUnsuccessful=onlyUnsuccessful,
                          nurseryZones=nurseryDepthZones,
                          lhsTypeInfo=lhsTypeInfo);
    
    #extract age, variable of interest for indivs
    qry<-"select
            d.id,d.age,d.&&var
          from
            dfr d,
            ids i
          where
            i.id=d.id;";
    qry<-gsub("&&var",var,qry);
    cat(qry,"\n");
    indivsDFR<-sqldf::sqldf(qry);
    
    #extract time series of var for each indiv by zones
    qry<-"select
            start_depthzone as spawningDepthZone,
            start_alongshorezone as spawningAlongshoreZone,
            end_depthzone as nurseryDepthZone,
            end_alongshorezone as nurseryAlongshoreZone,
            d.id as id,
            age,
            &&var
          from
            indivsDFR d,
            (select 
                i.id,
                start_depthzone,
                start_alongshorezone,
                end_depthzone,
                end_alongshorezone
             from
                indivConn ic,
                ids i
             where
                ic.ID=i.ID and
                ic.start_depthzone      in ('&&start_depthzones') and
                ic.start_alongshorezone in (&&start_alongshorezones) and
                ic.end_depthzone        in ('&&end_depthzones') and
                ic.end_alongshorezone   in (&&end_alongshorezones)
            ) i1
            where 
              d.id=i1.id
            order by
              spawningDepthZone,spawningAlongshoreZone,
              nurseryDepthZone,nurseryAlongshoreZone,
              d.id,age;";
    qry<-gsub("&&var",var,qry);
    qry<-gsub("&&start_depthzones",paste(spawningDepthZones,collapse="','"),qry);
    qry<-gsub("&&start_alongshorezones",paste(spawningAlongshoreZones,collapse=","),qry);
    qry<-gsub("&&end_depthzones",paste(nurseryDepthZones,collapse="','"),qry);
    qry<-gsub("&&end_alongshorezones",paste(nurseryAlongshoreZones,collapse=","),qry);
    cat(qry,"\n");
    dfr1<-sqldf::sqldf(qry);
    xrng1<-range(dfr1$age,na.rm=TRUE,finite=TRUE);
    yrng1<-range(dfr1[[var]],na.rm=TRUE,finite=TRUE);
    
    #plot by spawning depth zones
    clrs<-c("blue","cyan")
    nNDZ<-length(nurseryDepthZones);
    for (spaz in spawningAlongshoreZones){
        for (spdz in spawningDepthZones){            
            plot(dfr1$age,dfr1[[var]],xlim=xrng1,ylim=yrng1,ylab=var,xlab='age',type='n');
            for (iNDZ in 1:nNDZ){
                idx<-(dfr1$spawningDepthZone==spdz)&
                     (dfr1$spawningAlongshoreZone==spaz)&
                     (dfr1$nurseryDepthZone==nurseryDepthZones[iNDZ]);
                points(ceiling(dfr1$age[idx])+0.2*(iNDZ-1),dfr1[[var]][idx],col=clrs[iNDZ])
            }
            mtext(paste(spaz),side=3,line=0,adj=0.05);
        }
    }
    
    p <- ggplot(mapping=aes_string(x="age",y=var,fill="nurseryAlongshoreZone",shape="nurseryDepthZone"),
                data=dfr1);
    p <- p + geom_point(position='jitter')
    p <- p + geom_boxplot()
    p <- p + facet_wrap(spawningDepthZone,nrow=2)
    print(p)
}

# compareIndivTrajectories(dfr=NULL,
#                            indivConn=NULL,
#                            var='temperature',
#                            lhsTypeInfo=getLifeStageInfo.ATF(),
#                            onlySuccessful=TRUE,
#                            onlyUnsuccessful=FALSE,
#                            nurseryAlongshoreZones=1:13,
#                            nurseryDepthZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), 
#                            spawningAlongshoreZones=1:12,
#                            spawningDepthZones=c("SpawningArea_300to600m"))
                                   
