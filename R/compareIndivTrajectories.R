#'
<<<<<<< HEAD
#'@title UNFINISHED FUNCTION!!
=======
#'@title Compare individual trajectories
#'
#'@importFrom wtsUtilities getCSV
#'
#'@export
>>>>>>> 62a74cf16a4686e604456070f29fe0f6b451d39d
#'
compareIndivTrajectories<-function(dfr=NULL,
                                   indivConn=NULL,
                                   var=NULL,
                                   lhsTypeInfo=getLifeStageInfo.ATF(),
                                   onlySuccessfulIndivs=TRUE,
                                   nurseryAlongshoreZones=1:13,
                                   nurseryDepthZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), 
                                   spawningAlongshoreZones=1:12,
<<<<<<< HEAD
                                   spawningDepthZones=c("SpawningArea_300to600m")){
=======
                                   spawningDepthZones=c("SpawningArea_300to600m")
                                   ){
>>>>>>> 62a74cf16a4686e604456070f29fe0f6b451d39d
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
                          onlySuccessful=onlySuccessfulIndivs,
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
    indivsDFR<-sqldf(qry);
    
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
    dfr1<-sqldf(qry);
    xrng1<-range(dfr1$age,na.rm=TRUE,finite=TRUE);
    yrng1<-range(dfr1[[var]],na.rm=TRUE,finite=TRUE);
    
    #plot by spawning depth zones
    clrs<-c("blue","cyan")
    nNDZ<-length(nurseryDepthZones);
    for (spaz in spawningAlongshoreZones){
        for (spdz in spawningDepthZones){            
            plot(dfr1$age[idx],dfr1[[var]][idx],xlim=xrng1,ylim=yrng1,ylab=var,xlab='age',type='n');
            for (iNDZ in 1:nNDZ){
                idx<-(dfr1$spawningDepthZone==spdz)&
                     (dfr1$spawningAlongshoreZone==spaz)&
                     (dfr1$nurseryDepthZone==nurseryDepthZones[iNDZ]);
                points(ceiling(dfr1$age[idx])+0.2*(iNDZ-1),dfr1[[var]][idx],col=clrs[iNDZ])
            }
            mtext(paste(spaz),side=3,line=0,adj=0.05);
        }
    }
}