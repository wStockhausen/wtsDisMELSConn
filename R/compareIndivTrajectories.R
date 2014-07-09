compareIndivTrajectories<-function(dfr=NULL,
                                   indivConn=NULL,
                                   var=NULL,
                                   onlySuccessfulIndivs=TRUE,
                                   nurseryAlongshoreZones=1:13,
                                   nurseryDepthZones=c("NurseryArea_000to050m","NurseryArea_050to150m"), 
                                   spawningAlongshoreZones=1:12,
                                   spawningDepthZones=c("SpawningArea_300to600m"),
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
    
    #define zones to incorporate
    nurseryDepthZones<-as.data.frame(list(zone=nurseryDepthZones));    
    nurseryAlongshoreZones<-as.data.frame(list(zone=nurseryAlongshoreZones));
    
    spawningDepthZones<-as.data.frame(list(zone=spawningDepthZones));    
    spawningAlongshoreZones<-as.data.frame(list(zone=spawningAlongshoreZones));
    
    qry<-"select 
            sd.zone as spawningDepthZone,
            sa.zone as spawningAlongshoreZone,
            nd.zone as nurseryDepthZone,
            na.zone as nurseryAlongshoreZone
          from
            spawningDepthZones as sd,
            spawningAlongshoreZones as sa,
            nurseryDepthZones nd,
            nurseryAlongshoreZone as na;";
    zones<-sqldf(qry);
    
    #extract ids for indivs
    ids<-extractIndivIDs(indivConn=indivConn,
                          onlySuccessful=onlySuccessfulIndivs,
                          nurseryZones=nurseryZones,
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
    nr<-nrow(zones);
    for (r in 1:nr){
        qry<-"select
                d.id,d.age,d.&&var
              from
                indivsDFR d,
                (select 
                    id
                 from
                    indivConn ic,
                    ids i
                 where
                    i.ID=ic.ID and
                    i.start_depthzone=&&start_depthzone and
                    i.start_alongshorezone=&&start_alongshorezone and
                    i.start_depthzone=&&start_depthzone and
                    i.start_alongshorezone=&&start_alongshorezone
                ) i1
                where 
                  d.id=i1.id
                order by
                  id,age;";
        qry<-gsub("&&var",var,qry);
        qry<-gsub("&&start_depthzone",zones$spawningDepthZone,qry);
        qry<-gsub("&&start_alongshorezone",zones$spawningAlongShoreZone,qry);
        cat(qry,"\n");
        dfr1<-sqldf(qry);
    }
}