#'
#'@title Get a list that defines life stage info for the DisMELS POP IBM.
#'#
#'#export
#'
getLifeStageInfo.POP<-function(){
    #java class names
    classNames<-c('SimplePelagicLHS','SimpleSettlerLHS','SimpleBenthicJuvenileLHS');
    
    #names of 'additional attributes' variables for each life stage class
    PelagicClassInfo <-c('size','temperature','salinity');
    SettlerClassInfo <-c('size','temperature','salinity');
    JuvenileClassInfo<-c('size','temperature','salinity');
    
    #class info
    info.classes<-list(SimplePelagicLHS=PelagicClassInfo,
                       SimpleSettlerLHS=SettlerClassInfo,
                       SimpleBenthicJuvenileLHS=JuvenileClassInfo)
    
    
    #defined life stage type names (NOT class names)
    lifeStageTypes=list(preflexion.larva =list(class=classNames[1],info=info.classes[[1]]),
                        postflexion.larva=list(class=classNames[1],info=info.classes[[1]]),
                        pelagic.juvenile =list(class=classNames[1],info=info.classes[[1]]),
                        settler          =list(class=classNames[2],info=info.classes[[2]]),
                        benthic.juvenile =list(class=classNames[3],info=info.classes[[3]]));
    
    return(invisible(list(classNames=classNames,classInfo=info.classes,lifeStageTypes=lifeStageTypes)));
}