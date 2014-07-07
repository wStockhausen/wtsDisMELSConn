#'
#'@title Get a list that defines life stage info for the DisMELS arrowtooth flounder IBM.
#'#
#'#export
#'
getLifeStageInfo.ATF<-function(){
    #java class names
    classNames<-c('EggStage','LarvaStage','SettlerStage','BenthicJuvenileStage');
    
    #names of 'additional attributes' variables for each life stage class
    EggClassInfo     <-c('devStage','diameter','density','temperature','salinity','rho');
    LarvaClassInfo   <-c('size','weight','temperature','salinity');
    SettlerClassInfo <-c('size','weight','temperature','salinity');
    JuvenileClassInfo<-c('size','weight','temperature','salinity');
    
    #class info
    info.classes<-list(EggStage=EggClassInfo,
                       LarvaStage=LarvaClassInfo,
                       SettlerStage=SettlerClassInfo,
                       BenthicJuvenileStage=JuvenileClassInfo)
    
    
    #defined life stage type names (NOT class names)
    lifeStageTypes=list(egg01=                         list(class=classNames[1],info=info.classes[[1]]),
                        small.yolk.sac.larva=          list(class=classNames[2],info=info.classes[[2]]),
                        large.yolk.sac.larva=          list(class=classNames[2],info=info.classes[[2]]),
                        small.feeding.preflexion.larva=list(class=classNames[2],info=info.classes[[2]]),
                        large.feeding.preflexion.larva=list(class=classNames[2],info=info.classes[[2]]),
                        postflexion.larva=             list(class=classNames[2],info=info.classes[[2]]),
                        settlement.stage.larva=        list(class=classNames[3],info=info.classes[[3]]),
                        benthic.juvenile=              list(class=classNames[4],info=info.classes[[4]]));
    
    return(invisible(list(classNames=classNames,classInfo=info.classes,lifeStageTypes=lifeStageTypes)));
}