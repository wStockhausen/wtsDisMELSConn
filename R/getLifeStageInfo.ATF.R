#'
#'@title Get a list that defines life stage info for the DisMELS arrowtooth flounder IBM.
#'
#'@description Function to get a list that defines life stage info for the DisMELS arrowtooth flounder IBM.
#'
#'@return a list
#'
#'@details none
#'
#'@export
#'
getLifeStageInfo.ATF<-function(){
    #results file type
    resType='NEW';
    
    #java class names
    classNames<-c('EggStage','LarvaStage','SettlerStage','BenthicJuvenileStage');
    
    #names of 'additional attributes' variables for each life stage class
    EggClassInfo     <-c('devStage','diameter','density','temperature','salinity','rho');
    LarvaClassInfo   <-c('size','weight','temperature','salinity');
    SettlerClassInfo <-c('size','weight','temperature','salinity');
    JuvenileClassInfo<-c('size','weight','temperature','salinity');
    
    #data types
    EggClassDataTypes      <- rep('numeric',times=6)
    LarvaClassDataTypes    <- rep('numeric',times=4)
    SettlerClassDataTypes  <- rep('numeric',times=4)
    JuvenileClassDataTypes <- rep('numeric',times=4)
    
    #class info
    info.classes<-list(EggStage            =list(vars=EggClassInfo,     types=EggClassDataTypes),
                       LarvaStage          =list(vars=LarvaClassInfo,   types=LarvaClassDataTypes),
                       SettlerStage        =list(vars=SettlerClassInfo, types=SettlerClassDataTypes),
                       BenthicJuvenileStage=list(vars=JuvenileClassInfo,types=JuvenileClassDataTypes))
    
    
    #defined life stage type names (NOT class names)
    lifeStageTypes=list(egg01=                         list(class=classNames[1],info=info.classes[[1]]),
                        small.yolk.sac.larva=          list(class=classNames[2],info=info.classes[[2]]),
                        large.yolk.sac.larva=          list(class=classNames[2],info=info.classes[[2]]),
                        small.feeding.preflexion.larva=list(class=classNames[2],info=info.classes[[2]]),
                        large.feeding.preflexion.larva=list(class=classNames[2],info=info.classes[[2]]),
                        postflexion.larva=             list(class=classNames[2],info=info.classes[[2]]),
                        settlement.stage.larva=        list(class=classNames[3],info=info.classes[[3]]),
                        benthic.juvenile=              list(class=classNames[4],info=info.classes[[4]]));
    
    return(invisible(list(resType=resType,classNames=classNames,classInfo=info.classes,lifeStageTypes=lifeStageTypes)));
}