#'
#'@title Get standard variable names for DisMELS output
#'
#'@description Function to get standard variable names for DisMELS output.
#'
#'@param newResType - flag ("NEW" or "OLD") indicating if results are based on the new or old DisMELS results format.
#'
#'@return data frame with columns for names of standard variables ('vars') and types ('types)
#'
#'@details none
#'
#'@export
#'
getStdVars<-function(newResType){
    if (toupper(newResType)=='NEW'){
          stdVarsAll<-c('typeName','id','parentID','origID','startTime','time',
                        'horizType','vertType','horizPos1','horizPos2','vertPos','gridCellID','track',
                        'active','alive','attached','age','ageInStage','number');
          type<-c('character','integer','integer','integer','character','character',
                  'integer','integer','numeric','numeric','numeric','character','character',
                  'character','character','character','numeric','numeric','numeric')
    } else {
          stdVarsAll<-c('typeName','id','parentID','origID','horizType','vertType',
                        'active','alive','attached','startTime','time',
                        'age','ageInStage','size','number','horizPos1','horizPos2','vertPos',
                        'temp','salinity','gridCellID','track');
          type<-c('character','integer','integer','integer','integer','integer',
                  'character','character','character','character','character',
                  'numeric','numeric','numeric','numeric','numeric','numeric','numeric',
                  'numeric','numeric','character','character');
    }
    return(data.frame(list(vars=stdVarsAll,types=type),stringsAsFactors=FALSE));
}