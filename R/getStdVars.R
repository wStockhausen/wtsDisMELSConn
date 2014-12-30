#'
#'@title Get standard variable names for DisMELS output.
#'
#'@description Function to get standard variable names for DisMELS output.
#'
#'@param newResType - flag (T/F) indicating if results are based on the new or old DisMELS results format.
#'
#'@return character vector with names of standard variables
#'
#'@export
#'
getStdVars<-function(newResType=TRUE){
    if (newResType){
          stdVarsAll<-c('typeName','id','parentID','origID','startTime','time',
                        'horizType','vertType','horizPos1','horizPos2','vertPos','gridCellID','track',
                        'active','alive','attached','age','ageInStage','number');
    } else {
          stdVarsAll<-c('typeName','id','parentID','origID','horizType','vertType','active','alive','attached',
                        'startTime','time','age','ageInStage','size','number','horizPos1','horizPos2','vertPos',
                        'temp','salinity','gridCellID','track');
    }
    return(stdVarsAll);
}