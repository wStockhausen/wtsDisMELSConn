#'
#'@title Calculate linear regressions for time series in Y as function of time series in X.
#'
#'@description Function to calculate linear regressions between time series in Y as function of time series in X.
#'
#'@param mdfrX - melted dataframe with independent (X) time series
#'@param mdfrY - melted dataframe with dependent (Y) time series
#'@param verbose - flag to print messages
#'
#'@details Requires packages 'plyr' and 'reshape2'. Datasets are standardized as z-scores, 
#'so the constant term in the linear model is dropped.
#'
#'@return list with named elements 'res', 'summary' and 'plot': \cr
#'\itemize{
#'  \item res - a list with sublists by unique YG:YV:XG:XV with elements
#'  \itemize{
#'    \item lms     - list with linear model (lm) results by Y for X
#'    \item summary - dataframe with summaries of linear model results
#'  }
#'  \item summary - a dataframe summarizing all LR results. Columns are
#'  \itemize{
#'    \item ygroup - dependent variable group label
#'    \item y      - dependent variable label
#'    \item xgroup - independent variable group label
#'    \item x      - independent variable label
#'    \item n      - number of valid data points
#'    \item rho    - Pearson's correlation coefficient
#'    \item rsq    - R^2 for linear fit
#'    \item adj.rsq - adjusted R^2 for linear fit
#'    \item F      - F statistic
#'    \item prF    - Pr(>F) (p-value uncorrected for multiple comparisons)
#'    \item aicc   - AICc value
#'  }
#'  \item data - dataframe with data involved in linear models
#'  \itemize{
#'    \item ygroup - dependent variable group label
#'    \item yvar   - dependent variable label
#'    \item xgroup - independent variable group label
#'    \item xvar   - independent variable label
#'    \item year   - year
#'    \item x      - independent value in LR
#'    \item y      - dependent value in LR
#'  }
#'}
#'
#'@import wtsUtilities
#'
#'@export
#'
calcLRs.YsByXs<-function(mdfrX,
                         mdfrY,
                         verbose=FALSE){
#   require(plyr);
#   require(reshape2);
#   require(wtsUtilities);
  
  ##standardize independent (X) values by variable, group across years
  if (!('group' %in% names(mdfrX))) mdfrX$group<-"";#add group column
  mdfrZXs<-plyr::ddply(mdfrX,.variables=c('group','variable'),.fun=standardize,col='value',.inform=TRUE);
  
  ##standardize dependent (Y) values by variable, group across years
  if (!('group' %in% names(mdfrY))) mdfrY$group<-"";#add group column
  mdfrZYs<-plyr::ddply(mdfrY,.variables=c('group','variable'),.fun=standardize,col='value',.inform=TRUE);

  uXGs<-as.character(unique(mdfrZXs[["group"]]));
  uXVs<-as.character(unique(mdfrZXs[["variable"]]));
  uYGs<-as.character(unique(mdfrZYs[["group"]]));
  uYVs<-as.character(unique(mdfrZYs[["variable"]]));
  if (verbose){
    cat("Starting calcLRs.YsByXs(...)\n")
    cat("---\n")
    cat("uXGs = \n",paste0("---'",uXGs,"'\n"));
    cat("uXVs = \n",paste0("---'",uXVs,"'\n"));
    cat("uYGs = \n",paste0("---'",uYGs,"'\n"));
    cat("uYVs = \n",paste0("---'",uYVs,"'\n"));
    cat("---\n")
  }

  #define objects for output
  lm.res<-list();#list of lm results
  lm.sums<-NULL;#dataframe with summaries of lm results
  mYsOnXs<-NULL; #dataframe with data for linear fits
  for (uYG in uYGs){
    if (verbose) cat("--\nProcessing dependent (Y) group '",uYG,"'\n",sep='');
    for (uYV in uYVs){
      if (verbose) cat("--\nProcessing dependent (Y) variable '",uYV,"'\n",sep='');
      ##extract Y data for uYG, uYV combination
      idy<-(mdfrZYs$group==uYG)&(mdfrZYs$variable==uYV);
      mdfrZY<-mdfrZYs[idy,];
      if (is.null(mdfrZY)||(nrow(mdfrZY)==0)){
        cat("skipping All MODELS for Y-group = '",uYG,"'/'",uXV,"', Y-variable = '",uYV,"'\n",sep='');
      } else {
        if (uYV=='') mdfrZY$variable<-'y';#replace '' with 'y'
        ##process by unique X group variables
        for (uXG in uXGs){ 
          if (verbose) cat("--Processing independent (X) group '",uXG,"'\n",sep='');
          ##do linear model analysis by X-value
          for (uXV in uXVs){
            if (verbose) cat("----Assessing independent (X) variable '",uXV,"'\n",sep='');
            idx<-(mdfrZXs$group==uXG)&(mdfrZXs$variable==uXV);
            mdfrZX<-mdfrZXs[idx,];
            if (is.null(mdfrZX)||(nrow(mdfrZX)==0)){
              cat("------skipping model for X = '",uXG,"'/'",uXV,"', Y = '",uYG,"'/'",uYV,"'\n",sep='');
            } else {
              if (uXV=='') mdfrZX$variable<-'x';#replace '' with 'x'
              cols<-c("year","variable","value");
              mdfrYX<-rbind(mdfrZY[,cols],mdfrZX[,cols]);
              dfrYX<-reshape2::dcast(mdfrYX,year~variable,fun.aggregate=sum,value.var='value');
              dfrYX<-dfrYX[,c("year",ifelse(uXV=='','x',uXV),ifelse(uYV=='','y',uYV))];#re-order columns into canonical (x,y) order
              names(dfrYX)<-c("year","x","y");                                         #rename columns to canonical form
              tst<-sum(abs(dfrYX$x),na.rm=TRUE)*sum(abs(dfrYX$y),na.rm=TRUE);
              if(is.na(tst)||(tst==0)){
                  cat("skipping model for X = '",uXG,"'/'",uXV,"', Y = '",uYG,"/",uYV,"'\n",sep='');
              } else {
                cat("------running model for X = '",uXG,"'/'",uXV,"', Y = '",uYG,"/",uYV,"'\n",sep='');
                lmp<-lm(y~x-1,dfrYX);
                smr<-summary(lmp);
                ant<-anova(lmp);
                ##add lm results to lm.res
                gv<-paste(uYG,uYV,uXG,uXV,sep=":");
                lm.res[[gv]]<-list(ygroup=uYG,yvar=uYV,xgroup=uXG,xvar=uXV,lm=lmp,data=dfrYX);
                ##add to summary results to sum.vars
                lm.sums<-rbind(lm.sums,data.frame(ygroup=uYG,
                                                  yvar=uYV,
                                                  xgroup=uXG,
                                                  xvar=uXV,
                                                  n=sum((!is.na(dfrYX$x))&(!is.na(dfrYX$y))),
                                                  rho=smr$coefficients[1,"Estimate"],
                                                  rsq=smr$r.squared,
                                                  adj.rsq=smr$adj.r.squared,
                                                  F=ant[1,"F value"],
                                                  prF=ant[1,"Pr(>F)"],
                                                  aicc=aicc(lmp)));
                ##add data to mYsOnXs
                dfrYX$ygroup<-uYG;
                dfrYX$yvar  <-uYV;
                dfrYX$xgroup<-uXG;
                dfrYX$xvar  <-uXV;
                mYsOnXs<-rbind(mYsOnXs,dfrYX[,c("ygroup","yvar","xgroup","xvar","year","x","y")]);
              }#tst check
            }#mdfrZX check
          }#uXVs loop
        }#uXGs loop
      }##uYG and uYV combination has data
    }##uYVs
  }##uYGs
  
  #make sure character columns ARE character columns, not factors
  lm.sums$ygroup<-as.character(lm.sums$ygroup);
  lm.sums$yvar  <-as.character(lm.sums$yvar);
  lm.sums$xgroup<-as.character(lm.sums$xgroup);
  lm.sums$xvar  <-as.character(lm.sums$xvar);
  mYsOnXs$ygroup<-as.character(mYsOnXs$ygroup);
  mYsOnXs$yvar  <-as.character(mYsOnXs$yvar);
  mYsOnXs$xgroup<-as.character(mYsOnXs$xgroup);
  mYsOnXs$xvar  <-as.character(mYsOnXs$xvar);
  
  if (verbose) cat("Finished calcLRs.YsByXs(...)\n");
  return(invisible(list(res=lm.res,summary=lm.sums,data=mYsOnXs)));
}
