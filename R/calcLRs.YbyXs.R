#'
#'@title Calculate linear regressions for time series in Y as function of time series in X.
#'
#'@description Function to calculate linear regressions between time series in Y as function of time series in X.
#'
#'@param mdfrX - melted dataframe with independent (X) time series
#'@param mdfrY - melted dataframe with dependent (Y) time series
#'@param yvars - vector of dependent variables to process (or NULL for all) 
#'@param xlab - label associated with independent variables
#'@param ylab - label associated with dependent variables
#'@param nrows - number of rows for LR plots
#'@param coord_fixed - flag to use equal x/y unit dimensions in LR plots
#'@param labelByGroup - flag to use "group" column to organize linear regression analysis
#'@param verbose - flag to print messages
#'
#'@return list with named elements 'res', 'summary' and 'plot': \cr
#'\itemize{
#'  \item res - a list with sublists by unique Y with elements
#'  \itemize{
#'    \item lms     - list with linear model (lm) results by Y for X
#'    \item summary - dataframe with summaries of linear model results
#'    \item plots   - list of ggplot2 objects (p1=standardized time series, p2=fits to lm's)
#'    \item data    - dataframe with data involved in linear models
#'  }
#'  \item summary - a dataframe summarizing all LR results. Columns are
#'  \itemize{
#'    \item ygroup - dependent variable group label
#'    \item y      - dependent variable label
#'    \item xgroup - independent variable group label
#'    \item x      - independent variable label
#'    \item rho    - Pearson's correlation coefficient
#'    \item rsq    - R-squared for linear fit
#'    \item p      - P-value (uncorrected for multiple comparisons)
#'  }
#'  \item plot - a ggplot object.
#'}
#'
#'@import ggplot2
#'@import plyr
#'@import reshape2
#'@import wtsUtilities
#'
#'@export
#'
calcLRs.YbyXs<-function(mdfrX,
                        mdfrY,
                        yvars=NULL,
                        xlab='index',
                        ylab='index',
                        nrows=2,
                        coord_fixed=FALSE,
                        labelByGroup=FALSE,
                        verbose=FALSE){
#   require(ggplot2);
#   require(plyr);
#   require(reshape2);
#   require(wtsUtilities);
  
  #define objects for output
  res<-list();#list of individual LR results and plot objects
##  data<-NULL; #dataframe for data involved in LR results
  sums<-NULL; #dataframe for summary of LR results
  
  ##standardize independent (X) values by variable, group across years
  if (!('group' %in% names(mdfrX))) mdfrX$group<-"";#add group column
  mdfrZXs<-ddply(mdfrX,.variables=c('group','variable'),.fun=standardize,col='value',.inform=TRUE);
  
  ##standardize dependent (Y) values by variable, group across years
  if (!('group' %in% names(mdfrY))) mdfrY$group<-"";#add group column
  mdfrZYs<-ddply(mdfrY,.variables=c('group','variable'),.fun=standardize,col='value',.inform=TRUE);

  uXGs<-as.character(unique(mdfrZXs[["group"]]));
  uXVs<-as.character(unique(mdfrZXs[["variable"]]));
  uYGs<-as.character(unique(mdfrZYs[["group"]]));
  uYVs<-as.character(unique(mdfrZYs[["variable"]]));
  if (is.null(yvars)) yvars<-uYVs;
  for (uYV in uYVs){
    if (verbose) cat("Processing dependent variable ",uYV,"\n");
    mdfrYByXs<-NULL;
    for (uXG in uXGs){
      tmp<-mdfrZYs[mdfrZYs$variable==uYV,];
      tmp$group<-uXG;
      tmp$linetype<-uYV;
      mdfrYByXs<-rbind(mdfrYByXs,tmp);
    }
    tmp<-mdfrZXs;
    tmp$linetype<-xlab;
    mdfrYByXs<-rbind(mdfrYByXs,tmp);
    ##plot zscores for uYV and uXVs by year (x axis) and group (plot)
    ncol<-ceiling(length(uXVs)/8);
    p1 <- ggplot(mdfrYByXs,aes_string(x='year',y='value',
                                     colour='variable',linetype="linetype"));
    p1 <- p1 + geom_line(size=1.25);
    p1 <- p1 + scale_linetype_manual(values=c('dotted','solid'),breaks=c(uYV,xlab),limits=c(uYV,xlab))
    p1 <- p1 + ylab('z-score');
    p1 <- p1 + facet_grid(group~.)
    p1 <- p1 + guides(colour=guide_legend(title='',order=2,ncol=ncol),
                      linetype=guide_legend(title='',order=1));
    #print(p1);

    lm.vars<-list();
    sum.vars<-NULL;
    ##process by unique values of "group" for independent variables
    for (uXG in uXGs){ 
      if (verbose) cat("--Processing X group ",uXG,'\n');
      dfrYOnXs<-dcast(mdfrYByXs[mdfrYByXs$group==uXG,],year~variable,fun.aggregate=sum,value.var='value');
      ##linear model analysis
      for (uXV in uXVs){
        if (verbose) cat("----Assessing independent variable ",uXV,'\n');
        tst<-sum(abs(dfrYOnXs[[uXV]]),na.rm=TRUE)*sum(abs(dfrYOnXs[[uYV]]),na.rm=TRUE);
        if(!is.na(tst)&&(tst>0)){
          gv<-uXV;
          if (labelByGroup) gv<-paste(uXG,gv);
          if (verbose) cat("------Processing independent group/variable ",gv,'\n');
          lm.vars[[gv]]<-lm(as.formula(paste("`",uYV,"`~\`",uXV,"`",sep='')),dfrYOnXs);
          s<-summary(lm.vars[[gv]]);
          sum.vars<-rbind(sum.vars,data.frame(xgroup=uXG,
                                              x=uXV,
                                              rho=s$coefficients[2,1],
                                              rsq=s$r.squared,
                                              p=s$coefficients[2,4]));
        }
      }#uXVs loop
      ##if (verbose) cat(uXG,'\n');
    }#uXGs loop

    ##plot the linear fits
    tmp<-mdfrZXs;
    if (!labelByGroup) tmp$group<-'';
    dX<-dcast(tmp,year~group+variable,value.var='value');
    mX<-melt(dX,id.vars='year');
    mYX<-rbind(mX,mdfrZYs[mdfrZYs$variable==uYV,c('year','variable','value')])
    dYX<-dcast(mYX,year~variable,value.var='value');
    mYonX<-melt(dYX,id.vars=c('year',uYV));
    mYonX$variable<-gsub("_"," ",mYonX$variable,fixed=TRUE);
    
    p2 <- ggplot(mYonX,aes_string(y=paste0('`',uYV,'`'),x='value'));
    p2 <- p2 + geom_point(size=1.25);
    p2 <- p2 + stat_smooth(method="lm",formula=y~x);
    if (coord_fixed) p2 <- p2 + coord_fixed();
    p2 <- p2 + xlab(paste0('z-score(',xlab,')'));
    p2 <- p2 + ylab(paste0('z-score(',uYV,')'));
    p2 <- p2 + facet_wrap(~variable,nrow=nrows);
    #print(p2);
    
##    data <- rbind(data,mYonX);
    res[[uYV]]<-list(lms=lm.vars,summary=sum.vars,plots=list(p1=p1,p2=p2))
    sum.vars$ygroup<-'';
    sum.vars$y     <-uYV;
    sums<-rbind(sums,sum.vars[,c("ygroup","y","xgroup","x","rho","rsq","p")]);
  }##uYVs
  
  ##plot the linear fits on one page
  mYonXp<-NULL;
  for (uYV in uYVs){
    tmp<-mdfrZXs;
    if (!labelByGroup) tmp$group<-'';
    dX<-dcast(tmp,year~group+variable,value.var='value');
    mX<-melt(dX,id.vars='year');
    mYX<-rbind(mX,mdfrZYs[mdfrZYs$variable==uYV,c('year','variable','value')])
    dYX<-dcast(mYX,year~variable,value.var='value');
    mYonX<-melt(dYX,id.vars=c('year',uYV));
    mYonX$variable<-gsub("_"," ",mYonX$variable,fixed=TRUE);
    mYonX$yvar<-uYV;
    names(mYonX)<-c("year","y","xvar","x","yvar");
    mYonXp<-rbind(mYonXp,mYonX[,c("year","yvar","y","xvar","x")]);
  }
  
  p3 <- ggplot(mYonXp,aes_string(y="y",x="x"));
  p3 <- p3 + geom_point(size=1.25);
  p3 <- p3 + stat_smooth(method="lm",formula=y~x);
  if (coord_fixed) p3 <- p3 + coord_fixed();
  p3 <- p3 + xlab(paste0('z-score(',xlab,')'));
  p3 <- p3 + ylab(paste0('z-score(',ylab,')'));
  p3 <- p3 + facet_grid(yvar~xvar);
  #print(p3);

  ##create a dataframe for the data to the linear fits, but without using "group"
  mYonXp<-NULL;
  for (uYV in uYVs){
    tmp<-mdfrZXs;
    dX<-dcast(tmp,year~variable,value.var='value');
    mX<-melt(dX,id.vars='year');
    mYX<-rbind(mX,mdfrZYs[mdfrZYs$variable==uYV,c('year','variable','value')])
    dYX<-dcast(mYX,year~variable,value.var='value');
    mYonX<-melt(dYX,id.vars=c('year',uYV));
    mYonX$variable<-gsub("_"," ",mYonX$variable,fixed=TRUE);
    mYonX$yvar<-uYV;
    names(mYonX)<-c("year","y","xvar","x","yvar");
    mYonXp<-rbind(mYonXp,mYonX[,c("year","yvar","y","xvar","x")]);
  }
  
  return(invisible(list(res=res,summary=sums,plot=p3,data=mYonXp)));
}

# mdfrI<-mdfrEIs;  ylab<-'Index';           vars<-c('AO','PDO','MEI','lagged MEI');
# #mdfrI<-mdfrCSFs; ylab<-'Cross-shelf flow';vars<-c('east','central','west');
# lst<-calcLinRegs.YbyXs(mdfrEIs,mdfrFbySA,xlab="index");
