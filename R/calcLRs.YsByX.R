#'
#'@title Calculate linear regressions for time series in Y as function of time series in X.
#'
#'@description Function to calculate linear regressions between time series in Y as function of time series in X.
#'
#'@param mdfrX - melted dataframe with independent (X) time series
#'@param mdfrY - melted dataframe with dependent (Y) time series
#'@param yvars - vector of dependent variables to process (or NULL for all) 
#'@param xlab - label associated with independent variables
#'@param ylab - label associated with  dependent variables
#'@param nrows - number of rows for LR plots
#'@param coord_fixed - flag to use equal x/y unit dimensions in LR plots
#'@param labelByGroup - flag to use "group" column to organize linear regression analysis
#'
#'@return list with named elements 'res', 'summary' and 'plot': \cr
#'\itemize{
#'  \item res - a list with sublists by unique X with elements
#'  \itemize{
#'    \item lms     - list with linear model (lm) results by Y for X
#'    \item summary - dataframe with summaries of linear model results
#'    \item plots   - list of ggplot2 objects (p1=standardized time series, p2=fits to lm's)
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
calcLRs.YsByX<-function(mdfrX,
                        mdfrY,
                        yvars=NULL,
                        xlab="index",
                        ylab=NULL,
                        nrows=2,
                        coord_fixed=FALSE,
                        labelByGroup=FALSE){
#   require(ggplot2);
#   require(plyr);
#   require(reshape2);
#   require(wtsUtilities);
  
  #define objects for output
  res<-list();#list of individual LR results and plot objects
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
  for (uXV in uXVs){
    cat("Processing independent variable ",uXV,"\n");
    mdfrYsByX<-NULL;
    for (uYG in uYGs){
      tmp<-mdfrZXs[mdfrZXs$variable==uXV,];
      tmp$group<-uYG;
      tmp$linetype<-uXV;
      mdfrYsByX<-rbind(mdfrYsByX,tmp);
    }
    tmp<-mdfrZYs;
    tmp$linetype<-ylab;
    mdfrYsByX<-rbind(mdfrYsByX,tmp);
    ##plot zscores for uYVs and uXV by year (x axis) and group (plot)
    ncol<-ceiling(length(uYVs)/8);
    p1 <- ggplot(mdfrYsByX,aes_string(x='year',y='value',
                                     colour='variable',linetype="linetype"));
    p1 <- p1 + geom_line(size=1.25);
    p1 <- p1 + scale_linetype_manual(values=c('dotted','solid'),breaks=c(uXV,ylab),limits=c(uXV,ylab))
#    p1 <- p1 + scale_colour_discrete(aesthetics="value",scale_name="colour",breaks=c(uXV,uYVs))
    p1 <- p1 + ylab('z-score');
    p1 <- p1 + facet_grid(group~.)
    p1 <- p1 + guides(colour=guide_legend(title='',order=2,ncol=ncol),
                      linetype=guide_legend(title='',order=1));
    #print(p1);

    lm.vars<-list();
    sum.vars<-NULL;
    ##process by unique values of "group"
    for (uYG in uYGs){ 
      cat("--Processing Y group ",uYG,'\n')
      dfrYsOnX<-dcast(mdfrYsByX[mdfrYsByX$group==uYG,],year~variable,fun.aggregate=sum,value.var='value');
      ##linear model analysis
      for (yvar in yvars){
        cat("----Assessing dependent variable ",yvar,'\n');
        if (!is.null(dfrYsOnX[[uXV]])&&!is.null(dfrYsOnX[[yvar]])){
          tst<-sum(abs(dfrYsOnX[[uXV]]),na.rm=TRUE)*sum(abs(dfrYsOnX[[yvar]]),na.rm=TRUE);
          if(!is.na(tst)&&(tst>0)){
            gv<-yvar;
            if (labelByGroup) gv<-paste(uYG,gv);
            cat("------Processing dependent group/variable ",gv,'\n')
            lm.vars[[gv]]<-lm(as.formula(paste("`",yvar,"`~\`",uXV,"`",sep='')),dfrYsOnX);
            s<-summary(lm.vars[[gv]]);
            sum.vars<-rbind(sum.vars,data.frame(ygroup=uYG,
                                                y=yvar,
                                                rho=s$coefficients[2,1],
                                                rsq=s$r.squared,
                                                p=s$coefficients[2,4]));
          }
        }
      }#yvars loop
      ##cat(uYG,'\n')
    }#uYGs loop

    ##plot the linear fits
    tmp<-mdfrZYs[mdfrZYs$variable %in% yvars,];
    if (!labelByGroup) tmp$group<-'';
    dY<-dcast(tmp,year~group+variable,value.var='value');
    mY<-melt(dY,id.vars='year');
    mYX<-rbind(mdfrZXs[mdfrZXs$variable==uXV,c('year','variable','value')],mY)
    dYX<-dcast(mYX,year~variable,value.var='value');
    mYonX<-melt(dYX,id.vars=c('year',uXV));
    mYonX$variable<-gsub("_"," ",mYonX$variable,fixed=TRUE);
    
    p2 <- ggplot(mYonX,aes_string(y='value',x=paste0('`',uXV,'`')));
    p2 <- p2 + geom_point(size=1.25);
    p2 <- p2 + stat_smooth(method="lm",formula=y~x);
    if (coord_fixed) p2 <- p2 + coord_fixed();
    p2 <- p2 + xlab(paste0('z-score(',uXV, ')'));
    p2 <- p2 + ylab(paste0('z-score(',ylab,')'));
    p2 <- p2 + facet_wrap(~variable,nrow=nrows);
    #print(p2);
    
    res[[uXV]]<-list(lms=lm.vars,summary=sum.vars,plots=list(p1=p1,p2=p2))
    sum.vars$xgroup<-'';
    sum.vars$x     <-uXV;
    sums<-rbind(sums,sum.vars[,c("ygroup","y","xgroup","x","rho","rsq","p")]);
  }##uXVs
  
  
  ##plot the linear fits on one page
  mYonXp<-NULL;
  for (uXV in uXVs){
    tmp<-mdfrZYs[mdfrZYs$variable %in% yvars,];
    if (!labelByGroup) tmp$group<-'';
    dY<-dcast(tmp,year~group+variable,value.var='value');
    mY<-melt(dY,id.vars='year');
    mYX<-rbind(mdfrZXs[mdfrZXs$variable==uXV,c('year','variable','value')],mY)
    dYX<-dcast(mYX,year~variable,value.var='value');
    mYonX<-melt(dYX,id.vars=c('year',uXV));
    mYonX$variable<-gsub("_"," ",mYonX$variable,fixed=TRUE);
    mYonX$xvar<-uXV;
    names(mYonX)<-c("year","x","yvar","y","xvar");
    mYonXp<-rbind(mYonXp,mYonX);
  }
  
  p3 <- ggplot(mYonXp,aes_string(y="y",x="x"));
  p3 <- p3 + geom_point(size=1.25);
  p3 <- p3 + stat_smooth(method="lm",formula=y~x);
  if (coord_fixed) p3 <- p3 + coord_fixed();
  p3 <- p3 + xlab(paste0('z-score(',xlab,')'));
  p3 <- p3 + ylab(paste0('z-score(',ylab,')'));
  p3 <- p3 + facet_grid(yvar~xvar);
  #print(p3);

  return(invisible(list(res=res,summary=sums,plot=p3)));
}

# mdfrI<-mdfrEIs;  ylab<-'Index';           vars<-c('AO','PDO','MEI','lagged MEI');
# #mdfrI<-mdfrCSFs; ylab<-'Cross-shelf flow';vars<-c('east','central','west');
# lst<-calcLinRegs.YsByX(mdfrEIs,mdfrFbySA,xlab='index',ylab='FbySA',yvars=formatZeros(1:11));
