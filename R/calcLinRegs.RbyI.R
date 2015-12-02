#'
#'@title Calculate linear regressions between recruitment and index time series
#'
#'@description Function to calculate linear regressions between recruitment and index time series
#'
#'@param mdfrZR - melted dataframe with standardized recruitment time series
#'@param mdfrI - melted dataframe with raw index time series
#'@param ylab - label for y axis in time series plot of raw time series
#'@param nrows - number of rows for LR plots
#'@param labelByGroup - flag to use "group" column to organize linear regression analysis
#'
#'@return list with elements
#'* mdfrZIs - dataframe with z-scores for index time series
#'* lms     - list with linear model (lm) results by index
#'* summary - dataframe with summaries of linear model results
#'* plots   - list of ggplot2 objects (raw time series, standardized time series, fits to lm's)
#'
#'@import ggplot2
#'@import plyr
#'@import reshape2
#'@import wtsUtilities
#'
#'@export
#'
calcLinRegs.RbyI<-function(mdfrZR,mdfrI,ylab,vars,nrows=2,labelByGroup=FALSE){
  #define list for ggplot2 objects
  plots<-list();
  
  ##plot time series (not standardized)
  p1 <- ggplot(mdfrI,aes_string(x='year',y='value',colour='variable'));
  p1 <- p1 + geom_line(size=1.25);
  p1 <- p1 + ylab(ylab);
  p1 <- p1 + facet_grid(group~.)
  p1 <- p1 + guides(colour=guide_legend(title='',order=2),
                    linetype=guide_legend(title='',order=1));
  ##print(p1);
  plots$p1<-p1;
  
  ##standardize values by variable and group across years
  mdfrZIs<-ddply(mdfrI,.variables=c('group','variable'),.fun=standardize,col='value',.inform=TRUE);

  uGs<-unique(mdfrZIs[["group"]]);
  mdfrRbyIs<-NULL;
  for (uG in uGs){
    tmp<-mdfrZR;
    tmp$group<-uG;
    tmp$linetype<-'R'
    mdfrRbyIs<-rbind(mdfrRbyIs,tmp);
  }
  tmp<-mdfrZIs;
  tmp$linetype<-'Index';
  mdfrRbyIs<-rbind(mdfrRbyIs,tmp);
  p2 <- ggplot(mdfrRbyIs,aes_string(x='year',y='value',
                                  colour='variable',linetype="linetype"));
  p2 <- p2 + geom_line(size=1.25);
  p2 <- p2 + ylab('z-score');
  p2 <- p2 + facet_grid(group~.)
  p2 <- p2 + guides(colour=guide_legend(title='',order=2),
                    linetype=guide_legend(title='',order=1));
  ##print(p2);
  plots$p2<-p2;

  lm.vars<-list();
  sum.vars<-NULL;
  ##process by unique values of "group"
  for (uG in uGs){ 
    dfrRonIs<-dcast(mdfrRbyIs[mdfrRbyIs$group==uG,],year~variable,fun.aggregate=sum,value.var='value');
    ##linear model analysis
    for (var in vars){
      if (var %in% names(dfrRonIs)){
        tst<-sum(abs(dfrRonIs[[var]]),na.rm=TRUE);
        if(!is.na(tst)&&(tst>0)){
          gv<-var;
          if (labelByGroup) gv<-paste(uG,var);
          cat("Processing",gv,'\n')
          lm.vars[[gv]]<-lm(as.formula(paste("R~\`",var,"`",sep='')),dfrRonIs);
          s<-summary(lm.vars[[gv]]);
          sum.vars<-rbind(sum.vars,data.frame(index=gv,
                                              rho=s$coefficients[2,1],
                                              rsq=s$r.squared,
                                              p=s$coefficients[2,4]));
        }
      }
    }#vars loop
    ##cat(uG,'\n')
  }#uGs loop
  ##knitr::kable(sum.vars);

  ##plot the linear fits
  tmp<-mdfrZIs;
  if (!labelByGroup) tmp$group<-'';
  dI<-dcast(tmp,year~group+variable,value.var='value');
  mI<-melt(dI,id.vars='year');
  mRI<-rbind(mdfrZR[,1:3],mI)
  dRI<-dcast(mRI,year~variable,value.var='value');
  mRonI<-melt(dRI,id.vars=c('year','R'));
  mRonI$variable<-gsub("_"," ",mRonI$variable,fixed=TRUE);
  p3 <- ggplot(mRonI,aes_string(y='R',x='value'));
  p3 <- p3 + geom_point(size=1.25);
  p3 <- p3 + stat_smooth(method="lm",formula=y~x);
  p3 <- p3 + coord_fixed();
  p3 <- p3 + xlab('z-score(index)');
  p3 <- p3 + ylab('z-score(R)');
  p3 <- p3 + facet_wrap(~variable,nrow=nrows);
  ##print(p3);
  plots$p3<-p3;

  return(invisible(list(mdfrZIs=mdfrZIs,lms=lm.vars,summary=sum.vars,plots=plots)));
}

# mdfrI<-mdfrEIs;  ylab<-'Index';           vars<-c('AO','PDO','MEI','lagged MEI');
# #mdfrI<-mdfrCSFs; ylab<-'Cross-shelf flow';vars<-c('east','central','west');
# calcLinRegs.RbyI(mdfrzSAR,mdfrI,ylab,vars);
