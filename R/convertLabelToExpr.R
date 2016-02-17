#'
#'@title Convert plot with newline characters to plotmath
#'
#'@description Function to convert plot with newline characters to plotmath
#'
#'@param txt - character vector of labels.
#'@param test - flag to plot labels (for debugging)
#'
#'@return character vector of labels
#'
#'@details Converts the vector of labels to a text vector which
#'can be parsed to yield a plotmath expression. Each label is first split
#'by newline characters. The first split  is re-split using ":". The 
#'last line is split by "R^2". The resulting splits are recombined in nested
#''atop(x,y)' functions (see plotmath), with the first re-split interpreted
#'as a plotmath expression, as is 'R^2'.
#'
#'test=TRUE requires ggplot2 package.
#'
#'@export
#'
convertLabelToExpr<-function(txt,test=FALSE){
  lst<-strsplit(txt,split="\n",fixed=TRUE);
  nlst<-length(lst);
  res<-vector(mode="character",length=nlst)
  dfrp<-NULL;
  for (i in 1:nlst){
    ttl<-lst[[i]];
    nt<-length(ttl);
    txp<-strsplit(ttl[1],split=":",fixed=TRUE);
    strp<-'';
    L1<-paste0("paste(",txp[[1]][1],",':",txp[[1]][2],"')");
    if (nt==2){
      rxp<-strsplit(ttl[2],"R^2",fixed=TRUE);
      LR<-paste0("paste('",rxp[[1]][1],"',R^2,'",rxp[[1]][2],"')");
      strp<-paste0("atop(",L1,",",LR,")");
    } else if (nt==3){
      L2<-ttl[2];
      rxp<-strsplit(ttl[3],"R^2",fixed=TRUE);
      LR<-paste0("paste('",rxp[[1]][1],"',R^2,'",rxp[[1]][2],"')");
      strp<-paste0("atop(",L1,",atop('",L2,"',",LR,")",")");
    } else if (nt==4){
      L2<-ttl[2];
      L3<-ttl[3];
      rxp<-strsplit(ttl[4],"R^2",fixed=TRUE);
      LR<-paste0("paste('",rxp[[1]][1],"',R^2,'",rxp[[1]][2],"')");
      strp<-paste0("atop(",L1,",atop('",L2,"',atop('",L3,"',",LR,")",")",")");
    }

    res[i]<-strp;
    if (test){
      dfr<-as.data.frame(list(x=0.5,y=0.5,t=strp,lbl=strp));
      p <- ggplot2::ggplot(dfr,aes(x=x,y=y));
      p <- p + ggplot2::geom_text(aes(label=t),parse=TRUE);
      p <- p + ggplot2::xlab(strp);
      print(p);
      dfrp<-rbind(dfrp,dfr);
    }
  }
  if (test){
    p <- ggplot2::ggplot(dfrp,aes(x=x,y=y));
    p <- p + ggplot2::geom_text(aes(label=t),parse=TRUE);
    p <- p + facet_wrap(~lbl,labeller=label_parsed)
    print(p);
  }
  return(res);
}

