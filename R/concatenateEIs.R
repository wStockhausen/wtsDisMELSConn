#'
#'@title Concatenate files with environmental indices.
#'
#'@description Function to concatenate files with environmental indices.
#'
#'@param caption - caption for file dialog
#'@param types - vector of extension types to include in file dialog
#'@param hasHeader - flag indicating file has header
#'@param id.vars - vector of column names to use as id vars
#'@param out.csv - name of output csv file
#'@param out.dir - directory for output csv file
#'
#'@return melted dataframe
#'
#'@importFrom wtsUtilities addFilter
#'@importFrom tcltk tk_choose.files
#'@importFrom reshape2 melt
#'
#'@export
#'
concatenateEIs<-function(caption="Select environmental index files",
                         types="txt",
                         hasHeader=FALSE,
                         id.vars='year',
                         out.csv='test.csv',
                         out.dir=NULL){
#     require(wtsUtilities);
#     require(tcltk);
#     require(reshape2);
    Filters<-NULL;
    for (type in types){
        Filters<-addFilter(type,desc=paste(type," files (*.",type,")",sep=''),filter=paste("*.",type,sep=''),Filters=Filters);
    }
    in.fns<-tk_choose.files(caption=caption,
                             multi=TRUE,filters=matrix(Filters[types,],nrow=length(types),ncol=2,byrow=FALSE));
    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.fns[1])) {out.dir<-dirname(file.path(in.fns[1]));}
    }
      
    out.csv<-file.path(out.dir,out.csv);#full file path
      
    #use write.csv method
    dfr<-NULL;
    inclHdr<-TRUE;
    for (in.fn in in.fns){
        cat("Processing",in.fn,"\n")
        tbl<-read.csv(in.fn,sep='',header=hasHeader,stringsAsFactors=FALSE);
        if (ncol(tbl)>2){
          #read it in again,this time with headers
          tbl<-read.csv(in.fn,sep='',header=TRUE,stringsAsFactors=FALSE);
          names(tbl)[1]<-"year";
          tbl<-melt(tbl,id.vars="year",value.name="value");
          tbl$variable<-paste(gsub(".txt","",basename(in.fn),fixed=TRUE),tbl$variable,sep="_");
        } else {
          names(tbl)<-c("year","value");
          tbl$variable<-gsub(".txt","",basename(in.fn),fixed=TRUE);
        }
        dfr<-rbind(dfr,tbl);
        if (inclHdr){
            inclHdr<-FALSE;
            write.table(tbl,file=out.csv,sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE,append=FALSE)
        } else {
            write.table(tbl,file=out.csv,sep=",",quote=FALSE,col.names=FALSE,row.names=FALSE,append=TRUE)
        }
    }
    return(dfr);
}

##dfr<-concatenateEIs(types=c("txt","dat"));
