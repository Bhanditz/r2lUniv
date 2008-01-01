`r2lHist` <-
function(variable,latexNext,dirGraph="",graphName="V"){
  if(dirGraph==""){
    nomPShist <- paste(graphName,"-barplot.eps",sep="")
  }else{
    nomPShist <- paste(dirGraph,"/",graphName,"-hist.eps",sep="")
  }
  postscript(file=nomPShist,horizontal=FALSE,width=60,height=60)
    hist(variable,col="grey",main="",xlab="",ylab="")
  dev.off()
  cat("
      \\begin{tabular}{@{}l@{}}
        \\epsfig{figure=",nomPShist,",width=3cm}
      \\end{tabular}
   ",latexNext)
}

