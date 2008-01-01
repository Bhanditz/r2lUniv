`r2lBoxplot` <-
function(variable,latexNext,dirGraph="",graphName="V"){
  if(dirGraph==""){
    nomPSboxplot <- paste(graphName,"-boxplot.eps",sep="")
  }else{
    nomPSboxplot <- paste(dirGraph,"/",graphName,"-boxplot.eps",sep="")
  }
  postscript(file=nomPSboxplot,horizontal=FALSE,width=40,height=80)
    boxplot(variable,main="",xlab="",ylab="")
  dev.off()
  cat("
      \\begin{tabular}{@{}l@{}}
        \\epsfig{figure=",nomPSboxplot,",width=2cm}
      \\end{tabular}
   ",latexNext)
}

