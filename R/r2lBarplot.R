`r2lBarplot` <-
function(variable,latexNext,dirGraph="",graphName="V"){
  if(dirGraph==""){
    nomPSbarplot <- paste(graphName,"-barplot.eps",sep="")
  }else{
    nomPSbarplot <- paste(dirGraph,"/",graphName,"-barplot.eps",sep="")
  }
  if(class(variable)[1]=="discrete"){
    tabVar <- table(c(variable,range(na.omit(variable))[1]:range(na.omit(variable))[2]))-1
  }else{
    tabVar <- table(variable)
  }
  postscript(file=nomPSbarplot,horizontal=FALSE,width=60,height=60)
    barplot(tabVar,col="grey",main="",xlab="",ylab="")
  dev.off()
  cat("      \\begin{tabular}{@{}l@{}}
        \\epsfig{figure=",nomPSbarplot,",width=3cm}
      \\end{tabular}
   ",latexNext)
}

