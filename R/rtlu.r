rtlu <- function(data,fileOutput="",textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    if(fileOutput!=""){
        sink(fileOutput)
        on.exit(sink())
    }else{}
    if(graphDir!=""){
        dir.create(graphDir,showWarnings=FALSE)
    }else{}
    r2lUniv(data=data,textBefore=textBefore,textAfter=textAfter,graphDir=graphDir,graphName=graphName,...)
}

r2lUniv <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
  UseMethod("r2lUniv")
}

r2lUniv.logical <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    r2lUniv.factor(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,...)
}

r2lUniv.caracter <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    r2lUniv.factor(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,...)
}

r2lUniv.factor <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    cat("\n\n%%%% r2lu.factor %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data,"&\n")
    r2lBarplot(data,"\\\\ \\hline \n",graphDir,graphName)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.ordered <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    cat("\n\n%%%% r2lu.ordered %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data,"&\n")
    r2lSummary(data,"&\n")
    r2lBarplot(data," \\\\ \\hline \n",graphDir,graphName)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.integer <- r2lUniv.numeric <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",limDiscrete=10,...){
    if(length(table(data))<=limDiscrete && identical(as.numeric(data),round(data))){
      class(data) <- c("discrete",class(data))
    }else{
      class(data) <- c("continuous",class(data))
    }
    r2lUniv(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,...)
}

r2lUniv.discrete <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    cat("\n\n%%%% r2lu.discrete %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data,"&\n")
    r2lSummary(data,"&\n")
    r2lBoxplot(data," & \n",graphDir,graphName)
    r2lBarplot(data,"\\\\ \\hline \n",graphDir,graphName)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.continuous <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    cat("\n\n%%%% r2lu.continuous %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lSummary(data,"&\n")
    r2lBoxplot(data,"&\n",graphDir,graphName)
    r2lHist(data,"\\\\ \\hline \n",graphDir,graphName)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.data.frame <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",...){
    nbVar <- length(data)
    if(length(textBefore)==1){textBefore <- rep(textBefore,time=nbVar)}else{}
    if(length(graphName)==1){graphName <- paste(graphName,1:nbVar,sep="")}else{}
    cat("\n\n%%%% r2lu.data.frame %%%%\n")
    for (i in 1:nbVar){
        r2lUniv(data[,i],textBefore=textBefore[i],graphDir=graphDir,graphName=graphName[i],...)
    }
}






rtluMainFile <- function(fileList,fileName="main.tex"){
  sink(fileName)
  cat("
\\documentclass[a4paper,10pt]{article}
\\usepackage{epsfig}
\\title{R to LaTeX}
\\author{Example}
\\date{}
\\begin{document}
\\maketitle
")
  for (i in fileList){
    cat("\\input{",i,"}\n",sep="")
  }
  cat("\\end{document}")
  sink()
}




















