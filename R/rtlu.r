rtlu <- function(data,fileOutput="",textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    if(fileOutput!=""){
        sink(fileOutput)
        on.exit(sink())
    }else{}
    if(graphDir!=""){
        dir.create(graphDir,showWarnings=FALSE)
    }else{}
    r2lUniv(data=data,textBefore=textBefore,textAfter=textAfter,graphDir=graphDir,graphName=graphName,type=type,limDiscrete=limDiscrete)
}

r2lUniv <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
  UseMethod("r2lUniv")
}

r2lUniv.logical <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png"){
    r2lUniv.factor(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,type=type)
}

r2lUniv.caracter <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png"){
    r2lUniv.factor(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,type=type)
}

r2lUniv.factor <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    cat("\n\n%%%% rtlu.factor %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data,"&\n")
    r2lBarplot(data,"\\\\ \\hline \n",graphDir,graphName,type)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.ordered <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png"){
    cat("\n\n%%%% rtlu.ordered %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data,"&\n")
    r2lSummary(data,"&\n")
    r2lBarplot(data," \\\\ \\hline \n",graphDir=graphDir,graphName=graphName,type=type)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.integer <- r2lUniv.numeric <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    if(length(table(data))<=limDiscrete && identical(as.numeric(data),round(data))){
      class(data) <- c("discrete",class(data))
    }else{
      class(data) <- c("continuous",class(data))
    }
    r2lUniv(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,type=type)
}

r2lUniv.discrete <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png"){
    cat("\n\n%%%% rtlu.discrete %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data,"&\n")
    r2lSummary(data,"&\n")
    r2lBoxplot(data," & \n",graphDir=graphDir,graphName=graphName,type=type)
    r2lBarplot(data,"\\\\ \\hline \n",graphDir=graphDir,graphName=graphName,type=type)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.continuous <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png"){
    cat("\n\n%%%% r2lu.continuous %%%%\n")
    cat(textBefore,"\n")
    r2lBeginTitle(data)
    r2lSummary(data,"&\n")
    r2lBoxplot(data,"&\n",graphDir=graphDir,graphName=graphName,type=type)
    r2lHist(data,"\\\\ \\hline \n",graphDir=graphDir,graphName=graphName,type=type)
    r2lEnd()
    cat(textAfter,"\n")
}

r2lUniv.data.frame <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png"){
    nbVar <- length(data)
    if(length(textBefore)==1){textBefore <- rep(textBefore,time=nbVar)}else{}
    if(length(graphName)==1){graphName <- paste(graphName,1:nbVar,sep="")}else{}
    cat("\n\n%%%% r2lu.data.frame %%%%\n")
    for (i in 1:nbVar){
        r2lUniv(data[,i],textBefore=textBefore[i],graphDir=graphDir,graphName=graphName[i],type=type)
    }
}






rtluMainFile <- function(fileList,fileName="main.tex"){
  sink(fileName)
  cat("
\\documentclass[a4paper,10pt]{article}
\\usepackage{graphicx}
\\title{R to LaTeX}
\\author{Example}
\\date{}
\\begin{document}
\\maketitle
")
}
