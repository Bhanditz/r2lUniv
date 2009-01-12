################################
### Basic class
###  - logical and character are like factor
###  - integer and numeric are changed in discrete or continuous according to the number of modalities

r2lUniv.factor <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    cat("\n\n%%%% rtlu.factor %%%%\n")
    cat("\n",textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data)
    cat("      &\n")
    r2lBarplot(data,graphDir,graphName,type)
    cat("      \\\\ \\hline \n")
    r2lEnd()
    cat("\n",textAfter,"\n")
}


r2lUniv.logical <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    r2lUniv.factor(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,type=type)
}


r2lUniv.character <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    r2lUniv.factor(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,type=type)
}


r2lUniv.ordered <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    cat("\n\n%%%% rtlu.ordered %%%%\n")
    cat("\n",textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data)
    cat("      &\n")
    r2lSummary(data)
    cat("      &\n")
    r2lBarplot(data,graphDir=graphDir,graphName=graphName,type=type)
    cat("      \\\\ \\hline \n")
    r2lEnd()
    cat("\n",textAfter,"\n")
}


r2lUniv.discrete <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    cat("\n\n%%%% rtlu.discrete %%%%\n")
    cat("\n",textBefore,"\n")
    r2lBeginTitle(data)
    r2lFrequency(data)
    cat("      &\n")
    r2lSummary(data)
    cat("      &\n")
    r2lBoxplot(data,graphDir=graphDir,graphName=graphName,type=type)
    cat("      & \n")
    r2lBarplot(data,graphDir=graphDir,graphName=graphName,type=type)
    cat("      \\\\ \\hline \n")
    r2lEnd()
    cat("\n",textAfter,"\n")
}


r2lUniv.continuous <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    cat("\n\n%%%% r2lu.continuous %%%%\n")
    cat("\n",textBefore,"\n")
    r2lBeginTitle(data)
    r2lSummary(data)
    cat("      &\n")
    r2lBoxplot(data,graphDir=graphDir,graphName=graphName,type=type)
    cat("      &\n")
    r2lHist(data,graphDir=graphDir,graphName=graphName,type=type)
    cat("      \\\\ \\hline \n")
    r2lEnd()
    cat("\n",textAfter,"\n")
}


r2lUniv.integer <- r2lUniv.numeric <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    if(length(table(data))<=limDiscrete && identical(as.numeric(data),unclass(round(data)))){
      class(data) <- c("discrete",class(data))
    }else{
      class(data) <- c("continuous",class(data))
    }
    r2lUniv(data=data,textBefore=textBefore,graphDir=graphDir,graphName=graphName,type=type)
}




r2lUniv.data.frame <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
    nbVar <- length(data)
    if(length(textBefore)==1){textBefore <- rep(textBefore,time=nbVar)}else{}
    if(length(graphName)==1){graphName <- paste(graphName,1:nbVar,sep="")}else{}
    if(length(limDiscrete)==1){limDiscrete <- rep(limDiscrete,time=nbVar)}else{}
    cat("\n\n%%%% r2lu.data.frame %%%%\n")
    for (i in 1:nbVar){
        r2lUniv(data[,i],textBefore=textBefore[i],graphDir=graphDir,graphName=graphName[i],type=type,limDiscrete=limDiscrete[i])
    }
}


r2lUniv <- function(data,textBefore="",textAfter="",graphDir="graphUniv",graphName="V",type="png",limDiscrete=10){
  UseMethod("r2lUniv")
}


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







rtluMainFile <- function(fileName="main.tex",text=""){
  sink(fileName)
  cat("
\\documentclass[a4paper,10pt]{article}
\\usepackage{graphicx}
\\title{R to LaTeX}
\\author{Example}
\\date{}
\\begin{document}
",text,"
\\end{document}")
  sink()
}
