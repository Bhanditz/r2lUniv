#############################
### Print (optional) the number and the name of a variable
### Print the variable type, the \begin{tabular} and the title of each column.
### The number of column and the title change according to the variable type.
### Used in: r2lNominal, r2lOrdinal, r2lDiscrete, r2lContinuous

r2lBeginTitle <- function(variable){
    cat("  \\begin{center}\n")
    cat("    \\addtolength{\\leftskip}{-4cm}\\addtolength{\\rightskip}{-4cm}\n\n")

    if(class(variable)[1]=="factor" || class(variable)[1]=="character"){
        nbColumn <- 2
        columnTitle <- "{\\bf Frequency} & {\\bf Histogram}"
        variableType <- "Nominal"
    }else{}

    if(class(variable)[1]=="logical"){
        nbColumn <- 2
        columnTitle <- "{\\bf Frequency} & {\\bf Histogram}"
        variableType <- "Logical"
    }else{}

    if(class(variable)[1]=="ordered"){
        nbColumn <- 3
        variableType <- "Ordered"
        columnTitle <- "{\\bf Frequency} & {\\bf Summary} & {\\bf Histogram}"
    }else{}

    if(class(variable)[1]=="discrete"){
        nbColumn <- 4
        variableType <- "Discrete"
        columnTitle <- "{\\bf Frequency} & {\\bf Summary} & {\\bf Boxplot} & {\\bf Histogram}"
    }else{}
    if(class(variable)[1]=="continuous"){
        nbColumn <- 3
        variableType <- "Continuous"
        columnTitle <- "{\\bf Summary} & {\\bf Boxplot} & {\\bf Histogram}"
    }else{}

    cat("    \\begin{tabular}{",rep("|c",nbColumn),"|}\n",sep="")
    cat("      \\hline
        \\multicolumn{",nbColumn,"}{|l|}{",
          variableType," \\hfill N=",length(variable),
          " ; NA=",sum(is.na(variable))," (",format(sum(is.na(variable))/length(variable)*100,digits=3),"\\%)}\\\\
      \\hline
        ",columnTitle,"  \\\\
    ",sep="")
}


#############################
### Print each Frequency, its size and frequency
### Used in: r2lNominal, r2lOrdinal, r2lDiscrete

r2lFrequency <- function(variable){
    cat("      \\begin{tabular}{@{}l@{ : }cl@{}}\n")
    tableVar <- table(variable)
    sumVar <- sum(tableVar)
    for(i in 1:nrow(tableVar)){
        cat("           ",labels(tableVar[i]),"&",tableVar[i][[1]],"&(")
        cat(format(tableVar[i][[1]]/sumVar*100,digits=3))
        cat("\\%) \\\\\n")
    }
    cat("          \\end{tabular}\n")
}


#############################
### Print quartiles (for ordered, discrete and continuous), mean and var (for discrete and continuous)
### Used in: r2lOrdinal, r2lDiscrete, r2lContinuous

r2lSummary <- function(variable){
  cat("      \\begin{tabular}{@{}l@{ : }l@{}}\n")
  if(class(variable)[1]=="ordered"){
    quartile <- summary(as.integer(variable))
    niveaux <- levels(variable)
    cat("        Min.   &",niveaux[quartile[1]],"\\\\\n")
    cat("        Q1     &",niveaux[round(quartile[2])],"\\\\\n")
    cat("        Median &",niveaux[round(quartile[3])],"\\\\\n")
    cat("        Q3     &",niveaux[round(quartile[5])],"\\\\\n")
    cat("        Max.   &",niveaux[quartile[6]],"\\\\\n")
  }else{
    quartile <- summary(variable)
    cat("        Mean   &",format(mean(na.omit(variable)),digits=3),"\\\\\n")
    cat("        Var.   &",format(var(na.omit(variable)),digits=3),"\\\\\n")
    cat("        SD     &",format(sd(na.omit(variable)),digits=3),"\\\\ \n      \\hline \n")
    cat("        Min.   &",format(quartile[1],digits=3),"\\\\\n")
    cat("        Q1     &",format(quartile[2],digits=3),"\\\\\n")
    cat("        Median &",format(quartile[3],digits=3),"\\\\\n")
    cat("        Q3     &",format(quartile[5],digits=3),"\\\\\n")
    cat("        Max.   &",format(quartile[6],digits=3),"\\\\\n")
  }
  cat("      \\end{tabular}\n")
}


#############################
### Compute a barplot, save it in a file of type 'type' and print the LaTeX code
### Used in: r2lNominal, r2lOrdinal, r2lDiscrete

r2lBarplot <- function(variable,graphDir="",graphName="V",type="png"){
  if(graphDir==""){
    nomBarplot <- paste(graphName,"-barplot",sep="")
  }else{
    nomBarplot <- paste(graphDir,"/",graphName,"-barplot",sep="")
  }
  if(class(variable)[1]=="discrete"){
    tabVar <- table(c(variable,range(na.omit(variable))[1]:range(na.omit(variable))[2]))-1
  }else{
    tabVar <- table(variable)
  }
  barplot(tabVar,col="grey",main="",xlab="",ylab="")
  savePlot(filename=nomBarplot,type=type)
  cat("          \\begin{tabular}{@{}l@{}}
            \\includegraphics[width=3cm]{",nomBarplot,"}
          \\end{tabular}\n",sep="")
}


#############################
### Compute a histogram, save it in file of type 'type' and print the LaTeX code
### Used in: r2lContinuous

r2lHist <- function(variable,graphDir="",graphName="V",type="png"){
  if(graphDir==""){
    nomHist <- paste(graphName,"-hist",sep="")
  }else{
    nomHist <- paste(graphDir,"/",graphName,"-hist",sep="")
  }
  hist(variable,col="grey",main="",xlab="",ylab="")
  savePlot(filename=nomHist,type=type)
  cat("          \\begin{tabular}{@{}l@{}}
             \\includegraphics[width=3cm]{",nomHist,"}
          \\end{tabular}\n",sep="")
}


#############################
### Compute a boxplot, save it in a postscript file and print the LaTeX code
### Used in: r2lDiscrete, r2lContinuous

r2lBoxplot <- function(variable,graphDir="",graphName="V",type="png"){
  if(graphDir==""){
    nomBoxplot <- paste(graphName,"-boxplot",sep="")
  }else{
    nomBoxplot <- paste(graphDir,"/",graphName,"-boxplot",sep="")
  }
  boxplot(variable,main="",xlab="",ylab="")
  savePlot(filename=nomBoxplot,type=type)
  cat("          \\begin{tabular}{@{}l@{}}
             \\includegraphics[width=3cm]{",nomBoxplot,"}
          \\end{tabular}\n",sep="")
}


#############################
### End of the tabular and the begin
### Used in : r2lNominal, r2lOrdianl, r2lDiscrete, r2lContinuous

r2lEnd <- function(){
  cat("    \\end{tabular}

  \\end{center}
  ")
}



















