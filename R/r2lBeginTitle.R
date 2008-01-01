`r2lBeginTitle` <-
function(variable,varName=NA,varNumber=NA){
  if(!is.na(varName) || !is.na(varNumber)){
    title <- "{\\large \\bf "
    if(!is.na(varNumber)){
      title <- paste(title,varNumber,". ",sep="")
    }else{}
    if(!is.na(varName)){
      title <- paste(title,varName,sep="")
    }else{}
    title <- paste(title,"}",sep="")    
  }else{
    title <- NA
  }
  
  if(class(variable)[1]=="nominal"){
    nbColumn <- 2
    variableType <- "Nominal"
    columnTitle <- "{\\bf Frequency} & {\\bf Histogram}"
  }else{}
  if(class(variable)[1]=="ordinal"){
    nbColumn <- 3
    variableType <- "Ordinal"
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

  cat("    \\begin{tabular}{|",rep("c|",nbColumn),"}\n",sep="")
  if(!is.na(title)){
    cat("        \\multicolumn{",nbColumn,"}{c}{",title,"} \\\\ \n",sep="")
  }else{}
  cat("      \\hline
        \\multicolumn{",nbColumn,"}{|l|}{",
          variableType," \\hfill N=",length(variable),
          " ; NA=",sum(is.na(variable))," (",format(sum(is.na(variable))/length(variable)*100,digits=3),"\\%)}\\\\
      \\hline
        ",columnTitle,"  \\\\
  ",sep="")
}

