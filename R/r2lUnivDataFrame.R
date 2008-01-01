`r2lUnivDataFrame` <-
function(data,varName="",varNumber="",dirGraph="",graphName="V",limDiscrete=10,classModification=""){

  if(length(varName)==1){
    if(identical(varName,"")){
      varName <- names(data)
    }else{
      varName <- rep(varName,length(data))
    }
  }else{}

  if(length(varNumber)==1){
    varNumberGraph <- 1:length(data)
    if(identical(varNumber,"")){
      varNumber <- 1:length(data)
    }else{
      varNumber <- rep(varNumber,length(data))
    }
  }else{}

  if(length(graphName)==1){
    graphName <- paste(graphName,varNumberGraph,sep="")
  }else{}

  classColumnList <- r2lFindClasses(data,limDiscrete)
  if(!identical(classModification,"")){classColumnList <- r2lModifyClasses(classColumnList,classModification)}else{}
  classColumn <- vector(mode="character",length=length(data))
  classColumn[classColumnList[["nominal"]]] <- "nominal"
  classColumn[classColumnList[["ordinal"]]] <- "ordinal"
  classColumn[classColumnList[["discrete"]]] <- "discrete"
  classColumn[classColumnList[["continuous"]]] <- "continuous"

  for (iColumn in 1:length(data)){
    class(data[,iColumn]) <- c(classColumn[iColumn],class(data[,iColumn]))
    r2lUniv(data[,iColumn],fileOutput="",varName[iColumn],varNumber[iColumn],dirGraph,graphName[iColumn])
  }
}

