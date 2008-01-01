`r2lUniv.number` <-
function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",limDiscrete=10,...){
  if(length(table(data))<=limDiscrete && identical(as.numeric(data),as.numeric(as.integer(data)))){
    class(data) <- c("discrete",class(data))
    r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
  }else{
    class(data) <- c("continuous",class(data))
    r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
  }
}

