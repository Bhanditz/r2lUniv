`r2lUniv.ordered` <-
function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  class(data) <- c("ordinal",class(data))
  r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
}

