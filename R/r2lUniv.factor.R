`r2lUniv.factor` <-
function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  class(data) <- c("nominal",class(data))
  r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
}

