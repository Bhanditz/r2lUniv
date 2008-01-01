`r2lUniv.discrete` <-
function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivDiscrete(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivDiscrete(data,varName,varNumber,dirGraph,graphName)
  }
}

