`r2lUniv.ordinal` <-
function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivOrdinal(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivOrdinal(data,varName,varNumber,dirGraph,graphName)
  }
}

