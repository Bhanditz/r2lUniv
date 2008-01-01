`r2lUniv.nominal` <-
function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivNominal(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivNominal(data,varName,varNumber,dirGraph,graphName)
  }
}

