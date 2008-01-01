`r2lUniv.data.frame` <-
function(data,fileOutput="",varName="",varNumber="",dirGraph="",graphName="V",limDiscrete=10,classModification="",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivDataFrame(data,varName,varNumber,dirGraph,graphName,limDiscrete,classModification)
    sink()
  }else{
    r2lUnivDataFrame(data,varName,varNumber,dirGraph,graphName,limDiscrete,classModification)
  }
}

