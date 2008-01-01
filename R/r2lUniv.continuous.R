`r2lUniv.continuous` <-
function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivContinuous(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivContinuous(data,varName,varNumber,dirGraph,graphName)
  }
}

