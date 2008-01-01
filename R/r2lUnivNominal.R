`r2lUnivNominal` <-
function(data,varName=NA,varNumber=NA,dirGraph="",graphName="V"){
  r2lBegin()
  r2lBeginTitle(data,varName,varNumber)
  r2lFrequency(data,"&\n")
  r2lBarplot(data,"\\\\ \\hline \n",dirGraph,graphName)
  r2lEnd()
}

