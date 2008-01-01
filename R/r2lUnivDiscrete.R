`r2lUnivDiscrete` <-
function(data,varName=NA,varNumber=NA,dirGraph="",graphName="V"){
  r2lBegin()
  r2lBeginTitle(data,varName,varNumber)
  r2lFrequency(data,"&\n")
  r2lSummary(data,"&\n")
  r2lBoxplot(data," & \n",dirGraph,graphName)
  r2lBarplot(data,"\\\\ \\hline \n",dirGraph,graphName)
  r2lEnd()
}

