`r2lModifyClasses` <-
function(basicListClass,modification){
  basicListClass <- lapply(basicListClass,function(x){x[!x%in%unlist(modification)]})
  for(iBasicClass in c("nominal","ordinal","discrete","continuous")){
    basicListClass[[iBasicClass]] <- c(basicListClass[[iBasicClass]],modification[[iBasicClass]])
  }
  basicListClass <- basicListClass[sapply(basicListClass,length)>0]
  return(basicListClass)
}

