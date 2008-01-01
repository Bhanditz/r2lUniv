`r2lFindClasses` <-
function(dataFrame,limDiscrete=10){
  listClass <- list(nominal=c(),ordinal=c(),discrete=c(),continuous=c())
  for(i in 1:ncol(dataFrame)){
    classVar <- r2lFindClass(dataFrame[,i],limDiscrete)[1]
    classNum <-  switch(classVar,
        nominal=1,
        ordinal=2,
        discrete=3,
        continuous=4
      )
    listClass[[classNum]] <- c(listClass[[classNum]],i)
  }
  for(i in c("nominal","ordinal","discrete","continuous")){listClass[[i]]<-listClass[[i]]}
  return(listClass)
}

