`r2lFindClass` <-
function(variable,limDiscrete=10){
  classVar <- class(variable)
  if(classVar[1]=="factor" || classVar[1]=="logical"){classVar <- c("nominal",class(variable))}else{}
  if(classVar[1]=="ordered"){classVar <- c("ordinal",class(variable))}else{}
  if(classVar[1]=="integer" || classVar[1]=="numeric"){
    if(length(table(variable))<=limDiscrete && identical(as.numeric(variable),as.numeric(as.integer(variable)))){
      classVar <- c("discrete",class(variable))
    }else{
      classVar <- c("continuous",class(variable))
    }
  }else{}
  return(classVar)
}

