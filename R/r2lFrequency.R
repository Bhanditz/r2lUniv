`r2lFrequency` <-
function(variable,latexNext){
  cat("      \\begin{tabular}{@{}l@{ : }cl@{}}\n")
  tableVar <- table(variable)
  sumVar <- sum(tableVar)
  for(i in 1:nrow(tableVar)){
    cat("        ",labels(tableVar[i]),"&",tableVar[i][[1]],"&(")
    cat(format(tableVar[i][[1]]/sumVar*100,digits=3))
    cat("\\%) \\\\\n")
  }
  cat("      \\end{tabular}\n")
  cat("   ",latexNext)
}

