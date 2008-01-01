`r2lSummary` <-
function(variable,latexNext){
  cat("      \\begin{tabular}{@{}l@{ : }l@{}}\n")
  if(class(variable)[1]=="ordinal"){
    quartile <- summary(as.integer(variable))
    niveaux <- levels(variable)
    cat("        Min.   &",niveaux[quartile[1]],"\\\\\n")
    cat("        Q1     &",niveaux[round(quartile[2])],"\\\\\n")
    cat("        Median &",niveaux[round(quartile[3])],"\\\\\n")
    cat("        Q3     &",niveaux[round(quartile[5])],"\\\\\n")
    cat("        Max.   &",niveaux[quartile[6]],"\\\\\n")
  }else{
    quartile <- summary(variable)
    cat("        Mean   &",format(mean(na.omit(variable)),digits=3),"\\\\\n")
    cat("        Var.   &",format(var(na.omit(variable)),digits=3),"\\\\\n")
    cat("        SD     &",format(sd(na.omit(variable)),digits=3),"\\\\ \n      \\hline \n")
    cat("        Min.   &",format(quartile[1],digits=3),"\\\\\n")
    cat("        Q1     &",format(quartile[2],digits=3),"\\\\\n")
    cat("        Median &",format(quartile[3],digits=3),"\\\\\n")
    cat("        Q3     &",format(quartile[5],digits=3),"\\\\\n")
    cat("        Max.   &",format(quartile[6],digits=3),"\\\\\n")
  }
  cat("      \\end{tabular}\n")
  cat("   ",latexNext)
}

