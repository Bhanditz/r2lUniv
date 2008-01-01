`r2lGenerateLatexMain` <-
function(fileList,fileName="main.tex"){
  sink(fileName)
  cat("
\\documentclass[a4paper,10pt]{article}
\\usepackage{epsfig}
\\title{R to LaTeX\\\\ Examples}
\\author{Me}
\\begin{document}
\\maketitle
")
  for (i in fileList){
    cat("\\input{",i,"}\n",sep="")
  }
  cat("\\end{document}")
  sink()
}

