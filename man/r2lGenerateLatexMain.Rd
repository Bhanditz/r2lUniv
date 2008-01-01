\name{r2lGenerateLatexMain}
\alias{r2lGenerateLatexMain}

\title{Generation of LaTeX main document}
\description{
  This function generates a LaTeX main document including some \link[=r2lConcepts]{LaTeX summary} built by \code{r2lUniv}.
}
\usage{
r2lGenerateLatexMain(fileList, fileName = "main.tex")
}

\arguments{
  \item{fileList}{\code{list}; list of the outfiles, files to be included in the main document.}
  \item{fileName}{\code{characters}; name of the main document.}
}
\details{
  \code{r2lGenerateLatexMain} generates a LaTeX main document including some 
  \link[=r2lConcepts]{LaTeX summary} built by \code{r2lUniv}.

  It creates a file "main.tex". You need to have a LaTeX compiler and
  \code{epsfig} package.
  If outfiles given as argument do exist (in the same
  directory), compiling \code{main.tex} will produce a \code{dvi}
  file that will present the \link[=r2lConcepts]{LaTeX summary}. 
}
\value{
  A file ("main.tex" by default).
}



\seealso{\code{\link{r2lUniv}},
  \code{\link{r2lUniv.data.frame}},
  \code{\link{r2lUniv.r2lBasicClasses}}
}
\section{Author}{
  Christophe Genolini \cr
  \email{christophe.genolini@free.fr}\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health \cr
  INSERM U669 / Maison de Solenn / Paris
}

\section{English correction}{
  Jean-Marc Chamot \cr
  \email{jchamot@u-paris10.fr}\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}


\examples{
 # # # # # # # # # # # # # # # # # # #
#   R to LaTeX, Univariate Analyses   #
 #             Examples              # 
  #      r2lGenerateLatexMain       #
   # # # # # # # # # # # # # # # # #

### Creates a file name "main.tex" that includes "file1.tex" and "file2.tex"
r2lGenerateLatexMain(c("file1.tex","file2.tex"))

### Creates a file name "main2.tex" that includes "file1.tex" and "file2.tex"
r2lGenerateLatexMain(c("file1.tex","file2.tex"),fileName="main2.tex")
}

\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
\keyword{print}        % Printing
