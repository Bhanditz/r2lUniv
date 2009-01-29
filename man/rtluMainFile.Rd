\name{rtluMainFile}
\alias{rtluMainFile}

\title{Generation of LaTeX main document}
\description{
  This function generates a LaTeX main document design to include some files built by \code{rtlu}.
}
\usage{
rtluMainFile(fileList, fileName = "main.tex")
}

\arguments{
  \item{fileList}{\code{list}; list of the outfiles, files to be included in the main document.}
  \item{fileName}{\code{characters}; name of the main document.}
}
\details{
  \code{rtluMainFile} generates a LaTeX main document including some
  files built by \code{rtlu}.

  It creates a file "main.tex". You need to have a LaTeX compiler and
  \code{epsfig} package.
}
\value{
  A LaTeX file ("main.tex" by default).
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


\references{LaTeX web site \url{http://www.latex-project.org/}\cr
Data are available on line: \url{http://christophe.genolini.free.fr/EPO/EPO2007-Fraude.php}}



\seealso{
  \code{\link{rtlu}},
  \code{\link{r2lUniv-package}},
  \code{\link{examCheating}},
  \code{\link{Sweave}},
  \code{\link[Hmisc]{latex}}
}

\examples{
 # # # # # # # # # # # # # # # # # # #
#   R to LaTeX, Univariate Analyses   #
 #             Examples              #
  #          rtluMainFile           #
   # # # # # # # # # # # # # # # # #

### Creates a file name "main.tex" that includes "file1.tex" and "file2.tex"
rtluMainFile(c("file1.tex","file2.tex"))

### Creates a file name "main2.tex" that includes "file1.tex" and "file2.tex"
rtluMainFile(c("file1.tex","file2.tex"),fileName="main2.tex")
}


\keyword{print}        % Printing
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
