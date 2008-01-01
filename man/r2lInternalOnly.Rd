\name{r2lInternalOnly}
\alias{r2lBarplot}
\alias{r2lBegin}
\alias{r2lBeginTitle}
\alias{r2lBoxplot}
\alias{r2lEnd}
\alias{r2lFrequency}
\alias{r2lHist}
\alias{r2lSummary}

\alias{r2lUnivNominal}
\alias{r2lUnivOrdinal}
\alias{r2lUnivDiscrete}
\alias{r2lUnivContinuous}

\alias{r2lUnivDataFrame}

\title{Internal functions}
\description{
  These functions are for internal uses only.
}
\usage{
r2lBarplot(variable, latexNext, dirGraph = "", graphName = "V")
r2lBegin()
r2lBeginTitle(variable, varName = NA, varNumber = NA)
r2lBoxplot(variable, latexNext, dirGraph = "", graphName = "V")
r2lEnd()
r2lFrequency(variable, latexNext)
r2lHist(variable, latexNext, dirGraph = "", graphName = "V")
r2lSummary(variable, latexNext)

r2lUnivNominal(data, varName = NA, varNumber = NA, dirGraph = "", graphName = "V")
r2lUnivOrdinal(data, varName = NA, varNumber = NA, dirGraph = "", graphName = "V")
r2lUnivDiscrete(data, varName = NA, varNumber = NA, dirGraph = "", graphName = "V")
r2lUnivContinuous(data, varName = NA, varNumber = NA, dirGraph = "", graphName = "V")

r2lUnivDataFrame(data, varName = "", varNumber = "", dirGraph = "", graphName = "V", limDiscrete = 10, classModification = "")
}

\arguments{}
\details{}
\value{}
\references{}
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
\seealso{}
\examples{}
\keyword{internal}

