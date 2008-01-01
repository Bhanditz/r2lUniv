\name{r2lFindClasses}
\alias{r2lFindClasses}

\title{Determine Basic Classes for a data.frame}
\description{
  \code{r2lFindClasses} sets the \emph{basic type} of each column in a \code{data.frame}.
}
\usage{
r2lFindClasses(dataFrame, limDiscrete = 10)
}

\arguments{
  \item{dataFrame}{\code{dataFrame} whose type is to be set.}
  \item{limDiscrete}{\code{numeric}; fixes the limit that distinguishes continuous variables
    from discrete ones. See \link{r2lFindClass} for details.}
}

\details{
  \code{r2lFindClasses} is a generalisation of \link{r2lFindClass}.
  It calls \code{r2lFindClass} on each column of the \code{data.frame}.
  The return value is a four-fields list - one for each \emph{basic
    type}-. Each field contains the columns that have the corresponding
  \emph{basic type}. 
}
\value{
  A four-fields list:
  \item{Nominal}{column numbers found to be nominal}
  \item{Ordinal}{column numbers found to be ordinal}
  \item{Discrete}{column numbers found to be discrete}
  \item{Continuous}{column numbers found to be continuous}
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


\seealso{
  \code{\link{r2lUniv}},
  \code{\link{r2lUniv.data.frame}},
  \code{\link{r2lFindClasses}},
  \code{\link{r2lModifyClasses}}
}

\examples{
 # # # # # # # # # # # # # # # # # # #
#   R to LaTeX, Univariate Analyses   #
 #             Examples              # 
  #         r2lFindClasses          #
   # # # # # # # # # # # # # # # # #
	
data(examCheating)

### Without modification in examCheating
r2lFindClasses(examCheating)

### With modifications
examCheating$Id <- factor(examCheating$Id)
examCheating$YearOfStudy <- ordered(examCheating$YearOfStudy,levels=c("L1","L2","L3","M1","M2"))
examCheating$Bac <- ordered(examCheating$Bac,levels=c("Remedial exam","Pass","Fairly good","Good","Very good","Summa cum laude"))
for(iColumn in 8:17){examCheating[,iColumn] <- ordered(examCheating[,iColumn],levels=c("Never","Rarely","Sometimes","Often","Always"))}
str(examCheating)
r2lFindClasses(examCheating)
r2lFindClasses(examCheating,3)

### Special case: only one class
r2lFindClasses(examCheating[,8:12])
}

\keyword{univar}       % Simple univariate statistics
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
\keyword{classes}      % Data type

