\name{r2lModifyClasses}
\alias{r2lModifyClasses}
\title{Modify basic classes}
\description{
  Given a list of classes (like one produced by \code{r2lFindClasses}),
  the \code{r2lModifyClasses} function lets the user change some of the classes.
}
\usage{
r2lModifyClasses(basicListClass, modification)
}

\arguments{
  \item{basicListClass}{A list of classes, usually produced by \code{r2lFindClasses}}
  \item{modification}{A list of classes, containing the classes to change.}
}
\details{
  To set the \emph{basic class},
  \code{\link{r2lFindClasses}} uses some rules. The user might decide to
  accept \code{r2lFindClasses} choices for most columns, but not for
  all. In this case, instead of setting the basic type one by one, he
  can provide a modification list that will be used to correct some
  of \code{r2lFindClasses} choices.\cr

  Note that the user does not really need to use this function: providing
  directly the modification list to \code{r2lUniv} will have the same
  effect since \code{r2lUniv} will use \code{r2lModifyClasses} on the
  modification list.
}
\value{
  A four-field list, like \code{r2lFindClasses}:
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
  #        r2lModifyClasses         #
   # # # # # # # # # # # # # # # # #
	
data(examCheating)

### Without modification in examCheating
(listClass <- r2lFindClasses(examCheating))

### We want Age to be treated as a continuous
changeInList <- list(continuous=c(2))
r2lModifyClasses(listClass,changeInList)

### ... and we also want some variables to be ordinal
changeInList <- list(continuous=c(2),ordinal=c(8:10,12))
r2lModifyClasses(listClass,changeInList)
}


\keyword{univar}       % Simple univariate statistics
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
\keyword{classes}      % Data type

