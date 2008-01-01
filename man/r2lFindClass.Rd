\name{r2lFindClass}
\alias{r2lFindClass}
\title{Determine Basic Classes for a vector}
\description{
  \code{r2lFindClass} sets the \emph{basic type} of a \code{vector}.
}
\usage{
r2lFindClass(variable, limDiscrete = 10)
}

\arguments{
  \item{variable}{\code{vector} whose type is to be set.}
  \item{limDiscrete}{\code{numberic}; fixes the limit that distinguishes continuous variables
    from discrete ones. See details below.}
}
\details{
  \code{r2lFindClass} determines the \emph{basic type} of a vector, as
  defined in \link{r2lUniv}. The rules are the following:
  
  \itemize{
    \item \code{factor} becomes \code{nominal}.
    \item \code{logical} becomes \code{nominal}.
    \item \code{ordered} becomes \code{ordinal}.
    \item \code{integer} or \code{numeric} become either \code{discrete}
    or \code{continuous}: 
    \itemize{
      \item If the variable has only "few" modalities
      (like [childNumber] or [Goal] in soccer) and is vector of "integer", then
      it becomes \code{discrete}
      \item Otherwise, if it takes a lot of different values or is a
      decimal vector, it becomes \code{continuous}
      \item Warning: in \R, a vector of "integer" will have the class
      \code{numeric}. We do not here consider the class, but the nature
      of the variable.
      \item "few" is user-defined through the parameter
      \code{limDiscrete}. If a \code{vector} has more than
      \code{limDiscrete} different observations, it will be
      \code{continuous}. Otherwise, it is elligible for being
      \code{discrete} (if the second condition holds).
    }
  } 
}

\value{A vector of characters containing in first position the
  \link[=r2lConcepts]{r2l basic class}, then the \R class of the object (the \R class is
  not erased by \code{r2lFindClass}, it just becomes secondary class)
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
  #          r2lFindClass           #
   # # # # # # # # # # # # # # # # # 

### Example for each R classes
data(Vfactor)
str(Vfactor)
r2lFindClass(Vfactor)

data(Vlogical)
str(Vlogical)
r2lFindClass(Vlogical)

data(Vordered)
str(Vordered)
r2lFindClass(Vordered)

data(Vinteger)
str(Vinteger)
r2lFindClass(Vinteger)
r2lFindClass(Vinteger,4)

data(Vnumeric)
str(Vnumeric)
r2lFindClass(Vnumeric)
r2lFindClass(Vnumeric,30)
}

\keyword{univar}       % Simple univariate statistics
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
\keyword{classes}      % Data type
