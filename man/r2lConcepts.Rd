\name{Concepts}
\alias{r2lConcepts}
%\alias{r2lUniv basic analysis}
%\alias{R classes}
%\alias{Why does r2lUniv not use R classes?}
%\alias{LaTeX summary}


\title{Concepts}

\description{
  Here are some basic statiscal concepts used by r2lUniv. Just to be sure we are
  speaking about the same things...
}
\section{r2lUniv basic classes}{
  Classically, statistics divide variables in four types. The
  classification is made according to some mathematical properties: 1) Are
  arithmetic operations possible? 2.a) Is there a natural order? 2.b) How many
  observations is there? 
  \enumerate{
    \item \emph{Nominal}: They have no mathematical properties. Example: [Name]
    or [Sex]. One cannot say that (Peter)<(John) or we cannot add
    (Peter) and (John) 
    \item \emph{Ordinal}: They can be ordered, but arithmetic operations
    are not
    possible. Example: [Pain] in {(Medium), (Low), (Strong)}. We have
    (Low)<(Medium)<(Strong), but (Low+Medium) / 2 makes no sense.  
    \item \emph{Discrete}: They can be ordered, arithmetic operations are possible but
    observations are only integer and there are only a few possible
    observations. Example: [childNumber]: (2) is less
    than (3), (2+3)/2 = 2.5 makes sense but a woman can not have 2.5
    children. 
    \item \emph{Continuous}: They can be ordered, arithmetic operations
    are possible and
    observations are any numbers. Example: [Weight]. (13.5) is more than
    (12.8), (13.6+12.8)/2 = 13.2 makes sense and (13.2) is a possible
    weight. 
  }
}

\section{r2lUniv basic analyses}{
  Due to these mathematical differences, statistical analyses and
  graphical representations of variables differ:  
  \enumerate{
    \item \emph{Nominal}: Frequency. Graph: barplot.
    \item \emph{Ordinal}: Frequency and quartile. Graph: barplot.
    \item \emph{Discrete}: Frequency, mean, variance, quartile. Graph: boxplot and barplot.
    \item \emph{Continuous}: Mean, variance, quartile. Graph: boxplot and
    histogram.
  }
}

\section{\R classes}{
  \R classification is close to the \emph{r2lUniv basic classes} but minor differences exist: 
  \enumerate{
    \item \R distinguishes two kinds of nominal: the logical and factor.
    \item \R does not detect ordinal variables, the user has to precise them.
    \item \R does not make differences between Discrete and Continuous
    (how could it? Even for human being, this distinction is not
    obvious...) 
  }
}

\section{Why does r2lUniv not use R classes?}{
  \code{r2lUniv} asks \R to perform automatically some analyses. The
  variable [childNumber] deserves a barplot and not a histogram
  whereas it is the opposite for [Weight]. For \R, both of these
  variables are \emph{numeric}. So to get to an automatic treatment,
  \code{r2lUniv} needs
  to set the class of [NumberOfChild] to \emph{Discrete}
  and the class of [Weight] to \emph{Continuous}. 
}


\section{Mapping between r2lUniv and \R classes}{
  Here is a summary of the mapping of \R classes into \emph{r2lUniv
    basic classes}:
  \itemize{
    \item \code{factor} becomes \code{nominal}.
    \item \code{logical} becomes \code{nominal}.
    \item \code{ordered} becomes \code{ordinal}.
    \item \code{integer} or \code{numeric} become either \code{discrete}
    or \code{continuous}. See \link{r2lFindClass} for details.
  } 
}

\section{LaTeX summary}{
  \code{r2lUniv} asks \R to perform automatically some analyses, then
  generates a LaTeX code to be included in a document. This LaTeX code is
  called \emph{LaTeX summary}.
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


\keyword{univar}       % Simple univariate statistics
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
\keyword{classes}      % Data type
\keyword{print}        % Printing
\keyword{package}