\name{examCheating}
\alias{examCheating}

\alias{Vnominal}
\alias{Vordinal}
\alias{Vdiscrete}
\alias{Vcontinuous}

\alias{Vfactor}
\alias{Vlogical}
\alias{Vordered}
\alias{Vinteger}
\alias{Vnumeric}

\docType{data}
\title{Exam Cheating at French University}
\description{
  This data set results from an enquiry leads by some second year
  Students from University Paris X - Nanterre. They build up a
  questionnaire to measure fraud during exam at university. This
  is NOT a validate questionnaire, just an exercise done by some students
  on a topic they choose -to make statistics learning more interactif
  (more fun ?)-.
}

\usage{
data(examCheating)
data(Vnominal)
data(Vordinal)
data(Vdiscrete)
data(Vcontinuous)
data(Vfactor)
data(Vlogical)
data(Vordered)
data(Vinteger)
data(Vnumeric)
}
\format{
  A data frame with 314 observations on the following 20 variables.
  \describe{
    \item{\code{Id}}{Line identification}
    \item{\code{Age}}{\emph{What is your age ?}}
    \item{\code{Gender}}{\emph{What is your gender ?}}
    \item{\code{YearOfStudy}}{\emph{What is your studying year ?}\cr
      Note: In European University, the studies are
      split in two cycles. The first called "Licence" last three years (L1, L2
      then L3). The second called "Master" (M1 then M2) takes place
      after the licence and takes two years.}
    \item{\code{Field}}{\emph{What do you study ?}\cr
      Note:\code{LEA} is "Foreigh langage",
      \code{SEGMI} is "Math", \code{SJAP} is "Law", \code{SSA} is "",
      \code{STAPS} is "Sports"}
    \item{\code{RepeatingYear}}{\emph{Did you ever takes a class twice ?}\cr
      In France, a student that fail the final exam will be asked to
      take the same year of study once again.}
    \item{\code{Bac}}{\emph{What grade did you get at Bac ?} \cr
      Bac is a french national exam that take place between high school
      and university. The possible result are \code{Fail}
      \code{Remedial exam} \code{Pass} \code{Fairly good} \code{Good}
      \code{Very good}  \code{Summa cum laude}} 
    \item{\code{Peep}}{\emph{Did you ever peep during exams ?}}
    \item{\code{Whispering}}{\emph{Did you ever whisper to neighbors during exams ?}}
    \item{\code{PaperSwapping}}{\emph{Did you ever swap rough paper during exams ?}}
    \item{\code{CribSheet}}{\emph{Did you ever use crib sheet ?}}
    \item{\code{SMS}}{\emph{Did you ever send SMS during exams ?}}
    \item{\code{LapCopying}}{\emph{Did you ever lap copy during exams ?}}
    \item{\code{PaperKeeping}}{\emph{Did you ever keep your paper ?}}
    \item{\code{PreparingRoom}}{\emph{Did you ever prepare the exam room
	or the toilets before exams ?}}
    \item{\code{ExamStealing}}{\emph{Did you ever steal the exam subject
	before the exam ?}}
    \item{\code{Other}}{\emph{Did you use some other way of cheating ?}}
    \item{\code{BacCheat}}{\emph{Did you cheat during the Bac ?}}
    \item{\code{HighSchoolCheat}}{\emph{Did you cheat during you high school
	studies ?}}
    \item{\code{CheatScore}}{Sum of all the cheating
      variables}{with convention \code{Never=0} \code{Rarely=1} 
      \code{Sometimes=2} \code{Often=3} \code{Always=4})}
  }
}

\details{
  \itemize{
    \item{\code{examCheating}}{All the variable}
    \item{\code{Vnominal}}{Column \code{Field}}
    \item{\code{Vordinal}}{Column \code{Peep}}
    \item{\code{Vdiscrete}}{Column \code{Age}}
    \item{\code{Vcontinous}}{Column \code{CheatScore}}
    \item{\code{Vfactor}}{Column \code{Field}}
    \item{\code{Vlogical}}{Column \code{RepeatingYear}}
    \item{\code{Vordered}}{Column \code{Peep}}
    \item{\code{Vinteger}}{Column \code{Age}}
    \item{\code{Vnumeric}}{Column \code{CheatScore}}
  }
}

\section{Author (lecturer)}{
  Christophe Genolini \cr
  \email{christophe.genolini@free.fr}\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health \cr
  INSERM U669 / Maison de Solenn / Paris
}

\section{Author (students)}{L2 (2007-2008) groups TD1 and TD2}


\examples{
data(examCheating)
str(examCheating)

data(Vnominal)
str(Vnominal)

data(Vordinal)
str(Vordinal)

### and so on...
}
\keyword{datasets}
