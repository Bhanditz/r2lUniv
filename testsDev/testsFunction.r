load("../data/examCheating.rda")
source("../R/function.r")
source("../R/rtlu.r")

Vfactor <- examCheating$Field
Vlogical <- examCheating$BacCheat
Vordered <- examCheating$YearOfStudy <- as.ordered(examCheating$YearOfStudy)
Vdiscrete <- examCheating$Age
class(Vdiscrete) <- "discrete"
Vnumeric <- Vcontinuous <- examCheating$CheatScore
class(Vcontinuous) <- "continuous"


cat("
###################################
###         r2lBeginTitle")

r2lBeginTitle(Vfactor)
r2lBeginTitle(Vlogical)
r2lBeginTitle(Vordered)
r2lBeginTitle(Vdiscrete)
r2lBeginTitle(Vcontinuous)



cat("
###################################
###               r2lFrequency")

r2lFrequency(Vfactor)#,latexNext)
r2lFrequency(Vlogical)#,latexNext)
r2lFrequency(Vordered)#,latexNext)
r2lFrequency(Vdiscrete)#,latexNext)
r2lFrequency(Vcontinuous)#,latexNext)


cat("
##################################
###                r2lSummary")

r2lSummary(Vordinal)
r2lSummary(Vdiscrete)
r2lSummary(Vcontinuous)


cat("
##################################
###         r2lBarplot")

r2lBarplot(Vfactor)
r2lBarplot(Vdiscrete,graphDir="graphUniv")
r2lBarplot(Vlogical,graphDir="graphUniv")
r2lBarplot(Vordinal,graphName="nameEssai")
r2lBarplot(Vordinal,graphDir="graphUniv",type="pdf")

cat("
##################################
###         r2lHist")

r2lHist(Vdiscrete,graphDir="graphUniv")
r2lHist(Vcontinuous)

cat("
##################################
###         r2lBoxplot")

r2lBoxplot(Vdiscrete,graphDir="graphUniv",graphName="Name")
r2lBoxplot(Vcontinuous,graphDir="graphUniv",graphName="NameBis")


cat("
##################################
###         r2lEnd")

r2lEnd()










