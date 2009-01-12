source("./testsFunction.r")

cat("
####################################################################################
######################## r2l's Methods for Single variables ########################
####################################################################################")

cat("
#######################
### Method : r2lUniv.factor")

r2lUniv.factor(Vfactor)
r2lUniv.logical(Vlogical)
r2lUniv.character(as.character(Vfactor))


cat("
#######################
### Method : r2lUniv.ordered")

r2lUniv.ordered(Vordered,textAfter="toto",graphDir="graphUniv",graphName="ord")


cat("
#############################
### Méthode : r2lUniv.discrete")

r2lUniv.discrete(Vdiscrete,graphDir="graphUniv",graphName="dis")
r2lUniv.discrete(Vcontinuous,graphDir="graphUniv",graphName="dis")
r2lUniv.continuous(Vdiscrete,graphDir="graphUniv",graphName="con")
r2lUniv.continuous(Vcontinuous,graphDir="graphUniv",graphName="con")

r2lUniv.integer(Vdiscrete,graphDir="graphUniv",graphName="dis")
r2lUniv.integer(Vdiscrete,graphDir="graphUniv",graphName="dis",limDiscrete=3)
r2lUniv.integer(Vcontinuous,graphDir="graphUniv",graphName="dis")
r2lUniv.integer(Vcontinuous,graphDir="graphUniv",graphName="dis",limDiscrete=100)


cat("
#############################
### Method : r2lUniv.data.frame")

r2lUniv.data.frame(examCheating)
r2lUniv.data.frame(examCheating,limDiscrete=50)

r2lUniv(Vfactor)
r2lUniv(Vlogical)
r2lUniv(Vordered)
r2lUniv(Vdiscrete)
r2lUniv(Vnumeric)
r2lUniv(examCheating)


cat("
#############################
### rtlu")

rtlu(Vfactor)
rtlu(Vlogical)
rtlu(Vordered)
rtlu(Vdiscrete)
rtlu(Vnumeric)
rtlu(examCheating)


















