source("./R/rtlu.r")





r2lBegin <- function(){
  cat("  \\begin{center}\n")
  cat("%    \\addtolength{\\leftskip}{-4cm}\\addtolength{\\rightskip}{-4cm}\n")
}
r2lBegin()
cleanProg(r2lBegin)


cat("
##################################
###         r2lBeginTitle")
### Print (optional) the number and the name of a variable
### Print the variable type, the \begin{tabular} and the title of each column.
### The number of column and the title change according to the variable type.
### Used in: r2lNominal, r2lOrdinal, r2lDiscrete, r2lContinuous

# variable      : variable contening the data
# varName       : name of the variable
# varNumber     : number of the variable

variable  = Vnominal
varName   = "titre"
varNumber = 3
r2lBeginTitle <- function(variable,varName=NA,varNumber=NA){
  if(!is.na(varName) || !is.na(varNumber)){
    title <- "{\\large \\bf "
    if(!is.na(varNumber)){
      title <- paste(title,varNumber,". ",sep="")
    }else{}
    if(!is.na(varName)){
      title <- paste(title,varName,sep="")
    }else{}
    title <- paste(title,"}",sep="")
  }else{
    title <- NA
  }

  if(class(variable)[1]=="nominal"){
    nbColumn <- 2
    variableType <- "Nominal"
    columnTitle <- "{\\bf Frequency} & {\\bf Histogram}"
  }else{}
  if(class(variable)[1]=="ordinal"){
    nbColumn <- 3
    variableType <- "Ordinal"
    columnTitle <- "{\\bf Frequency} & {\\bf Summary} & {\\bf Histogram}"
  }else{}
  if(class(variable)[1]=="discrete"){
    nbColumn <- 4
    variableType <- "Discrete"
    columnTitle <- "{\\bf Frequency} & {\\bf Summary} & {\\bf Boxplot} & {\\bf Histogram}"
  }else{}
  if(class(variable)[1]=="continuous"){
    nbColumn <- 3
    variableType <- "Continuous"
    columnTitle <- "{\\bf Summary} & {\\bf Boxplot} & {\\bf Histogram}"
  }else{}

  cat("    \\begin{tabular}{|",rep("c|",nbColumn),"}\n",sep="")
  if(!is.na(title)){
    cat("        \\multicolumn{",nbColumn,"}{c}{",title,"} \\\\ \n",sep="")
  }else{}
  cat("      \\hline
        \\multicolumn{",nbColumn,"}{|l|}{",
          variableType," \\hfill N=",length(variable),
          " ; NA=",sum(is.na(variable))," (",format(sum(is.na(variable))/length(variable)*100,digits=3),"\\%)}\\\\
      \\hline
        ",columnTitle,"  \\\\
  ",sep="")
}
cleanProg(r2lBeginTitle(variable))
r2lBeginTitle(variable)
r2lBeginTitle(Vnominal)
r2lBeginTitle(Vordinal,"title")
r2lBeginTitle(Vdiscrete,,4)
r2lBeginTitle(Vcontinuous,"title",4)


cat("
###################################
###               r2lFrequency")
### Print each Frequency, its size and frequency
### Used in: r2lNominal, r2lOrdinal, r2lDiscrete

variable   = Vnominal
latexNext  = "&"

r2lFrequency <- function(variable,latexNext){
  cat("      \\begin{tabular}{@{}l@{ : }cl@{}}\n")
  tableVar <- table(variable)
  sumVar <- sum(tableVar)
  for(i in 1:nrow(tableVar)){
    cat("        ",labels(tableVar[i]),"&",tableVar[i][[1]],"&(")
    cat(format(tableVar[i][[1]]/sumVar*100,digits=3))
    cat("\\%) \\\\\n")
  }
  cat("      \\end{tabular}\n")
  cat("   ",latexNext)
}
cleanProg(r2lFrequency(variable,latexNext))
r2lFrequency(variable,latexNext)
r2lFrequency(Vnominal,latexNext)
r2lFrequency(Vordinal,latexNext)
r2lFrequency(Vdiscrete,latexNext)


cat("
##################################
###                r2lSummary")
### Print quartiles (for all), mean and var (for discrete and continuous)
### Used in: r2lOrdinal, r2lDiscrete, r2lContinuous

variable   = Vdiscrete
latexNext  = "&"

r2lSummary <- function(variable,latexNext){
  cat("      \\begin{tabular}{@{}l@{ : }l@{}}\n")
  if(class(variable)[1]=="ordinal"){
    quartile <- summary(as.integer(variable))
    niveaux <- levels(variable)
    cat("        Min.   &",niveaux[quartile[1]],"\\\\\n")
    cat("        Q1     &",niveaux[round(quartile[2])],"\\\\\n")
    cat("        Median &",niveaux[round(quartile[3])],"\\\\\n")
    cat("        Q3     &",niveaux[round(quartile[5])],"\\\\\n")
    cat("        Max.   &",niveaux[quartile[6]],"\\\\\n")
  }else{
    quartile <- summary(variable)
    cat("        Mean   &",format(mean(na.omit(variable)),digits=3),"\\\\\n")
    cat("        Var.   &",format(var(na.omit(variable)),digits=3),"\\\\\n")
    cat("        SD     &",format(sd(na.omit(variable)),digits=3),"\\\\ \n      \\hline \n")
    cat("        Min.   &",format(quartile[1],digits=3),"\\\\\n")
    cat("        Q1     &",format(quartile[2],digits=3),"\\\\\n")
    cat("        Median &",format(quartile[3],digits=3),"\\\\\n")
    cat("        Q3     &",format(quartile[5],digits=3),"\\\\\n")
    cat("        Max.   &",format(quartile[6],digits=3),"\\\\\n")
  }
  cat("      \\end{tabular}\n")
  cat("   ",latexNext)
}
cleanProg(r2lSummary(variable,latexNext))
r2lSummary(variable,latexNext)
r2lSummary(Vordinal,latexNext)
r2lSummary(Vdiscrete,latexNext)
r2lSummary(Vcontinuous,latexNext)


cat("
##################################
###         r2lBarplot")
### Compute a barplot, save it in a postscript file and print the LaTeX code
### Used in: r2lNominal, r2lOrdinal, r2lDiscrete

variable  = Vdiscrete
variable  = Vnominal
latexNext = "&"
dirGraph  = "dirGraph"
graphName = "V"
r2lBarplot <- function(variable,latexNext,dirGraph="",graphName="V"){
  if(dirGraph==""){
    nomPSbarplot <- paste(graphName,"-barplot.eps",sep="")
  }else{
    nomPSbarplot <- paste(dirGraph,"/",graphName,"-barplot.eps",sep="")
  }
  if(class(variable)[1]=="discrete"){
    tabVar <- table(c(variable,range(na.omit(variable))[1]:range(na.omit(variable))[2]))-1
  }else{
    tabVar <- table(variable)
  }
  postscript(file=nomPSbarplot,horizontal=FALSE,width=60,height=60)
    barplot(tabVar,col="grey",main="",xlab="",ylab="")
  dev.off()
  cat("      \\begin{tabular}{@{}l@{}}
        \\epsfig{figure=",nomPSbarplot,",width=3cm}
      \\end{tabular}
   ",latexNext)
}
cleanProg(r2lBarplot(variable,"&"))
r2lBarplot(variable,"&")
r2lBarplot(Vdiscrete,"&",dirGraph)
r2lBarplot(Vnominal,"&",dirGraph)
r2lBarplot(Vordinal,"&",,"nameEssai")
r2lBarplot(Vordinal,"&",dirGraph,"nameEssai")

cat("
##################################
###         r2lHist")
### Compute a histogram, save it in a postscript file and print the LaTeX code
### Used in: r2lContinuous

variable  = Vcontinuous
latexNext = "&"
dirGraph  = "dirGraph"
graphName = "V"
r2lHist <- function(variable,latexNext,dirGraph="",graphName="V"){
  if(dirGraph==""){
    nomPShist <- paste(graphName,"-barplot.eps",sep="")
  }else{
    nomPShist <- paste(dirGraph,"/",graphName,"-hist.eps",sep="")
  }
  postscript(file=nomPShist,horizontal=FALSE,width=60,height=60)
    hist(variable,col="grey",main="",xlab="",ylab="")
  dev.off()
  cat("
      \\begin{tabular}{@{}l@{}}
        \\epsfig{figure=",nomPShist,",width=3cm}
      \\end{tabular}
   ",latexNext)
}
cleanProg(r2lHist(variable,latexNext,dirGraph))
r2lHist(variable,latexNext,dirGraph)
r2lHist(Vcontinuous,"&")

cat("
##################################
###         r2lBoxplot")
### Compute a boxplot, save it in a postscript file and print the LaTeX code
### Used in: r2lDiscrete, r2lContinuous

variable  = Vdiscrete
latexNext = "&"
dirGraph  = ""
graphName = "V"
r2lBoxplot <- function(variable,latexNext,dirGraph="",graphName="V"){
  if(dirGraph==""){
    nomPSboxplot <- paste(graphName,"-boxplot.eps",sep="")
  }else{
    nomPSboxplot <- paste(dirGraph,"/",graphName,"-boxplot.eps",sep="")
  }
  postscript(file=nomPSboxplot,horizontal=FALSE,width=40,height=80)
    boxplot(variable,main="",xlab="",ylab="")
  dev.off()
  cat("
      \\begin{tabular}{@{}l@{}}
        \\epsfig{figure=",nomPSboxplot,",width=2cm}
      \\end{tabular}
   ",latexNext)
}
cleanProg(r2lBoxplot(variable,latexNext,dirGraph,graphName="Name"))
r2lBoxplot(variable,latexNext,dirGraph,graphName="Name")
r2lBoxplot(Vdiscrete,"&",dirGraph="dirGraph",graphName="Name")
r2lBoxplot(Vcontinuous,latexNext,dirGraph="dirGraph",graphName="NameBis")


cat("
##################################
###         r2lEnd")
### End of the tabular and the begin
### Used in : r2lNominal, r2lOrdianl, r2lDiscrete, r2lContinuous
r2lEnd <- function(){
  cat("
    \\end{tabular}
  \\end{center}
  ");
}
cleanProg(r2lEnd())
r2lEnd()







cat("
####################################################################################
######################## r2l's Methods for Single variables ########################
####################################################################################")

cat("
#############################
###          r2lUniv")
r2lUniv <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  UseMethod("r2lUniv")
}

cat("
#######################
### Method : r2lUniv.nominal")

data  = Vnominal
varName   = "Color"
varNumber = NA
dirGraph  = "dirGraph"
graphName = "V"
r2lUnivNominal <- function(data,varName=NA,varNumber=NA,dirGraph="",graphName="V"){
  r2lBegin()
  r2lBeginTitle(data,varName,varNumber)
  r2lFrequency(data,"&\n")
  r2lBarplot(data,"\\\\ \\hline \n",dirGraph,graphName)
  r2lEnd()
}
cleanProg(r2lUnivNominal(data=Vnominal,varName="EyesColor",dirGraph="dirGraph",graphName="EC"))

r2lUniv.nominal <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivNominal(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivNominal(data,varName,varNumber,dirGraph,graphName)
  }
}
r2lUniv.nominal(Vnominal,fileOutput="inNom.tex",varName="EyesColor",varNumber=NA,dirGraph="dirGraph",graphName="nom")
r2lUniv(Vnominal,fileOutput="inNom.tex",varName="EyesColor",varNumber=NA,dirGraph="dirGraph",graphName="nom")
cleanProg(r2lUniv(Vnominal,"inNom.tex"))

cat("
#######################
### Method : r2lUniv.ordinal")

data  = Vordinal
varName   = "ExamResult"
varNumber = NA
dirGraph  = "dirGraph"
graphName = "V"

r2lUnivOrdinal <- function(data,varName=NA,varNumber=NA,dirGraph="",graphName="V"){
  r2lBegin()
  r2lBeginTitle(data,varName,varNumber)
  r2lFrequency(data,"&\n")
  r2lSummary(data,"&\n")
  r2lBarplot(data," \\\\ \\hline \n",dirGraph,graphName)
  r2lEnd()
}
cleanProg(r2lUnivOrdinal(data,varName,varNumber=NA,dirGraph="dirGraph",graphName="ord"))

r2lUniv.ordinal <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivOrdinal(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivOrdinal(data,varName,varNumber,dirGraph,graphName)
  }
}
r2lUniv.ordinal(Vordinal,fileOutput="inOrdi.tex",varName,varNumber=NA,dirGraph="dirGraph",graphName="ord")
r2lUniv(Vordinal,fileOutput="inOrdi.tex",varName,varNumber=NA,dirGraph="dirGraph",graphName="ord")
cleanProg(r2lUniv(Vordinal,"inOrdi.tex"))


cat("
#############################
### Méthode : r2lUniv.discrete")

data        = Vdiscrete
varName         = "NbChild"
varNumber       = NA
dirGraph        = "dirGraph"
graphName       = "dis"

r2lUnivDiscrete <- function(data,varName=NA,varNumber=NA,dirGraph="",graphName="V"){
  r2lBegin()
  r2lBeginTitle(data,varName,varNumber)
  r2lFrequency(data,"&\n")
  r2lSummary(data,"&\n")
  r2lBoxplot(data," & \n",dirGraph,graphName)
  r2lBarplot(data,"\\\\ \\hline \n",dirGraph,graphName)
  r2lEnd()
}
r2lUnivDiscrete(data=data,varName,dirGraph="dirGraph",graphName="dis")
cleanProg(r2lUnivDiscrete(data=data,varName,dirGraph="dirGraph",graphName="dis"))

r2lUniv.discrete <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivDiscrete(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivDiscrete(data,varName,varNumber,dirGraph,graphName)
  }
}
r2lUniv.discrete(Vdiscrete,fileOutput="inDis.tex",varName,varNumber=NA,dirGraph="dirGraph",graphName="dis")
r2lUniv.discrete(Vdiscrete,fileOutput="inDis.tex",varName,varNumber=NA,dirGraph="dirGraph",graphName="dis")
cleanProg(r2lUniv.discrete(Vdiscrete,fileOutput="inDis.tex"))


cat("
#############################
### Méthode : r2lUniv.continuous")

data        = Vcontinuous
varName         = "Weight"
varNumber       = NA
dirGraph        = "dirGraph"
graphName       = "con"

r2lUnivContinuous <- function(data,varName=NA,varNumber=NA,dirGraph="",graphName="V"){
  r2lBegin()
  r2lBeginTitle(data,varName,varNumber)
  r2lSummary(data,"&\n")
  r2lBoxplot(data,"&\n",dirGraph,graphName)
  r2lHist(data,"\\\\ \\hline \n",dirGraph,graphName)
  r2lEnd()
}
cleanProg(r2lUnivContinuous(data,varName,varNumber=NA,dirGraph,graphName))

r2lUniv.continuous <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivContinuous(data,varName,varNumber,dirGraph,graphName)
    sink()
  }else{
    r2lUnivContinuous(data,varName,varNumber,dirGraph,graphName)
  }
}
r2lUniv.continuous(data,fileOutput="inCon.tex",varName,varNumber=NA,dirGraph="dirGraph",graphName="num")
r2lUniv(Vcontinuous,"inCon.tex")
cleanProg(r2lUniv(Vcontinuous,"inCon.tex"))


cat("
#############################
### Méthode : r2lUniv.factor")
r2lUniv.factor <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  class(data) <- c("nominal",class(data))
  r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
}
r2lUniv(Vfactor,"inFac.tex")
cleanProg(r2lUniv(Vfactor,"inFac.tex"))


cat("
#############################
### Méthode : r2lUniv.logical")
r2lUniv.logical <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  class(data) <- c("nominal",class(data))
  r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
}
r2lUniv(Vlogical,"inLog.tex")
cleanProg(r2lUniv(Vlogical,"inLog.tex"))


cat("
#############################
### Méthode : r2lUniv.ordered")
r2lUniv.ordered <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",...){
  class(data) <- c("ordinal",class(data))
  r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
}
r2lUniv(Vordered,"inOrde.tex")
cleanProg(r2lUniv(Vordered,"inOrde.tex"))


cat("
#############################
### Méthode : r2lUniv.number")
r2lUniv.number <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",limDiscrete=10,...){
  if(length(table(data))<=limDiscrete && identical(as.numeric(data),as.numeric(as.integer(data)))){
    class(data) <- c("discrete",class(data))
    r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
  }else{
    class(data) <- c("continuous",class(data))
    r2lUniv(data,fileOutput,varName,varNumber,dirGraph,graphName)
  }
}


cat("
#############################
### Méthode : r2lUniv.integer")
r2lUniv.integer <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",limDiscrete=10,...){
  r2lUniv.number(data,fileOutput,varName,varNumber,dirGraph,graphName,limDiscrete)
}
r2lUniv(Vinteger,"inInt.tex")
r2lUniv(Vinteger,"inNum2.tex",limDiscrete=5)
cleanProg(r2lUniv(Vinteger,"inInt.tex"))


cat("
#############################
### Méthode : r2lUniv.numeric")
r2lUniv.numeric <- function(data,fileOutput="",varName=NA,varNumber=NA,dirGraph="",graphName="V",limDiscrete=10,...){
  r2lUniv.number(data,fileOutput,varName,varNumber,dirGraph,graphName,limDiscrete)
}
r2lUniv(Vnumeric,"inNum.tex")
r2lUniv(Vnumeric,"inNum2.tex",limDiscrete=20)
cleanProg(r2lUniv(Vnumeric,"inNum.tex"))







cat("
##############################################################################
######################## r2l's Methods for data.frame ########################
##############################################################################")

cat("
##############################
###           r2lFindClass")
### Set the class (nominal, ordinal, discrete, coutinous) of a variable
###  - The factor and logical become nominal
###  - ordered become ordinal
###  - numeric and integer become discrete or continuous, depending of the number of modality

variable    = Vnumeric
limDiscrete = 3
r2lFindClass <- function(variable,limDiscrete=10){
  classVar <- class(variable)
  if(classVar[1]=="factor" || classVar[1]=="logical"){classVar <- c("nominal",class(variable))}else{}
  if(classVar[1]=="ordered"){classVar <- c("ordinal",class(variable))}else{}
  if(classVar[1]=="integer" || classVar[1]=="numeric"){
    if(length(table(variable))<=limDiscrete && identical(as.numeric(variable),as.numeric(as.integer(variable)))){
      classVar <- c("discrete",class(variable))
    }else{
      classVar <- c("continuous",class(variable))
    }
  }else{}
  return(classVar)
}
cleanProg(r2lFindClass(dataFrame$sexe))
r2lFindClass(dataFrame$sexe)
r2lFindClass(dataFrame$result)
r2lFindClass(dataFrame$age)
r2lFindClass(dataFrame$age,3)
r2lFindClass(dataFrame$weigth)

cat("
##############################
###           r2lFindClasses")
### Set the classes of the column of a data.frame

limDiscrete = 10
r2lFindClasses <- function(dataFrame,limDiscrete=10){
  listClass <- list(nominal=c(),ordinal=c(),discrete=c(),continuous=c())
  for(i in 1:ncol(dataFrame)){
    classVar <- r2lFindClass(dataFrame[,i],limDiscrete)[1]
    classNum <-  switch(classVar,
        nominal=1,
        ordinal=2,
        discrete=3,
        continuous=4
      )
    listClass[[classNum]] <- c(listClass[[classNum]],i)
  }
  for(i in c("nominal","ordinal","discrete","continuous")){listClass[[i]]<-listClass[[i]]}
  return(listClass)
}
cleanProg(r2lFindClasses(dataFrame))
r2lFindClasses(dataFrame)
r2lFindClasses(dataFrame,5)

cat("
###############################
###          r2lModifyClasses")
### Modify the BacisListClass according to the type difining in modification
(basicListClass <- r2lFindClasses(dataFrame))
(modification <- list(nominal=c(2),continuous=c(5,4)))
r2lModifyClasses <- function(basicListClass,modification){
  basicListClass <- lapply(basicListClass,function(x){x[!x%in%unlist(modification)]})
  for(iBasicClass in c("nominal","ordinal","discrete","continuous")){
    basicListClass[[iBasicClass]] <- c(basicListClass[[iBasicClass]],modification[[iBasicClass]])
  }
  basicListClass <- basicListClass[sapply(basicListClass,length)>0]
  return(basicListClass)
}

cleanProg(r2lModifyClasses(basicListClass,modification),,,1) # 1 pour length
r2lModifyClasses(basicListClass,modification)




cat("
#############################
###            Method : r2lUniv.data.frame")
### varName :
###   If varName="", then the column name of the data.frame are use.
###   For no title, use varName=NA.
###   For user define title, use varName=list("ta","ttr",...)
###   If varName=="onlyOne", then it will be use for all.
### varNumber:
###   The use of varNumber is the same than varTitle: NA => no number, varNumber=c(1,3,...) for a list, varNumber="" for using 1:length(data.frame)
### dirGraph: empty or a unique name of a directory
### graphName: if graphName="sing", then it is duplicate using varNumber as index.
###   Warnings: if varNumber is define by the user with some number use more than once, some graph will have the same name and some will be erased.
### classModification: this list is modification of the defaut choise. If "", then the default are use for all.


varName     = "" #varName = NA; varName = "Sing"; varName = c("t","tt","a","aa","z","zz","t")
varNumber   = "" #varNumber = NA; varNumber   = c(1,1,2,3,3,6,3)

dirGraph    = "dirGraph"
graphName   = "V"
classModification = ""
limDiscrete = 10
r2lUnivDataFrame <- function(data,varName="",varNumber="",dirGraph="",graphName="V",limDiscrete=10,classModification=""){

  if(length(varName)==1){
    if(identical(varName,"")){
      varName <- names(data)
    }else{
      varName <- rep(varName,length(data))
    }
  }else{}

  if(length(varNumber)==1){
    varNumberGraph <- 1:length(data)
    if(identical(varNumber,"")){
      varNumber <- 1:length(data)
    }else{
      varNumber <- rep(varNumber,length(data))
    }
  }else{}

  if(length(graphName)==1){
    graphName <- paste(graphName,varNumberGraph,sep="")
  }else{}

  classColumnList <- r2lFindClasses(data,limDiscrete)
  if(!identical(classModification,"")){classColumnList <- r2lModifyClasses(classColumnList,classModification)}else{}
  classColumn <- vector(mode="character",length=length(data))
  classColumn[classColumnList[["nominal"]]] <- "nominal"
  classColumn[classColumnList[["ordinal"]]] <- "ordinal"
  classColumn[classColumnList[["discrete"]]] <- "discrete"
  classColumn[classColumnList[["continuous"]]] <- "continuous"

  for (iColumn in 1:length(data)){
    class(data[,iColumn]) <- c(classColumn[iColumn],class(data[,iColumn]))
    r2lUniv(data[,iColumn],fileOutput="",varName[iColumn],varNumber[iColumn],dirGraph,graphName[iColumn])
  }
}
cleanProg(r2lUnivDataFrame(dataFrame,varName,varNumber,dirGraph,graphName,limDiscrete,classModification))
r2lUnivDataFrame(dataFrame,varName,varNumber,dirGraph,graphName,limDiscrete,classModification)



r2lUniv.data.frame <- function(data,fileOutput="",varName="",varNumber="",dirGraph="",graphName="V",limDiscrete=10,classModification="",...){
  if(fileOutput!=""){
    sink(fileOutput)
    r2lUnivDataFrame(data,varName,varNumber,dirGraph,graphName,limDiscrete,classModification)
    sink()
  }else{
    r2lUnivDataFrame(data,varName,varNumber,dirGraph,graphName,limDiscrete,classModification)
  }
}
r2lUniv.data.frame(dataFrame,fileOutput="inDF.tex",varName,varNumber,dirGraph="dirGraph",graphName="DF")
r2lUniv(dataFrame,fileOutput="inDF2.tex")
cleanProg(r2lUniv(dataFrame,fileOutput="inDF2.tex"))


cat("
#############################
###            Method : r2lGenerateLatexMain")

r2lGenerateLatexMain <- function(fileList,fileName="main.tex"){
  sink(fileName)
  cat("
\\documentclass[a4paper,10pt]{article}
\\usepackage{epsfig}
\\title{R to LaTeX\\\\ Examples}
\\author{Me}
\\begin{document}
\\maketitle
")
  for (i in fileList){
    cat("\\input{",i,"}\n",sep="")
  }
  cat("\\end{document}")
  sink()
}

rm(list=c("basicListClass","classModification","cleanProg","dirGraph",
     "graphName","latexNext","limDiscrete","modification",
     "data","varName","dataFrame","varNumber"))
package.skeleton(name="r2lUniv2")


























