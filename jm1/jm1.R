library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)
library(forecast)
library(base)

setwd("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\jm1")

#sink("jm1.log.txt")

source("preprocess.R")
source("bn.r")
source("TAN.R")
source("hc.R")
source("mmhc.R")
source("tabu.R")
source("HITON.R")
source("RSMAX2.R")
source("mmpc.R")
source("gs.R")
source("IAMB.R")
source("mmpc.R")
source("fastIAMB.R")
source("interIAMB.R")



debug = FALSE

preprocess(debug)

#General Bayesian + TAN

runBN(debug)

runTAN(debug)

#General Bayesian Networks

attributes <<- names(jm1.disc.data) #Global variable
whitelist.arcs <<- NULL

#########Score Based


runHC(debug) 
runTABU(debug)


#########Hybrid

runMMHC(debug)
runRSMAX2(debug)
runHITON(debug)



#Constraint Based

runGS(debug)
runIAMB(debug)
runMMPC(debug)
runFASTIAMB(debug)
runINTERIAMB(debug)





# Include an arc from class node to every other node
len <<- length(jm1.disc.data) #Number of attributes
from <<- NULL
for( i in 1:(len-1)){
  from <- c( from,c("v")) 
}
#from
to <<- NULL
attributes <- names(jm1.disc.data)
for(i in 1:(len-1) ){
  to <<- c(to, attributes[i]  )
}
#to

whitelist.arcs <<- data.frame(from,to)
setwd("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\jm1\\plots")

#Build a BAN by including arc from Classification Node to every other node


#runHC(debug) 
#runTABU(debug)


#########Hybrid

runMMHC(debug)
runRSMAX2(debug)
runHITON(debug)



#Constraint Based

runGS(debug)
runIAMB(debug)
runMMPC(debug)
runFASTIAMB(debug)
runINTERIAMB(debug)
