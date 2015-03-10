library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)
library(forecast)
library(base)
library(gRain)
library(caTools)

setwd("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\pc1")

#sink("pc1.log.txt")

source("initialize.R")
source("start.R")
source("preprocess.R")
source("drawPlot.R")
source("bn.r")
source("TAN.R")
source("hc.R")
source("tabu.R")
source("mmhc.R")
source("HITON.R")
source("RSMAX2.R")
source("mmpc.R")
source("gs.R")
source("mmpc.R")
source("IAMB.R")
#source("fastIAMB.R")
#source("interIAMB.R")


initialize() ;


debug = FALSE

 i= 0
  for( i in 1:10){
   paste("In iteration",i ,sep= " ")
    startProcess(debug,i)
  }
  setwd("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\pc1")
  
