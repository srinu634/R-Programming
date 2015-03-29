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
library(pcalg)


dataset.name <<- "pc3" #Set the file name here


setwd(paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\code",sep=""))

#sink("pc1.log.txt")

source("initialize.R")
source("start.R")
source("preprocess.R")
source("drawPlot.R")
source("getpriorprobspos.R")
source("getpriorprobsneg.R")


initialize() ;


debug = FALSE
getwd();
 i= 0
  for( i in 1:1){
   paste("In iteration",i ,sep= " ")
    startProcess(debug,i)
  }

setwd(paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\",dataset.name,sep=""))
