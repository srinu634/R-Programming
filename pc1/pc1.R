library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)
library(forecast)




source("preprocess.R")
source("bn.r")
source("TAN.R")
source("hc.R")
source("mmhc.R")
source("tabu.R")
source("HITON.R")

debug = TRUE

preprocess(debug)

runBN(debug)

runTAN(debug)


runHC(debug)

runMMHC(debug)

runTABU(debug)

runHITON(debug)




###############Constraint Based Networks########################

#gs
#str(pc1.disc.data)
# Include an arc from class node to every other node
len <- length(pc1.disc.data) #Number of attributes
from <- NULL
for( i in 1:(len-1)){
  from <- c( from,c("Defective")) 
}
from
to <- NULL
attributes <- names(pc1)
for(i in 1:(len-1) ){
  to <- c(to, attributes[i]  )
}






#graphviz.plot(pc1.si.hiton.pc$arcs)

pc1.rsmax2 = empty.graph(attributes)
whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
#str(whitelist.arcs)
#names(whitelist.arcs)
pc1.rsmax2 = cextend (  rsmax2(pc1.disc.data,whitelist = NULL,debug=FALSE) ) # cextend :: makes sure that all edges are directed

pc1.rsmax2.fitted = bn.fit(pc1.rsmax2,pc1.disc.data)

pc1.rsmax2.pred<- predict(pc1.rsmax2.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data

table(pc1.rsmax2.pred, pc1.disc.data[, "Defective"]) #output the prediction matrix
#Change the outputs to numeric values; 
pc1.given.rsmax2 <- as.numeric(as.character(pc1.rsmax2.pred))
pc1.pred.rsmax2 <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
accuracy(f = pc1.given.rsmax2 , x = pc1.pred.rsmax2)  #print the accuracy

#graphviz.plot(pc1.rsmax2)






#to
# 
# names(pc1.disc.data) = names(pc1)
# pc1.gs = empty.graph(attributes)
# whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
# #str(whitelist.arcs)
# #names(whitelist.arcs)
# pc1.gs = cextend (  gs(pc1.disc.data,whitelist = NULL,debug=FALSE) ) # cextend :: makes sure that all edges are directed

# see set.arc to set the arc directions
#class(pc1.gs)
#modelstring(pc1.gs)
# arcs(pc1.gs)
#pc1.gs$arcs ## To see the info about arcs
#pc1.gs$nodes

# 
# pc1.gs.fitted = bn.fit(pc1.gs,pc1.disc.data)
# 
# pc1.gs.pred<- predict(pc1.gs.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data
# 
# table(pc1.gs.pred, pc1.disc.data[, "Defective"]) #output the prediction matrix
# #Change the outputs to numeric values; 
# pc1.given.gs <- as.numeric(as.character(pc1.gs.pred))
# pc1.pred.gs <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
# accuracy(f = pc1.given.gs , x = pc1.pred.gs)  #print the accuracy
# 
# 
# #graphviz.plot(pc1.gs)
# 







# pc1.iamb = empty.graph(attributes)
# whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
# #str(whitelist.arcs)
# #names(whitelist.arcs)
# pc1.iamb = cextend (  iamb(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
# 
# pc1.iamb.fitted = bn.fit(pc1.iamb,pc1.disc.data)
# 
# pc1.iamb.pred<- predict(pc1.iamb.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data
# 
# table(pc1.iamb.pred, pc1.disc.data[, "Defective"]) #output the prediction matrix
# #Change the outputs to numeric values; 
# pc1.given.iamb <- as.numeric(as.character(pc1.iamb.pred))
# pc1.pred.iamb <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
# accuracy(f = pc1.given.iamb , x = pc1.pred.iamb)  #print the accuracy
# 
# #graphviz.plot(pc1.iamb)
# 
# 
# 
# 










# pc1.fast.iamb = empty.graph(attributes)
# whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
# #str(whitelist.arcs)
# #names(whitelist.arcs)
# pc1.fast.iamb = cextend (  fast.iamb(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
# 
# pc1.fast.iamb.fitted = bn.fit(pc1.fast.iamb,pc1.disc.data)
# 
# pc1.fast.iamb.pred<- predict(pc1.fast.iamb.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data
# 
# table(pc1.fast.iamb.pred, pc1.disc.data[, "Defective"]) #output the prediction matrix
# #Change the outputs to numeric values; 
# pc1.given.fast.iamb <- as.numeric(as.character(pc1.fast.iamb.pred))
# pc1.pred.fast.iamb <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
# accuracy(f = pc1.given.fast.iamb , x = pc1.pred.fast.iamb)  #print the accuracy
# 
# #graphviz.plot(pc1.fast.iamb)
# 
# 
# 
# 
# 
# 








# pc1.inter.iamb = empty.graph(attributes)
# whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
# #str(whitelist.arcs)
# #names(whitelist.arcs)
# pc1.inter.iamb = cextend (  inter.iamb(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
# 
# pc1.inter.iamb.fitted = bn.fit(pc1.inter.iamb,pc1.disc.data)
# 
# pc1.inter.iamb.pred<- predict(pc1.inter.iamb.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data
# 
# table(pc1.inter.iamb.pred, pc1.disc.data[, "Defective"]) #output the prediction matrix
# #Change the outputs to numeric values; 
# pc1.given.inter.iamb <- as.numeric(as.character(pc1.inter.iamb.pred))
# pc1.pred.inter.iamb <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
# accuracy(f = pc1.given.inter.iamb , x = pc1.pred.inter.iamb)  #print the accuracy
# 
# #graphviz.plot(pc1.inter.iamb)
# 
# 











pc1.mmpc = empty.graph(attributes)
whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
#str(whitelist.arcs)
#names(whitelist.arcs)
pc1.mmpc = cextend (  mmpc(pc1.disc.data,whitelist = NULL,debug=FALSE) ) # cextend :: makes sure that all edges are directed

pc1.mmpc.fitted = bn.fit(pc1.mmpc,pc1.disc.data,method = "mle")

pc1.mmpc.pred<- predict(pc1.mmpc.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data

table(pc1.mmpc.pred, pc1.disc.data[, "Defective"]) #output the prediction matrix
#Change the outputs to numeric values; 
pc1.given.mmpc <- as.numeric(as.character(pc1.mmpc.pred))
pc1.pred.mmpc <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
accuracy(f = pc1.given.mmpc , x = pc1.pred.mmpc)  #print the accuracy

#graphviz.plot(pc1.mmpc)
