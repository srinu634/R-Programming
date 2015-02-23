library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)


pc1 = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc1.arff") #Load the dataset


pc1.test = pc1[600:705,] # Build a Test set
#pc1 = pc1[1:599,]

# str(pc1) 

# Change the Strings Y,N to 1,0
pc1$Defective <- as.numeric(factor(pc1$Defective , levels=c("N" ,"Y") ) ) 
pc1.test$Defective <- as.numeric(pc1.test$Defective , levels=c("N" ,"Y") ) 


#discretize the data for bayesian networks
pc1.disc = chiM(pc1, alpha = 0.05) 
pc1.test.disc = chiM(pc1.test,alpha=0.05)

#load the data into a data frame
pc1.disc.data = data.frame(pc1.disc$Disc.data)
pc1.test.data = data.frame(pc1.test.disc$Disc.data)



#Change all the discretized values into factors
pc1.disc.data[,names(pc1)] <- lapply(pc1.disc.data[,names(pc1)] , factor) 
pc1.test.data[,names(pc1.test)] <- lapply(pc1.test.data[,names(pc1.test)] , factor) 
#str(pc1.test.set.data)


#Building a Naive Bayes classifier
pc1.bn = naive.bayes(pc1.disc.data, "Defective")
pc1.pred.bn = predict(pc1.bn, pc1.disc.data)  #2nd parameter should be pc1.test.data
table(pc1.pred.bn,pc1.disc.data[,"Defective"])   #output the prediction matrix

#Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
pc1.given.bn <- as.numeric(as.character(pc1.pred.bn))
pc1.predicted.bn <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
accuracy(f = pc1.given.bn , x = pc1.predicted.bn)  #print the accuracy


################ Tree Augmented Network classifier###########################
pc1.tan = tree.bayes(pc1.disc.data, "Defective")
#graphviz.plot(pc1.tan)
pc1.fitted = bn.fit(pc1.tan, pc1.disc.data)
#coefficients(pc1.fitted)
pc1.pred.tan <- predict(pc1.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data
#pc1.pred.tan <- as.numeric(pc1.pred.tan)
#pc1.perf.tan <- performance(pc1.pred.tan,  measure = "tpr",x.measure =   "fpr")
table(pc1.pred.tan, pc1.disc.data[, "Defective"]) #output the prediction matrix
#Change the outputs to numeric values; 
pc1.given.tan <- as.numeric(as.character(pc1.pred.tan))
pc1.pred.tan <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
accuracy(f = pc1.given.tan , x = pc1.pred.tan)  #print the accuracy

#score(pc1.tan, pc1.disc.data, type = "bde")
#score(pc1.bn,pc1.disc.data,type="bde")

###############Constraint Based Networks########################

#gs
str(pc1.disc.data)
# Include an arc from class node to every other node, build a grow-shrink algorithm on remaining nodes.
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
#to

names(pc1.disc.data) = names(pc1)
graph.gs = empty.graph(attributes)
whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
#str(whitelist.arcs)
#names(whitelist.arcs)
graph.gs = gs(pc1.disc.data,whitelist = whitelist.arcs,debug=TRUE)

graphviz.plot(graph.gs)
  
#traceback()
#iamb
#fast.iamb
#inter.iamb

