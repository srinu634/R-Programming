library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)


pc2 = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc2.arff") #Load the dataset


pc2.test = pc2[600:705,] # Build a Test set
#pc2 = pc2[1:599,]

# str(pc2) 

# Change the Strings Y,N to 1,0
pc2$Defective <- as.numeric(factor(pc2$Defective , levels=c("N" ,"Y") ) ) 
pc2.test$Defective <- as.numeric(pc2.test$Defective , levels=c("N" ,"Y") ) 


#discretize the data for bayesian networks
pc2.disc = chiM(pc2, alpha = 0.05) 
pc2.test.disc = chiM(pc2.test,alpha=0.05)

#load the data into a data frame
pc2.disc.data = data.frame(pc2.disc$Disc.data)
pc2.test.data = data.frame(pc2.test.disc$Disc.data)


#Change all the discretized values into factors
pc2.disc.data[,names(pc2)] <- lapply(pc2.disc.data[,names(pc2)] , factor) 
pc2.test.data[,names(pc2.test)] <- lapply(pc2.test.data[,names(pc2.test)] , factor) 
#str(pc2.test.set.data)


#Building a Naive Bayes classifier
pc2.bn = naive.bayes(pc2.disc.data, "Defective")
pc2.pred.bn = predict(pc2.bn, pc2.disc.data)  #2nd parameter should be pc2.test.data
table(pc2.pred.bn,pc2.disc.data[,"Defective"])   #output the prediction matrix

#Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
pc2.given.bn <- as.numeric(as.character(pc2.pred.bn))
pc2.predicted.bn <- as.numeric(as.character(pc2.disc.data[,"Defective"]))
accuracy(f = pc2.given.bn , x = pc2.predicted.bn)  #print the accuracy


#Building a Tree Augmented Network classifier
pc2.tan = tree.bayes(pc2.disc.data, "Defective")
#graphviz.plot(pc2.tan)
pc2.fitted = bn.fit(pc2.tan, pc2.disc.data)
#coefficients(pc2.fitted)
pc2.pred.tan <- predict(pc2.fitted$Defective, pc2.disc.data) #2nd parameter should be pc2.test.data
#pc2.pred.tan <- as.numeric(pc2.pred.tan)
#pc2.perf.tan <- performance(pc2.pred.tan,  measure = "tpr",x.measure =   "fpr")
table(pc2.pred.tan, pc2.disc.data[, "Defective"]) #output the prediction matrix
#Change the outputs to numeric values; 
pc2.given.tan <- as.numeric(as.character(pc2.pred.tan))
pc2.pred.tan <- as.numeric(as.character(pc2.disc.data[,"Defective"]))
accuracy(f = pc2.given.tan , x = pc2.pred.tan)  #print the accuracy

#score(pc2.tan, pc2.disc.data, type = "bde")
#score(pc2.bn,pc2.disc.data,type="bde")

