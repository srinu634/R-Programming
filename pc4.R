library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)


pc4 = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc4.arff") #Load the dataset


pc4.test = pc4[600:705,] # Build a Test set
#pc4 = pc4[1:599,]

# str(pc4) 

# Change the Strings Y,N to 1,0
pc4$Defective <- as.numeric(factor(pc4$Defective , levels=c("N" ,"Y") ) ) 
pc4.test$Defective <- as.numeric(pc4.test$Defective , levels=c("N" ,"Y") ) 


#discretize the data for bayesian networks
pc4.disc = chiM(pc4, alpha = 0.05) 
pc4.test.disc = chiM(pc4.test,alpha=0.05)

#load the data into a data frame
pc4.disc.data = data.frame(pc4.disc$Disc.data)
pc4.test.data = data.frame(pc4.test.disc$Disc.data)


#Change all the discretized values into factors
pc4.disc.data[,names(pc4)] <- lapply(pc4.disc.data[,names(pc4)] , factor) 
pc4.test.data[,names(pc4.test)] <- lapply(pc4.test.data[,names(pc4.test)] , factor) 
#str(pc4.test.set.data)


#Building a Naive Bayes classifier
pc4.bn = naive.bayes(pc4.disc.data, "Defective")
pc4.pred.bn = predict(pc4.bn, pc4.disc.data)  #2nd parameter should be pc4.test.data
table(pc4.pred.bn,pc4.disc.data[,"Defective"])   #output the prediction matrix

#Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
pc4.given.bn <- as.numeric(as.character(pc4.pred.bn))
pc4.predicted.bn <- as.numeric(as.character(pc4.disc.data[,"Defective"]))
accuracy(f = pc4.given.bn , x = pc4.predicted.bn)  #print the accuracy


#Building a Tree Augmented Network classifier
pc4.tan = tree.bayes(pc4.disc.data, "Defective")
#graphviz.plot(pc4.tan)
pc4.fitted = bn.fit(pc4.tan, pc4.disc.data)
#coefficients(pc4.fitted)
pc4.pred.tan <- predict(pc4.fitted$Defective, pc4.disc.data) #2nd parameter should be pc4.test.data
#pc4.pred.tan <- as.numeric(pc4.pred.tan)
#pc4.perf.tan <- performance(pc4.pred.tan,  measure = "tpr",x.measure =   "fpr")
table(pc4.pred.tan, pc4.disc.data[, "Defective"]) #output the prediction matrix
#Change the outputs to numeric values; 
pc4.given.tan <- as.numeric(as.character(pc4.pred.tan))
pc4.pred.tan <- as.numeric(as.character(pc4.disc.data[,"Defective"]))
accuracy(f = pc4.given.tan , x = pc4.pred.tan)  #print the accuracy

#score(pc4.tan, pc4.disc.data, type = "bde")
#score(pc4.bn,pc4.disc.data,type="bde")
