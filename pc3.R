library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)


pc3 = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc3.arff") #Load the dataset


pc3.test = pc3[600:705,] # Build a Test set
#pc3 = pc3[1:599,]

# str(pc3) 

# Change the Strings Y,N to 1,0
pc3$Defective <- as.numeric(factor(pc3$Defective , levels=c("N" ,"Y") ) ) 
pc3.test$Defective <- as.numeric(pc3.test$Defective , levels=c("N" ,"Y") ) 


#discretize the data for bayesian networks
pc3.disc = chiM(pc3, alpha = 0.05) 
pc3.test.disc = chiM(pc3.test,alpha=0.05)

#load the data into a data frame
pc3.disc.data = data.frame(pc3.disc$Disc.data)
pc3.test.data = data.frame(pc3.test.disc$Disc.data)


#Change all the discretized values into factors
pc3.disc.data[,names(pc3)] <- lapply(pc3.disc.data[,names(pc3)] , factor) 
pc3.test.data[,names(pc3.test)] <- lapply(pc3.test.data[,names(pc3.test)] , factor) 
#str(pc3.test.set.data)


#Building a Naive Bayes classifier
pc3.bn = naive.bayes(pc3.disc.data, "Defective")
pc3.pred.bn = predict(pc3.bn, pc3.disc.data)  #2nd parameter should be pc3.test.data
table(pc3.pred.bn,pc3.disc.data[,"Defective"])   #output the prediction matrix

#Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
pc3.given.bn <- as.numeric(as.character(pc3.pred.bn))
pc3.predicted.bn <- as.numeric(as.character(pc3.disc.data[,"Defective"]))
accuracy(f = pc3.given.bn , x = pc3.predicted.bn)  #print the accuracy


#Building a Tree Augmented Network classifier
pc3.tan = tree.bayes(pc3.disc.data, "Defective")
#graphviz.plot(pc3.tan)
pc3.fitted = bn.fit(pc3.tan, pc3.disc.data)
#coefficients(pc3.fitted)
pc3.pred.tan <- predict(pc3.fitted$Defective, pc3.disc.data) #2nd parameter should be pc3.test.data
#pc3.pred.tan <- as.numeric(pc3.pred.tan)
#pc3.perf.tan <- performance(pc3.pred.tan,  measure = "tpr",x.measure =   "fpr")
table(pc3.pred.tan, pc3.disc.data[, "Defective"]) #output the prediction matrix
#Change the outputs to numeric values; 
pc3.given.tan <- as.numeric(as.character(pc3.pred.tan))
pc3.pred.tan <- as.numeric(as.character(pc3.disc.data[,"Defective"]))
accuracy(f = pc3.given.tan , x = pc3.pred.tan)  #print the accuracy

#score(pc3.tan, pc3.disc.data, type = "bde")
#score(pc3.bn,pc3.disc.data,type="bde")

