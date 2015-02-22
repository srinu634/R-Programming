library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)


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
pc1.disc.data[,col_names] <- lapply(pc1.disc.data[,names(pc1)] , factor) 
pc1.test.data[,col_names] <- lapply(pc1.test.data[,names(pc1.test)] , factor) 
#str(pc1.test.set.data)



#Building a Naive Bayes classifier
bn = naive.bayes(pc1.disc.data, "Defective")
pred.bn = predict(bn, pc1.disc.data)  #2nd parameter should be pc1.test.data
table(pred.bn,pc1.disc.data[,"Defective"])   #output the prediction matrix

#Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
given.bn <- as.numeric(as.character(pred.bn))
predicted.bn <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
accuracy(f = given.bn , x = predicted.bn)  #print the accuracy


#Building a Tree Augmented Network classifier
tan = tree.bayes(pc1.disc.data, "Defective")
#graphviz.plot(tan)
fitted = bn.fit(tan, pc1.disc.data)
#coefficients(fitted)
pred.tan = predict(fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data
table(pred.tan, pc1.disc.data[, "Defective"]) #output the prediction matrix
#Change the outputs to numeric values; 
given.tan <- as.numeric(as.character(pred.tan))
predicted.tan <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
accuracy(f = given.tan , x = predicted.tan)  #print the accuracy


