library(foreign)
library(bnlearn)
#library(infotheo)
pc5.data = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\R programming basics\\pc5.arff")

head(pc5.data)

varnames = names(pc5.data)
varnames

str(pc5.data)

pc5.data.frame = data.frame(pc5.data)

#for( i in varnames){
#  j = paste("pc5.data$",i,sep="") ;
#  print(j)
#  j = as.factor(j)
#}

sink("features.data", append=FALSE, split=FALSE)


pc5.data.frame
#d = discretize(pc5.data.frame, method = 'hartemink', breaks = 4, ibreaks = 20) All variables must be continous.


str(pc5.data);

tan = tree.bayes(pc5.data,"c") #specify the root node

fitted = bn.fit(tan, pc5.data, method = "bayes") #learn the parameters
fitted
pred = predict(fitted, pc5.data) #?
pred #?
table(pred, pc5.data[, "c"]) #?

graphviz.plot(tan)


#########################################################################################
#d = discretize(pc5.data.frame, method = 'hartemink', breaks = 4, ibreaks = 20)
#d = discretize(gaussian.test, method = 'hartemink', breaks = 4, ibreaks = 20)
#plot(hc(d))