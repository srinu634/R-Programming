library(foreign)
library(bnlearn)
#library(infotheo)
pc5.data = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\R programming basics\\pc5.arff")

head(pc5.data)

varnames = names(pc5.data)
varnames

str(pc5.data)

pc5.data.frame = data.frame(pc5.data)

tan = tree.bayes(pc5.data,"c") #specify the root node



#########################################################################################
#d = discretize(pc5.data.frame, method = 'hartemink', breaks = 4, ibreaks = 20)
#d = discretize(gaussian.test, method = 'hartemink', breaks = 4, ibreaks = 20)
#plot(hc(d))