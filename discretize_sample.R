library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)


pc5.data = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\R programming basics\\pc5.arff")

str(pc5.data)

nbins <- sqrt ( NROW(pc5.data) )

#pc5.data.discretized <- discretize(pc5.data,"equalfreq", nbins)

pc5.data$c <- as.numeric(factor(pc5.data$c , levels=c("FALSE" ,"TRUE") ) )

d = chiM(pc5.data, alpha = 0.05) #discretize the data for bayesian networks
d

data = data.frame(d$Disc.data)
col_names <- names(pc5.data)
# do do it for some names in a vector named 'col_names'
data[,col_names] <- lapply(data[,col_names] , factor)

# pc5.data$LOC_BLANK= as.factor(pc5.data$LOC_BLANK)
# pc5.data$BRANCH_COUNT= as.factor(pc5.data$BRANCH_COUNT)
# pc5.data$CALL_PAIRS= as.factor(pc5.data$CALL_PAIRS)
# pc5.data$LOC_CODE_AND_COMMENT= as.factor(pc5.data$LOC_CODE_AND_COMMENT)
# pc5.data$LOC_COMMENTS= as.factor(pc5.data$LOC_COMMENTS)
# pc5.data$CONDITION_COUNT= as.factor(pc5.data$CONDITION_COUNT)
# pc5.data$CYCLOMATIC_COMPLEXITY= as.factor(pc5.data$CYCLOMATIC_COMPLEXITY)
# pc5.data$CYCLOMATIC_DENSITY= as.factor(pc5.data$CYCLOMATIC_DENSITY)
# pc5.data$DECISION_COUNT= as.factor(pc5.data$DECISION_COUNT)
# pc5.data$DESIGN_COMPLEXITY= as.factor(pc5.data$DESIGN_COMPLEXITY)
# pc5.data$DESIGN_DENSITY= as.factor(pc5.data$DESIGN_DENSITY)
# pc5.data$EDGE_COUNT= as.factor(pc5.data$EDGE_COUNT)
# pc5.data$ESSENTIAL_COMPLEXITY= as.factor(pc5.data$ESSENTIAL_COMPLEXITY)
# pc5.data$ESSENTIAL_DENSITY= as.factor(pc5.data$ESSENTIAL_DENSITY)
# pc5.data$LOC_EXECUTABLE= as.factor(pc5.data$LOC_EXECUTABLE)
# pc5.data$PARAMETER_COUNT= as.factor(pc5.data$PARAMETER_COUNT)
# pc5.data$GLOBAL_DATA_COMPLEXITY= as.factor(pc5.data$GLOBAL_DATA_COMPLEXITY)
# pc5.data$GLOBAL_DATA_DENSITY= as.factor(pc5.data$GLOBAL_DATA_DENSITY)
# pc5.data$HALSTEAD_CONTENT= as.factor(pc5.data$HALSTEAD_CONTENT)
# pc5.data$HALSTEAD_DIFFICULTY= as.factor(pc5.data$HALSTEAD_DIFFICULTY)
# pc5.data$HALSTEAD_EFFORT= as.factor(pc5.data$HALSTEAD_EFFORT)
# pc5.data$HALSTEAD_ERROR_EST= as.factor(pc5.data$HALSTEAD_ERROR_EST)
# pc5.data$HALSTEAD_LENGTH= as.factor(pc5.data$HALSTEAD_LENGTH)
# pc5.data$HALSTEAD_LEVEL= as.factor(pc5.data$HALSTEAD_LEVEL)
# pc5.data$HALSTEAD_PROG_TIME= as.factor(pc5.data$HALSTEAD_PROG_TIME)
# pc5.data$HALSTEAD_VOLUME= as.factor(pc5.data$HALSTEAD_VOLUME)
# pc5.data$MAINTENANCE_SEVERITY= as.factor(pc5.data$MAINTENANCE_SEVERITY)
# pc5.data$MODIFIED_CONDITION_COUNT= as.factor(pc5.data$MODIFIED_CONDITION_COUNT)
# pc5.data$MULTIPLE_CONDITION_COUNT= as.factor(pc5.data$MULTIPLE_CONDITION_COUNT)
# pc5.data$NODE_COUNT= as.factor(pc5.data$NODE_COUNT)
# pc5.data$NORMALIZED_CYLOMATIC_COMPLEXITY= as.factor(pc5.data$NORMALIZED_CYLOMATIC_COMPLEXITY)
# pc5.data$NUM_OPERANDS= as.factor(pc5.data$NUM_OPERANDS)
# pc5.data$NUM_OPERATORS= as.factor(pc5.data$NUM_OPERATORS)
# pc5.data$NUM_UNIQUE_OPERANDS= as.factor(pc5.data$NUM_UNIQUE_OPERANDS)
# pc5.data$NUM_UNIQUE_OPERATORS= as.factor(pc5.data$NUM_UNIQUE_OPERATORS)
# pc5.data$NUMBER_OF_LINES= as.factor(pc5.data$NUMBER_OF_LINES)
# pc5.data$PERCENT_COMMENTS= as.factor(pc5.data$PERCENT_COMMENTS)
# pc5.data$LOC_TOTAL= as.factor(pc5.data$LOC_TOTAL)
# pc5.data$c= as.factor(pc5.data$c)

str(d)


tan = tree.bayes(data,"c") #specify the root node

fitted = bn.fit(tan, data, method = "bayes") #learn the parameters
fitted
pred = predict(fitted, data) #?
pred #?
table(pred, data[, "c"]) #?

plot(tan)

graphviz.plot(tan)


#d = discretize(pc5.data, method = 'hartemink', breaks = 5, ibreaks = 5)