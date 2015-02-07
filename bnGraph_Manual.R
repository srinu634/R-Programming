#bnlearn basics

library(bnlearn) #Import the package

attributes  = c("NAME","AGE","SEX","SCORE","RACE") ;

g = empty.graph(attributes)

g

######################
#Constructing the graph manually


arcs(g,ignore.cycles=T) = data.frame( 
"from" = c("NAME","SCORE") ,
"to" = c("AGE","SEX")
)


graphviz.plot(g)

