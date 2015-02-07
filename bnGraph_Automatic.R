data() #See the available data given by default
	  #pick any data set

cars ; #show cars data 
asia ; #asia data looks better

head(asia); #To see the features information

varnames = c("FEVER","COUGH","COLD","HEADACHE","SHIVERS","TEMPERATURE","DRYEYES","DRYSKIN")

names(asia) = varnames ; #change the headings

head(asia) #check the headings now.

data(asia) ; #pick the asia data 

g = hc(asia) ;  # ?hc for more info

graphviz.plot(g); # display the graph


###########################################

#Learn the parameters

params = bn.fit(g,asia)

print( params); #print the Coditional Probability Table   (CPT)
