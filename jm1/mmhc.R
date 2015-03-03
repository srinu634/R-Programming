runMMHC = function(debug) {
  
  if(debug) {
    print("Running Hybrid Max-Min Hill climb Algorithm")
  }
  
  attributes <- names(jm1.disc.data)
  
  if(debug) {
    print("Initialising Graph")
  }
  
  jm1.mmhc = empty.graph(names(jm1.disc.data))
  
  jm1.mmhc = cextend (  mmhc(jm1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  if(debug) {
    print("Learning the Parameters")
  }
  
  jm1.mmhc.fitted = bn.fit(jm1.mmhc,jm1.disc.data)
  
  jm1.mmhc.pred<- predict(jm1.mmhc.fitted$v, jm1.test.data) #2nd parameter should be jm1.test.data
  
  print( table(jm1.mmhc.pred, jm1.test.data[, "v"])) #output the prediction matrix
  #Change the outputs to numeric values; 
  jm1.given.mmhc <- as.numeric(as.character(jm1.mmhc.pred))
  jm1.pred.mmhc <- as.numeric(as.character(jm1.test.data[,"v"]))
  print( accuracy(f = jm1.given.mmhc , x = jm1.pred.mmhc) )  #print the accuracy
  
  if(debug) {
    print("Done with MMHC")
  }
  png('./plots/mmhc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(jm1.mmhc)
  dev.off()
  
  #graphviz.plot(jm1.mmhc)
  
}
