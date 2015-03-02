runMMHC = function(debug) {
  
  if(debug) {
    print("Running Hybrid Max-Min Hill climb Algorithm")
  }
  
  attributes <- names(pc2.disc.data)
  
  if(debug) {
    print("Initialising Graph")
  }
  
  pc2.mmhc = empty.graph(names(pc2.disc.data))
  
  pc2.mmhc = cextend (  mmhc(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  if(debug) {
    print("Learning the Parameters")
  }
  
  pc2.mmhc.fitted = bn.fit(pc2.mmhc,pc2.disc.data)
  
  pc2.mmhc.pred<- predict(pc2.mmhc.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
  print( table(pc2.mmhc.pred, pc2.test.data[, "K"])) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.mmhc <- as.numeric(as.character(pc2.mmhc.pred))
  pc2.pred.mmhc <- as.numeric(as.character(pc2.test.data[,"K"]))
  print( accuracy(f = pc2.given.mmhc , x = pc2.pred.mmhc) )  #print the accuracy
  
  if(debug) {
    print("Done with MMHC")
  }
  png('./plots/mmhc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.mmhc)
  dev.off()
  
  #graphviz.plot(pc2.mmhc)
  
}
