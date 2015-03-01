runMMHC = function(debug) {
  
  if(debug) {
    print("Running Hybrid Max-Min Hill climb Algorithm")
  }
  
  attributes <- names(pc1.disc.data)
  
  if(debug) {
    print("Initialising Graph")
  }
  
  pc1.mmhc = empty.graph(names(pc1.disc.data))
  
  pc1.mmhc = cextend (  mmhc(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  if(debug) {
    print("Learning the Parameters")
  }
  
  pc1.mmhc.fitted = bn.fit(pc1.mmhc,pc1.disc.data)
  
  pc1.mmhc.pred<- predict(pc1.mmhc.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
  print( table(pc1.mmhc.pred, pc1.test.data[, "L"])) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.mmhc <- as.numeric(as.character(pc1.mmhc.pred))
  pc1.pred.mmhc <- as.numeric(as.character(pc1.test.data[,"L"]))
  print( accuracy(f = pc1.given.mmhc , x = pc1.pred.mmhc) )  #print the accuracy
  
  if(debug) {
    print("Done with MMHC")
  }
  png('./plots/mmhc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc1.mmhc)
  dev.off()
  
  #graphviz.plot(pc1.mmhc)
  
}
