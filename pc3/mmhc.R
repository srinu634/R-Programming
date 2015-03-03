runMMHC = function(debug) {
  
  if(debug) {
    print("Running Hybrid Max-Min Hill climb Algorithm")
  }
  
  attributes <- names(pc3.disc.data)
  
  if(debug) {
    print("Initialising Graph")
  }
  
  pc3.mmhc = empty.graph(names(pc3.disc.data))
  
  pc3.mmhc = cextend (  mmhc(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  if(debug) {
    print("Learning the Parameters")
  }
  
  pc3.mmhc.fitted = bn.fit(pc3.mmhc,pc3.disc.data)
  
  pc3.mmhc.pred<- predict(pc3.mmhc.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
  print( table(pc3.mmhc.pred, pc3.test.data[, "L"])) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.mmhc <- as.numeric(as.character(pc3.mmhc.pred))
  pc3.pred.mmhc <- as.numeric(as.character(pc3.test.data[,"L"]))
  print( accuracy(f = pc3.given.mmhc , x = pc3.pred.mmhc) )  #print the accuracy
  
  if(debug) {
    print("Done with MMHC")
  }
  png('./plots/mmhc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.mmhc)
  dev.off()
  
  #graphviz.plot(pc3.mmhc)
  
}
