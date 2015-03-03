runMMHC = function(debug) {
  
  if(debug) {
    print("Running Hybrid Max-Min Hill climb Algorithm")
  }
  
  attributes <- names(pc5.disc.data)
  
  if(debug) {
    print("Initialising Graph")
  }
  
  pc5.mmhc = empty.graph(names(pc5.disc.data))
  
  pc5.mmhc = cextend (  mmhc(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  if(debug) {
    print("Learning the Parameters")
  }
  
  pc5.mmhc.fitted = bn.fit(pc5.mmhc,pc5.disc.data)
  
  pc5.mmhc.pred<- predict(pc5.mmhc.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  print( table(pc5.mmhc.pred, pc5.test.data[, "M"])) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.mmhc <- as.numeric(as.character(pc5.mmhc.pred))
  pc5.pred.mmhc <- as.numeric(as.character(pc5.test.data[,"M"]))
  print( accuracy(f = pc5.given.mmhc , x = pc5.pred.mmhc) )  #print the accuracy
  
  if(debug) {
    print("Done with MMHC")
  }
  png('./plots/mmhc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.mmhc)
  dev.off()
  
  #graphviz.plot(pc5.mmhc)
  
}
