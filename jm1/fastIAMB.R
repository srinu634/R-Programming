runFASTIAMB = function( debug) {
  pc1.fast.iamb = empty.graph( names(pc1.disc.data) )
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.fast.iamb = cextend (  fast.iamb(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.fast.iamb.fitted = bn.fit(pc1.fast.iamb,pc1.disc.data)
  
  pc1.fast.iamb.pred<- predict(pc1.fast.iamb.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
  table(pc1.fast.iamb.pred, pc1.test.data[, "L"]) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.fast.iamb <- as.numeric(as.character(pc1.fast.iamb.pred))
  pc1.pred.fast.iamb <- as.numeric(as.character(pc1.test.data[,"L"]))
  accuracy(f = pc1.given.fast.iamb , x = pc1.pred.fast.iamb)  #print the accuracy
  
  #graphviz.plot(pc1.fast.iamb)
  png('./plots/fastiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc1.fast.iamb)
  dev.off()
  
}