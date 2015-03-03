runFASTIAMB = function( debug) {
  pc3.fast.iamb = empty.graph( names(pc3.disc.data) )
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc3.fast.iamb = cextend (  fast.iamb(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc3.fast.iamb.fitted = bn.fit(pc3.fast.iamb,pc3.disc.data)
  
  pc3.fast.iamb.pred<- predict(pc3.fast.iamb.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
  table(pc3.fast.iamb.pred, pc3.test.data[, "L"]) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.fast.iamb <- as.numeric(as.character(pc3.fast.iamb.pred))
  pc3.pred.fast.iamb <- as.numeric(as.character(pc3.test.data[,"L"]))
  accuracy(f = pc3.given.fast.iamb , x = pc3.pred.fast.iamb)  #print the accuracy
  
  #graphviz.plot(pc3.fast.iamb)
  png('./plots/fastiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.fast.iamb)
  dev.off()
  
}