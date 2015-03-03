runFASTIAMB = function( debug) {
  pc5.fast.iamb = empty.graph( names(pc5.disc.data) )
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.fast.iamb = cextend (  fast.iamb(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc5.fast.iamb.fitted = bn.fit(pc5.fast.iamb,pc5.disc.data)
  
  pc5.fast.iamb.pred<- predict(pc5.fast.iamb.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  table(pc5.fast.iamb.pred, pc5.test.data[, "M"]) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.fast.iamb <- as.numeric(as.character(pc5.fast.iamb.pred))
  pc5.pred.fast.iamb <- as.numeric(as.character(pc5.test.data[,"M"]))
  accuracy(f = pc5.given.fast.iamb , x = pc5.pred.fast.iamb)  #print the accuracy
  
  #graphviz.plot(pc5.fast.iamb)
  png('./plots/fastiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.fast.iamb)
  dev.off()
  
}