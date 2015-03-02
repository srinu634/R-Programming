runFASTIAMB = function( debug) {
  pc2.fast.iamb = empty.graph( names(pc2.disc.data) )
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc2.fast.iamb = cextend (  fast.iamb(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc2.fast.iamb.fitted = bn.fit(pc2.fast.iamb,pc2.disc.data)
  
  pc2.fast.iamb.pred<- predict(pc2.fast.iamb.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
  table(pc2.fast.iamb.pred, pc2.test.data[, "K"]) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.fast.iamb <- as.numeric(as.character(pc2.fast.iamb.pred))
  pc2.pred.fast.iamb <- as.numeric(as.character(pc2.test.data[,"K"]))
  accuracy(f = pc2.given.fast.iamb , x = pc2.pred.fast.iamb)  #print the accuracy
  
  #graphviz.plot(pc2.fast.iamb)
  png('./plots/fastiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.fast.iamb)
  dev.off()
  
}