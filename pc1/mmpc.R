runMMPC = function(debug) {
  pc1.mmpc = empty.graph(names(pc1.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.mmpc = cextend (  mmpc(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.mmpc.fitted = bn.fit(pc1.mmpc,pc1.disc.data,method = "mle")
  
  pc1.mmpc.pred<- predict(pc1.mmpc.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
  print ( table(pc1.mmpc.pred, pc1.test.data[, "L"]) )#output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.mmpc <- as.numeric(as.character(pc1.mmpc.pred))
  pc1.pred.mmpc <- as.numeric(as.character(pc1.test.data[,"L"]))
 print( accuracy(f = pc1.given.mmpc , x = pc1.pred.mmpc) ) #print the accuracy
  
  #graphviz.plot(pc1.mmpc)
  
  png('./plots/mmpc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc1.mmpc)
  dev.off()
  
  
}