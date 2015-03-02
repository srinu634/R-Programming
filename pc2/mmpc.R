runMMPC = function(debug) {
  pc2.mmpc = empty.graph(names(pc2.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc2.mmpc = cextend ( mmpc(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc2.mmpc.fitted = bn.fit(pc2.mmpc,pc2.disc.data,method = "mle")
  
  pc2.mmpc.pred<- predict(pc2.mmpc.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
  print ( table(pc2.mmpc.pred, pc2.test.data[, "K"]) )#output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.mmpc <- as.numeric(as.character(pc2.mmpc.pred))
  pc2.pred.mmpc <- as.numeric(as.character(pc2.test.data[,"K"]))
 print( accuracy(f = pc2.given.mmpc , x = pc2.pred.mmpc) ) #print the accuracy
  
  #graphviz.plot(pc2.mmpc)
  
  png('./plots/mmpc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.mmpc)
  dev.off()
  
  
}