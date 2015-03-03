runMMPC = function(debug) {
  pc5.mmpc = empty.graph(names(pc5.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.mmpc = cextend (  mmpc(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc5.mmpc.fitted = bn.fit(pc5.mmpc,pc5.disc.data,method = "mle")
  
  pc5.mmpc.pred<- predict(pc5.mmpc.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  print ( table(pc5.mmpc.pred, pc5.test.data[, "M"]) )#output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.mmpc <- as.numeric(as.character(pc5.mmpc.pred))
  pc5.pred.mmpc <- as.numeric(as.character(pc5.test.data[,"M"]))
 print( accuracy(f = pc5.given.mmpc , x = pc5.pred.mmpc) ) #print the accuracy
  
  #graphviz.plot(pc5.mmpc)
  
  png('./plots/mmpc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.mmpc)
  dev.off()
  
  
}