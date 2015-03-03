runMMPC = function(debug) {
  pc3.mmpc = empty.graph(names(pc3.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc3.mmpc = cextend (  mmpc(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc3.mmpc.fitted = bn.fit(pc3.mmpc,pc3.disc.data,method = "mle")
  
  pc3.mmpc.pred<- predict(pc3.mmpc.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
  print ( table(pc3.mmpc.pred, pc3.test.data[, "L"]) )#output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.mmpc <- as.numeric(as.character(pc3.mmpc.pred))
  pc3.pred.mmpc <- as.numeric(as.character(pc3.test.data[,"L"]))
 print( accuracy(f = pc3.given.mmpc , x = pc3.pred.mmpc) ) #print the accuracy
  
  #graphviz.plot(pc3.mmpc)
  
  png('./plots/mmpc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.mmpc)
  dev.off()
  
  
}