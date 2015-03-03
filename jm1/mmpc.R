runMMPC = function(debug) {
  jm1.mmpc = empty.graph(names(jm1.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  jm1.mmpc = cextend (  mmpc(jm1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  jm1.mmpc.fitted = bn.fit(jm1.mmpc,jm1.disc.data,method = "mle")
  
  jm1.mmpc.pred<- predict(jm1.mmpc.fitted$v, jm1.test.data) #2nd parameter should be jm1.test.data
  
  print ( table(jm1.mmpc.pred, jm1.test.data[, "v"]) )#output the prediction matrix
  #Change the outputs to numeric values; 
  jm1.given.mmpc <- as.numeric(as.character(jm1.mmpc.pred))
  jm1.pred.mmpc <- as.numeric(as.character(jm1.test.data[,"v"]))
 print( accuracy(f = jm1.given.mmpc , x = jm1.pred.mmpc) ) #print the accuracy
  
  #graphviz.plot(jm1.mmpc)
  
  png('./plots/mmpc.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(jm1.mmpc)
  dev.off()
  
  
}