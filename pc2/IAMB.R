runIAMB = function(debug) {
  pc2.iamb = empty.graph(names(pc2.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc2.iamb = cextend (  iamb(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc2.iamb.fitted = bn.fit(pc2.iamb,pc2.disc.data)
  #print(pc2.iamb.fitted)
  pc2.iamb.pred<- predict(pc2.iamb.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
  print( table(pc2.iamb.pred, pc2.test.data[, "K"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.iamb <- as.numeric(as.character(pc2.iamb.pred))
  pc2.pred.iamb <- as.numeric(as.character(pc2.test.data[,"K"]))
  print ( accuracy(f = pc2.given.iamb , x = pc2.pred.iamb) ) #print the accuracy
  
  png('./plots/IAMB.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.iamb)
  dev.off()
  
}