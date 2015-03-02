runINTERIAMB = function(debug) {
  pc2.inter.iamb = empty.graph(names(pc2.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc2.inter.iamb = cextend (  inter.iamb(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc2.inter.iamb.fitted = bn.fit(pc2.inter.iamb,pc2.disc.data)
  
  pc2.inter.iamb.pred<- predict(pc2.inter.iamb.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
  print( table(pc2.inter.iamb.pred, pc2.test.data[, "K"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.inter.iamb <- as.numeric(as.character(pc2.inter.iamb.pred))
  pc2.pred.inter.iamb <- as.numeric(as.character(pc2.test.data[,"K"]))
  print (accuracy(f = pc2.given.inter.iamb , x = pc2.pred.inter.iamb))  #print the accuracy
  
 
  png('./plots/interiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.inter.iamb)
  dev.off()
  
}







