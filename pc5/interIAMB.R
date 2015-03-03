runINTERIAMB = function(debug) {
  pc5.inter.iamb = empty.graph(names(pc5.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.inter.iamb = cextend (  inter.iamb(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc5.inter.iamb.fitted = bn.fit(pc5.inter.iamb,pc5.disc.data)
  
  pc5.inter.iamb.pred<- predict(pc5.inter.iamb.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  print( table(pc5.inter.iamb.pred, pc5.test.data[, "M"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.inter.iamb <- as.numeric(as.character(pc5.inter.iamb.pred))
  pc5.pred.inter.iamb <- as.numeric(as.character(pc5.test.data[,"M"]))
  print (accuracy(f = pc5.given.inter.iamb , x = pc5.pred.inter.iamb))  #print the accuracy
  
 
  png('./plots/interiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.inter.iamb)
  dev.off()
  
}







