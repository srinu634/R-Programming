runINTERIAMB = function(debug,i) {
  pc1.inter.iamb = empty.graph(names(pc1.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.inter.iamb = cextend (  inter.iamb(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.inter.iamb.fitted = bn.fit(pc1.inter.iamb,pc1.disc.data)
  
  pc1.inter.iamb.pred<- predict(pc1.inter.iamb.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
  print( table(pc1.inter.iamb.pred, pc1.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.inter.iamb <- as.numeric(as.character(pc1.inter.iamb.pred))
  pc1.pred.inter.iamb <- as.numeric(as.character(pc1.test.data[,"L"]))
  print (accuracy(f = pc1.given.inter.iamb , x = pc1.pred.inter.iamb))  #print the accuracy
  
 
  png('./plots/interiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc1.inter.iamb)
  dev.off()
  
}







