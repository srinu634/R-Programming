runINTERIAMB = function(debug) {
  pc3.inter.iamb = empty.graph(names(pc3.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc3.inter.iamb = cextend (  inter.iamb(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc3.inter.iamb.fitted = bn.fit(pc3.inter.iamb,pc3.disc.data)
  
  pc3.inter.iamb.pred<- predict(pc3.inter.iamb.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
  print( table(pc3.inter.iamb.pred, pc3.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.inter.iamb <- as.numeric(as.character(pc3.inter.iamb.pred))
  pc3.pred.inter.iamb <- as.numeric(as.character(pc3.test.data[,"L"]))
  print (accuracy(f = pc3.given.inter.iamb , x = pc3.pred.inter.iamb))  #print the accuracy
  
 
  png('./plots/interiamb.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.inter.iamb)
  dev.off()
  
}







