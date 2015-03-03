runIAMB = function(debug) {
  pc3.iamb = empty.graph(names(pc3.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc3.iamb = cextend (  iamb(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc3.iamb.fitted = bn.fit(pc3.iamb,pc3.disc.data)
  #print(pc3.iamb.fitted)
  pc3.iamb.pred<- predict(pc3.iamb.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
  print( table(pc3.iamb.pred, pc3.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.iamb <- as.numeric(as.character(pc3.iamb.pred))
  pc3.pred.iamb <- as.numeric(as.character(pc3.test.data[,"L"]))
  print ( accuracy(f = pc3.given.iamb , x = pc3.pred.iamb) ) #print the accuracy
  
  png('./plots/IAMB.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.iamb)
  dev.off()
  
}