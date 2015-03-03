runIAMB = function(debug) {
  pc5.iamb = empty.graph(names(pc5.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.iamb = cextend (  iamb(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc5.iamb.fitted = bn.fit(pc5.iamb,pc5.disc.data)
  #print(pc5.iamb.fitted)
  pc5.iamb.pred<- predict(pc5.iamb.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  print( table(pc5.iamb.pred, pc5.test.data[, "M"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.iamb <- as.numeric(as.character(pc5.iamb.pred))
  pc5.pred.iamb <- as.numeric(as.character(pc5.test.data[,"M"]))
  print ( accuracy(f = pc5.given.iamb , x = pc5.pred.iamb) ) #print the accuracy
  
  png('./plots/IAMB.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.iamb)
  dev.off()
  
}