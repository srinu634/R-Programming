runIAMB = function(debug,i) {
  pc1.iamb = empty.graph(names(pc1.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.iamb = cextend (  iamb(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.iamb.fitted = bn.fit(pc1.iamb,pc1.disc.data)
  #print(pc1.iamb.fitted)
  pc1.iamb.pred<- predict(pc1.iamb.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
  print( table(pc1.iamb.pred, pc1.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.iamb <- as.numeric(as.character(pc1.iamb.pred))
  pc1.pred.iamb <- as.numeric(as.character(pc1.test.data[,"L"]))
  print ( accuracy(f = pc1.given.iamb , x = pc1.pred.iamb) ) #print the accuracy
  
  pc1.iamb.auc <- sapply( pc1.test.data, as.numeric )
  print (  colAUC(  pc1.iamb.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.iamb) , plotROC=TRUE ) )
  
 # png('/plots/IAMB.png',units="in", width=11, height=8.5, res=300)
 # graphviz.plot(pc1.iamb)
 # dev.off()
  
}