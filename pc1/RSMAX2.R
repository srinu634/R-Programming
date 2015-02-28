runRSMAX2 = function(debug) {
  pc1.rsmax2 = empty.graph(attributes)
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.rsmax2 = cextend (  rsmax2(pc1.disc.data,whitelist = NULL,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.rsmax2.fitted = bn.fit(pc1.rsmax2,pc1.disc.data)
  
  pc1.rsmax2.pred<- predict(pc1.rsmax2.fitted$L, pc1.disc.data) #2nd parameter should be pc1.test.data
  
 print( table(pc1.rsmax2.pred, pc1.disc.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.rsmax2 <- as.numeric(as.character(pc1.rsmax2.pred))
  pc1.pred.rsmax2 <- as.numeric(as.character(pc1.disc.data[,"L"]))
  print( accuracy(f = pc1.given.rsmax2 , x = pc1.pred.rsmax2)  ) #print the accuracy
 
 png('./plots/rsmax2.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(pc1.rsmax2)
 dev.off()

}