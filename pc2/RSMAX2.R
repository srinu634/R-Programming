runRSMAX2 = function(debug) {
  pc2.rsmax2 = empty.graph(names(pc2.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc2.rsmax2 = cextend (  rsmax2(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc2.rsmax2.fitted = bn.fit(pc2.rsmax2,pc2.disc.data)
  
  pc2.rsmax2.pred<- predict(pc2.rsmax2.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
 print( table(pc2.rsmax2.pred, pc2.test.data[, "K"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.rsmax2 <- as.numeric(as.character(pc2.rsmax2.pred))
  pc2.pred.rsmax2 <- as.numeric(as.character(pc2.test.data[,"K"]))
  print( accuracy(f = pc2.given.rsmax2 , x = pc2.pred.rsmax2)  ) #print the accuracy
 
 png('./plots/rsmax2.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(pc2.rsmax2)
 dev.off()

}