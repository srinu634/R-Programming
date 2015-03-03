runRSMAX2 = function(debug) {
  pc5.rsmax2 = empty.graph(names(pc5.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.rsmax2 = cextend (  rsmax2(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc5.rsmax2.fitted = bn.fit(pc5.rsmax2,pc5.disc.data)
  
  pc5.rsmax2.pred<- predict(pc5.rsmax2.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
 print( table(pc5.rsmax2.pred, pc5.test.data[, "M"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.rsmax2 <- as.numeric(as.character(pc5.rsmax2.pred))
  pc5.pred.rsmax2 <- as.numeric(as.character(pc5.test.data[,"M"]))
  print( accuracy(f = pc5.given.rsmax2 , x = pc5.pred.rsmax2)  ) #print the accuracy
 
 png('./plots/rsmax2.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(pc5.rsmax2)
 dev.off()

}