runRSMAX2 = function(debug) {
  pc3.rsmax2 = empty.graph(names(pc3.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc3.rsmax2 = cextend (  rsmax2(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc3.rsmax2.fitted = bn.fit(pc3.rsmax2,pc3.disc.data)
  
  pc3.rsmax2.pred<- predict(pc3.rsmax2.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
 print( table(pc3.rsmax2.pred, pc3.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.rsmax2 <- as.numeric(as.character(pc3.rsmax2.pred))
  pc3.pred.rsmax2 <- as.numeric(as.character(pc3.test.data[,"L"]))
  print( accuracy(f = pc3.given.rsmax2 , x = pc3.pred.rsmax2)  ) #print the accuracy
 
 png('./plots/rsmax2.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(pc3.rsmax2)
 dev.off()

}