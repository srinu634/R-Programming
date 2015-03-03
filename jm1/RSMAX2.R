runRSMAX2 = function(debug) {
  jm1.rsmax2 = empty.graph(names(jm1.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  jm1.rsmax2 = cextend (  rsmax2(jm1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  jm1.rsmax2.fitted = bn.fit(jm1.rsmax2,jm1.disc.data)
  
  jm1.rsmax2.pred<- predict(jm1.rsmax2.fitted$v, jm1.test.data) #2nd parameter should be jm1.test.data
  
 print( table(jm1.rsmax2.pred, jm1.test.data[, "v"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  jm1.given.rsmax2 <- as.numeric(as.character(jm1.rsmax2.pred))
  jm1.pred.rsmax2 <- as.numeric(as.character(jm1.test.data[,"v"]))
  print( accuracy(f = jm1.given.rsmax2 , x = jm1.pred.rsmax2)  ) #print the accuracy
 
 png('./plots/rsmax2.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(jm1.rsmax2)
 dev.off()

}