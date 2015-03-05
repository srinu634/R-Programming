runRSMAX2 = function(debug,i) {
  pc1.rsmax2 = empty.graph(names(pc1.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.rsmax2 = cextend (  rsmax2(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.rsmax2.fitted = bn.fit(pc1.rsmax2,pc1.disc.data)
  
  pc1.rsmax2.pred<- predict(pc1.rsmax2.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
 print( table(pc1.rsmax2.pred, pc1.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.rsmax2 <- as.numeric(as.character(pc1.rsmax2.pred))
  pc1.pred.rsmax2 <- as.numeric(as.character(pc1.test.data[,"L"]))
  print( accuracy(f = pc1.given.rsmax2 , x = pc1.pred.rsmax2)  ) #print the accuracy
 
 if(  identical(whitelist.arcs,NULL) ) 
   temp.path = "\\RSMAX2"
 else
   temp.path = "\\RSMAX2\\BAN" 
 
 
 drawPlot(temp.path,pc1.rsmax2,paste("RSMAX2",i,sep="")) ;

}