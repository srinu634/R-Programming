runRSMAX2 = function(debug,i) {
  pc1.rsmax2 = empty.graph(names(pc1.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.rsmax2 = cextend (  rsmax2(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.rsmax2.fitted = bn.fit(pc1.rsmax2,pc1.disc.data)
  
  pc1.rsmax2.pred<- predict(pc1.rsmax2.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
 print( table(pc1.rsmax2.pred, pc1.test.data[,length(pc1.test.data)]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.rsmax2 <- as.numeric(as.character(pc1.rsmax2.pred))
  pc1.pred.rsmax2 <- as.numeric(as.character(pc1.test.data[,length(pc1.test.data)]))
  print( accuracy(f = pc1.given.rsmax2 , x = pc1.pred.rsmax2)  ) #print the accuracy
 
 if(  identical(whitelist.arcs,NULL) ) 
   temp.path = "\\RSMAX2"
 else
   temp.path = "\\RSMAX2\\BAN" 
 
 pc1.rsmax2.auc <- sapply( pc1.test.data, as.numeric )
 print (  colAUC(  pc1.rsmax2.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.rsmax2) , plotROC=TRUE ) )
 
 
 
 # 
 pc1.rsmax2.auc <- sapply( pc1.test.data, as.numeric )
 print (  colAUC(  pc1.rsmax2.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.rsmax2) , plotROC=TRUE ) )
 
 pc1.rsmax2.auc <- sapply( pc1.test.data, as.numeric )
 print (  colAUC( pc1.rsmax2.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.rsmax2) , plotROC=TRUE ) )
 
 
 if( i==1  )
   auc.rsmax2 <<- colAUC(  pc1.rsmax2.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.rsmax2) , plotROC=TRUE )
 else{
   
   auc.rsmax2 <<- rbind( auc.rsmax2,colAUC(  pc1.rsmax2.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.rsmax2) , plotROC=TRUE ) )
 }
 
 print (  colAUC(  pc1.rsmax2.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.rsmax2) , plotROC=TRUE ) )
 #
 
 
 drawPlot(temp.path,pc1.rsmax2,paste("RSMAX2",i,sep="")) ;

}