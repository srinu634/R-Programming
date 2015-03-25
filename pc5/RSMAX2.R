runRSMAX2 = function(debug,i) {
  pc5.rsmax2 = empty.graph(names(pc5.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.rsmax2 = cextend (  rsmax2(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc5.rsmax2.fitted = bn.fit(pc5.rsmax2,pc5.disc.data)
  
  pc5.rsmax2.pred<- predict(pc5.rsmax2.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
 print( table(pc5.rsmax2.pred, pc5.test.data[,length(pc5.test.data)]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.rsmax2 <- as.numeric(as.character(pc5.rsmax2.pred))
  pc5.pred.rsmax2 <- as.numeric(as.character(pc5.test.data[,length(pc5.test.data)]))
  print( accuracy(f = pc5.given.rsmax2 , x = pc5.pred.rsmax2)  ) #print the accuracy
 
 if(  identical(whitelist.arcs,NULL) ) 
   temp.path = "\\RSMAX2"
 else
   temp.path = "\\RSMAX2\\BAN" 
 
 pc5.rsmax2.auc <- sapply( pc5.test.data, as.numeric )
 print (  colAUC(  pc5.rsmax2.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.rsmax2) , plotROC=TRUE ) )
 
 
 
 # 
 pc5.rsmax2.auc <- sapply( pc5.test.data, as.numeric )
 #print (  colAUC(  pc5.rsmax2.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.rsmax2) , plotROC=TRUE ) )
 
 pc5.rsmax2.auc <- sapply( pc5.test.data, as.numeric )
# print (  colAUC( pc5.rsmax2.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.rsmax2) , plotROC=TRUE ) )
 
 
 if( i==1  )
   auc.rsmax2 <<- colAUC(  pc5.rsmax2.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.rsmax2) , plotROC=TRUE )
 else{
   
   auc.rsmax2 <<- rbind( auc.rsmax2,colAUC(  pc5.rsmax2.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.rsmax2) , plotROC=TRUE ) )
 }
 
# print (  colAUC(  pc5.rsmax2.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.rsmax2) , plotROC=TRUE ) )
 #
 
 
 drawPlot(temp.path,pc5.rsmax2,paste("RSMAX2",i,sep="")) ;

}