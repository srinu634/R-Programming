runBN = function(debug,i) {
  
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  pc5.bn  <<- naive.bayes(pc5.disc.data, "M")
  pc5.pred.bn  <<- predict(pc5.bn, pc5.test.data)  #2nd parameter should be pc5.test.data
 
  
  print ( table(pc5.pred.bn,pc5.test.data[,"M"] ) )  #output the prediction matrix

  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  pc5.given.bn  <<- as.numeric(as.character(pc5.pred.bn))
  pc5.predicted.bn  <<- as.numeric(as.character(pc5.test.data[,"M"]))
  print(  accuracy(f = pc5.given.bn , x = pc5.predicted.bn) ) #print the accuracy
  
  pc5.bn.auc <- sapply( pc5.test.data, as.numeric )
  print (  colAUC( pc5.bn.auc[,- length(pc5.test.data)] , as.numeric( pc5.predicted.bn) , plotROC=TRUE ) )
  
  
  if( i==1  )
    auc.bn <<- colAUC(  pc5.bn.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.bn) , plotROC=TRUE )
  else{
    
    auc.bn <<- rbind( auc.bn,colAUC(  pc5.bn.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.bn) , plotROC=TRUE ) )
   }
  
  print (  colAUC(  pc5.bn.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.bn) , plotROC=TRUE ) )
  colAUC(  pc5.bn.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.bn) , plotROC=TRUE )
  
  
  
  
  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  

  drawPlot("\\BN",pc5.bn,paste("bn",i,sep="")) ;
  
 
  

}