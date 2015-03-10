runBN = function(debug,i) {
  
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  pc1.bn  <<- naive.bayes(pc1.disc.data, "L")
  pc1.pred.bn  <<- predict(pc1.bn, pc1.test.data)  #2nd parameter should be pc1.test.data
 
  
  print ( table(pc1.pred.bn,pc1.test.data[,"L"] ) )  #output the prediction matrix

  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  pc1.given.bn  <<- as.numeric(as.character(pc1.pred.bn))
  pc1.predicted.bn  <<- as.numeric(as.character(pc1.test.data[,"L"]))
  print(  accuracy(f = pc1.given.bn , x = pc1.predicted.bn) ) #print the accuracy
  
  pc1.bn.auc <- sapply( pc1.test.data, as.numeric )
  print (  colAUC( pc1.bn.auc[,- length(pc1.test.data)] , as.numeric( pc1.predicted.bn) , plotROC=TRUE ) )
  
  
  if( i==1  )
    auc.bn <<- colAUC(  pc1.bn.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.bn) , plotROC=TRUE )
  else{
    
    auc.bn <<- rbind( auc.bn,colAUC(  pc1.bn.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.bn) , plotROC=TRUE ) )
   }
  
  print (  colAUC(  pc1.bn.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.bn) , plotROC=TRUE ) )
  colAUC(  pc1.bn.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.bn) , plotROC=TRUE )
  
  
  
  
  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  

  drawPlot("\\BN",pc1.bn,paste("bn",i,sep="")) ;
  
 
  

}