runTAN = function(debug,i) {
  
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  pc1.tan <<- tree.bayes(pc1.disc.data, "L")
  #graphviz.plot(pc1.tan)
  pc1.fitted <<- bn.fit(pc1.tan, pc1.disc.data)
  #coefficients(pc1.fitted)
  pc1.pred.tan <<- predict(pc1.fitted, pc1.test.data) #2nd parameter should be pc1.test.data
  #pc1.pred.tan <- as.numeric(pc1.pred.tan)
  #pc1.perf.tan <- performance(pc1.pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(pc1.pred.tan, pc1.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.tan <<- as.numeric(as.character(pc1.pred.tan))
  pc1.pred.tan <<- as.numeric(as.character(pc1.test.data[,"L"]))
  accuracy(f = pc1.given.tan , x = pc1.pred.tan)  #print the accuracy
  
  pc1.tan.auc <- sapply( pc1.test.data, as.numeric )
  print (  colAUC( pc1.tan.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.tan) , plotROC=TRUE ) )
  
  
  #score(pc1.tan, pc1.disc.data, type = "bde")
  #score(pc1.tan,pc1.disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  pc1.tan.auc <- sapply( pc1.test.data, as.numeric )
  print (  colAUC( pc1.tan.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.tan) , plotROC=TRUE ) )
  
  
  if( i==1  )
    auc.tan <<- colAUC(  pc1.tan.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.tan) , plotROC=TRUE )
  else{
    
    auc.tan <<- rbind( auc.tan,colAUC(  pc1.tan.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.tan) , plotROC=TRUE ) )
  }
  
  print (  colAUC(  pc1.tan.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.tan) , plotROC=TRUE ) )
  
  
  drawPlot("\\TAN",pc1.tan,paste("tan",i,sep="")) ;

}