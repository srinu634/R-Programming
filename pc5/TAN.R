runTAN = function(debug,i) {
  
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  pc5.tan <<- tree.bayes(pc5.disc.data, "M")
  #graphviz.plot(pc5.tan)
  pc5.fitted <<- bn.fit(pc5.tan, pc5.disc.data)
  #coefficients(pc5.fitted)
  pc5.pred.tan <<- predict(pc5.fitted, pc5.test.data) #2nd parameter should be pc5.test.data
  #pc5.pred.tan <- as.numeric(pc5.pred.tan)
  #pc5.perf.tan <- performance(pc5.pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(pc5.pred.tan, pc5.test.data[, "M"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.tan <<- as.numeric(as.character(pc5.pred.tan))
  pc5.pred.tan <<- as.numeric(as.character(pc5.test.data[,"M"]))
  accuracy(f = pc5.given.tan , x = pc5.pred.tan)  #print the accuracy
  
  pc5.tan.auc <- sapply( pc5.test.data, as.numeric )
     # colAUC( pc5.tan.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.tan) , plotROC=TRUE ) 
  
  
  #score(pc5.tan, pc5.disc.data, type = "bde")
  #score(pc5.tan,pc5.disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  pc5.tan.auc <- sapply( pc5.test.data, as.numeric )
 # print (  colAUC( pc5.tan.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.tan) , plotROC=TRUE ) )
  
  
  if( i==1  )
    auc.tan <<- colAUC(  pc5.tan.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.tan) , plotROC=TRUE )
  else{
    
    auc.tan <<- rbind( auc.tan,colAUC(  pc5.tan.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.tan) , plotROC=TRUE ) )
  }
  
 # print (  colAUC(  pc5.tan.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.tan) , plotROC=TRUE ) )
  
  
  drawPlot("\\TAN",pc5.tan,paste("tan",i,sep="")) ;

}