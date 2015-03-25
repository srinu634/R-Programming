runMMHC = function(debug,i) {
  
  if(debug) {
    print("Running Hybrid Max-Min Hill climb Algorithm")
  }
  
  #attributes <- names(pc5.disc.data)
  
  if(debug) {
    print("Initialising Graph")
  }
  
  pc5.mmhc = empty.graph(names(pc5.disc.data))
  
  pc5.mmhc = cextend (  mmhc(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  if(debug) {
    print("Learning the Parameters")
  }
  
  pc5.mmhc.fitted <<- bn.fit(pc5.mmhc,pc5.disc.data)
  
  pc5.mmhc.pred<<- predict(pc5.mmhc.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  print( table(pc5.mmhc.pred, pc5.test.data[,length(pc5.test.data)]     )) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.mmhc <- as.numeric(as.character(pc5.mmhc.pred))
  pc5.pred.mmhc <- as.numeric(as.character(  pc5.test.data[,length(pc5.test.data)]     )  )
  print( accuracy(f = pc5.given.mmhc , x = pc5.pred.mmhc) )  #print the accuracy
  
  
  
  if(  identical(whitelist.arcs,NULL) ) 
    temp.path = "\\MMHC"
  else
    temp.path = "\\MMHC\\BAN" 
  
 # 
  pc5.mmhc.auc <- sapply( pc5.test.data, as.numeric )
 # print (  colAUC(  pc5.mmhc.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.mmhc) , plotROC=TRUE ) )
  
  pc5.mmhc.auc <- sapply( pc5.test.data, as.numeric )
 # print (  colAUC( pc5.mmhc.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.mmhc) , plotROC=TRUE ) )
  
  
  if( i==1  )
    auc.mmhc <<- colAUC(  pc5.mmhc.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.mmhc) , plotROC=TRUE )
  else{
    
    auc.mmhc <<- rbind( auc.mmhc,colAUC(  pc5.mmhc.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.mmhc) , plotROC=TRUE ) )
  }
  
 # print (  colAUC(  pc5.mmhc.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.mmhc) , plotROC=TRUE ) )
  #
 
  drawPlot(temp.path,pc5.mmhc,paste("mmhc",i,sep="")) ;
  
  #graphviz.plot(pc5.mmhc)
  
}
