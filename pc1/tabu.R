runTABU = function(debug,i) {
  
  
  
  score1 <- c("bde","k2","aic")
 
  for( j in score1) {
    
    if(debug){
      print("Running Score Based - TABU algorithm")
      print("Score considered: ")
    }
    
    print(i)
    
    pc1.tabu <<- empty.graph(names(pc1.disc.data))
    
    
    pc1.tabu <<- cextend (  tabu(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=j) ) # cextend :: makes sure that all edges are directed
    
   
    
    if(debug){
      print(str(pc1.test.data))
    }
    
    pc1.tabu.fitted <<- bn.fit(pc1.tabu,pc1.disc.data)
    
    if( debug){
      print(str(pc1.tabu.fitted))
    }
    
    pc1.tabu.pred <<- predict(pc1.tabu.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
    
    
    print ( table(pc1.tabu.pred, pc1.test.data[,length(pc1.test.data)]) )#output the prediction matrix
    #Change the outputs to numeric values; 
    pc1.given.tabu <<- as.numeric(as.character(pc1.tabu.pred))
    pc1.pred.tabu <<- as.numeric(as.character(      pc1.test.data[,length(pc1.test.data)]      ))
    accuracy(f = pc1.given.tabu , x = pc1.pred.tabu)  #print the accuracy
    
    pc1.tabu.auc <- sapply( pc1.test.data, as.numeric )
    print (  colAUC(  pc1.tabu.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.tabu) , plotROC=TRUE ) )
    
   
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\TABU"
    else
      temp.path = "\\TABU\\BAN" 
    
    #
    pc1.test.data.numeric <- sapply( pc1.test.data, as.numeric )
    
    
    if( identical(j,"aic" ) ){
      if( i==1  )
        auc.tabu.aic <<- colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.tabu) , plotROC=TRUE )
      else{
        
        auc.tabu.aic <<- rbind( auc.tabu.aic,colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.tabu) , plotROC=TRUE ) )
      }  
    }
    else if( identical(j,"k2") ) { 
      if( i==1  )
        auc.tabu.k2 <<- colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.tabu) , plotROC=TRUE )
      else{
        
        auc.tabu.k2 <<- rbind( auc.tabu.k2,colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.tabu) , plotROC=TRUE ) )
      }  
    }
    else {
      if( i==1  )
        auc.tabu.bde <<- colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.tabu) , plotROC=TRUE )
      else{
        
        auc.tabu.bde <<- rbind( auc.tabu.bde,colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.tabu) , plotROC=TRUE ) )
      }  
    }
    #
   
    
    drawPlot(temp.path,pc1.tabu,paste("tabu_",j,"_",i,sep="") ) ;
  }

}