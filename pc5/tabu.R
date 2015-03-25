runTABU = function(debug,i) {
  
  
  
  score1 <- c("bde","k2","aic")
 
  for( j in score1) {
    
    if(debug){
      print("Running Score Based - TABU algorithm")
      print("Score considered: ")
    }
    
    print(i)
    
    pc5.tabu <<- empty.graph(names(pc5.disc.data))
    
    
    pc5.tabu <<- cextend (  tabu(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=j) ) # cextend :: makes sure that all edges are directed
    
   
    
    if(debug){
      print(str(pc5.test.data))
    }
    
    pc5.tabu.fitted <<- bn.fit(pc5.tabu,pc5.disc.data)
    
    if( debug){
      print(str(pc5.tabu.fitted))
    }
    
    pc5.tabu.pred <<- predict(pc5.tabu.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
    
    
    print ( table(pc5.tabu.pred, pc5.test.data[,length(pc5.test.data)]) )#output the prediction matrix
    #Change the outputs to numeric values; 
    pc5.given.tabu <<- as.numeric(as.character(pc5.tabu.pred))
    pc5.pred.tabu <<- as.numeric(as.character(      pc5.test.data[,length(pc5.test.data)]      ))
    accuracy(f = pc5.given.tabu , x = pc5.pred.tabu)  #print the accuracy
    
    pc5.tabu.auc <- sapply( pc5.test.data, as.numeric )
   # print (  colAUC(  pc5.tabu.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.tabu) , plotROC=TRUE ) )
    
   
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\TABU"
    else
      temp.path = "\\TABU\\BAN" 
    
    #
    pc5.test.data.numeric <- sapply( pc5.test.data, as.numeric )
    
    
    if( identical(j,"aic" ) ){
      if( i==1  )
        auc.tabu.aic <<- colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.tabu) , plotROC=TRUE )
      else{
        
        auc.tabu.aic <<- rbind( auc.tabu.aic,colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.tabu) , plotROC=TRUE ) )
      }  
    }
    else if( identical(j,"k2") ) { 
      if( i==1  )
        auc.tabu.k2 <<- colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.tabu) , plotROC=TRUE )
      else{
        
        auc.tabu.k2 <<- rbind( auc.tabu.k2,colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.tabu) , plotROC=TRUE ) )
      }  
    }
    else {
      if( i==1  )
        auc.tabu.bde <<- colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.tabu) , plotROC=TRUE )
      else{
        
        auc.tabu.bde <<- rbind( auc.tabu.bde,colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.tabu) , plotROC=TRUE ) )
      }  
    }
    #
   
    
    drawPlot(temp.path,pc5.tabu,paste("tabu_",j,"_",i,sep="") ) ;
  }

}