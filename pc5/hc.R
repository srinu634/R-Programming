runHC = function(debug,i) {
  score1 <- c("aic","k2","bde")
 
  for( j in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered : ")
    }
    
    print(i)
    string = "L" ;
    pc5.hc = empty.graph( names(pc5.disc.data ))
    
    
    pc5.hc = cextend (  hc(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=j) ) # cextend :: makes sure that all edges are directed
    
    pc5.hc.fitted = bn.fit(pc5.hc,pc5.disc.data)
    
    pc5.hc.pred<- predict(pc5.hc.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
    
    print ( table(pc5.hc.pred, pc5.test.data[,length(pc5.test.data)]) ) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc5.given.hc <- as.numeric(as.character(pc5.hc.pred))
    pc5.pred.hc <- as.numeric(as.character(pc5.test.data[,length(pc5.test.data)]))
    accuracy(f = pc5.given.hc , x = pc5.pred.hc)  #print the accuracy
    
    pc5.hc.auc <- sapply( pc5.test.data, as.numeric )
    
   
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\HC"
    else
      temp.path = "\\HC\\BAN" 
    pc5.test.data.numeric <- sapply( pc5.test.data, as.numeric )
    
    
    if( identical(j,"aic" ) ){
      if( i==1  )
        auc.hc.aic <<- colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.hc) , plotROC=TRUE )
      else{
        
        auc.hc.aic <<- rbind( auc.hc.aic,colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.hc) , plotROC=TRUE ) )
      }  
    }
    else if( identical(j,"k2") ) { 
      if( i==1  )
        auc.hc.k2 <<- colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.hc) , plotROC=TRUE )
      else{
        
        auc.hc.k2 <<- rbind( auc.hc.k2,colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.hc) , plotROC=TRUE ) )
      }  
    }
    else {
      if( i==1  )
        auc.hc.bde <<- colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.hc) , plotROC=TRUE )
      else{
        
        auc.hc.bde <<- rbind( auc.hc.bde,colAUC(  pc5.test.data.numeric [,- length(pc5.test.data)] , as.numeric( pc5.pred.hc) , plotROC=TRUE ) )
      }  
    }
    
    drawPlot(temp.path,pc5.hc,paste("hc_",j,"_",i,sep="") ) ;
    
    }
  
    
    #print (  colAUC(  pc5.tan.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.tan) , plotROC=TRUE ) )
    
    
   
    
  }
  
  
  
  
  
  
 