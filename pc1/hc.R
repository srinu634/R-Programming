runHC = function(debug,i) {
  score1 <- c("aic","k2","bde")
 
  for( j in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered : ")
    }
    
    print(i)
    string = "L" ;
    pc1.hc = empty.graph( names(pc1.disc.data ))
    
    
    pc1.hc = cextend (  hc(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=j) ) # cextend :: makes sure that all edges are directed
    
    pc1.hc.fitted = bn.fit(pc1.hc,pc1.disc.data)
    
    pc1.hc.pred<- predict(pc1.hc.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
    
    print ( table(pc1.hc.pred, pc1.test.data[,length(pc1.test.data)]) ) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc1.given.hc <- as.numeric(as.character(pc1.hc.pred))
    pc1.pred.hc <- as.numeric(as.character(pc1.test.data[,length(pc1.test.data)]))
    accuracy(f = pc1.given.hc , x = pc1.pred.hc)  #print the accuracy
    
    pc1.hc.auc <- sapply( pc1.test.data, as.numeric )
    
   
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\HC"
    else
      temp.path = "\\HC\\BAN" 
    pc1.test.data.numeric <- sapply( pc1.test.data, as.numeric )
    
    
    if( identical(j,"aic" ) ){
      if( i==1  )
        auc.hc.aic <<- colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.hc) , plotROC=TRUE )
      else{
        
        auc.hc.aic <<- rbind( auc.hc.aic,colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.hc) , plotROC=TRUE ) )
      }  
    }
    else if( identical(j,"k2") ) { 
      if( i==1  )
        auc.hc.k2 <<- colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.hc) , plotROC=TRUE )
      else{
        
        auc.hc.k2 <<- rbind( auc.hc.k2,colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.hc) , plotROC=TRUE ) )
      }  
    }
    else {
      if( i==1  )
        auc.hc.bde <<- colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.hc) , plotROC=TRUE )
      else{
        
        auc.hc.bde <<- rbind( auc.hc.bde,colAUC(  pc1.test.data.numeric [,- length(pc1.test.data)] , as.numeric( pc1.pred.hc) , plotROC=TRUE ) )
      }  
    }
    
    
    }
  
    
    #print (  colAUC(  pc1.tan.auc[,- length(pc1.test.data)] , as.numeric( pc1.pred.tan) , plotROC=TRUE ) )
    
    
    drawPlot(temp.path,pc1.hc,paste("hc_",j,"_",i,sep="") ) ;
    
  }
  
  
  
  
  
  
 