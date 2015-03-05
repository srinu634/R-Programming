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
    
    print ( table(pc1.hc.pred, pc1.test.data[, "L"]) ) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc1.given.hc <- as.numeric(as.character(pc1.hc.pred))
    pc1.pred.hc <- as.numeric(as.character(pc1.test.data[,"L"]))
    accuracy(f = pc1.given.hc , x = pc1.pred.hc)  #print the accuracy
    
  
   
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\HC"
    else
      temp.path = "\\HC\\BAN" 
    
    
    drawPlot(temp.path,pc1.hc,paste("hc_",j,"_",i,sep="") ) ;
    
  }
  
  
  
  
  }
  
 