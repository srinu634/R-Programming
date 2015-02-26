runHC = function(debug) {
  score1 <- c("bde","k2","aic")
  attributes <- names(pc1.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc1.hc = empty.graph(attributes)
    
    
    pc1.hc = cextend (  hc(pc1.disc.data,whitelist = NULL,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
    pc1.hc.fitted = bn.fit(pc1.hc,pc1.disc.data)
    
    pc1.hc.pred<- predict(pc1.hc.fitted$Defective, pc1.test.data) #2nd parameter should be pc1.test.data
    
    table(pc1.hc.pred, pc1.test.data[, "Defective"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc1.given.hc <- as.numeric(as.character(pc1.hc.pred))
    pc1.pred.hc <- as.numeric(as.character(pc1.test.data[,"Defective"]))
    accuracy(f = pc1.given.hc , x = pc1.pred.hc)  #print the accuracy
    
    #graphviz.plot(pc1.hc)
  
  } #for 
  
  
  if(debug){
    print("Done with Score Based - Hill climbing Algorithm")
  }

}