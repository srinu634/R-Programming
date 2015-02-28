runTABU = function(debug) {
  
  
  
  score1 <- c("bde","k2","aic")
  attributes <- names(pc1.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc1.tabu = empty.graph(attributes)
    
    
    pc1.tabu = cextend (  tabu(pc1.disc.data,whitelist = NULL,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
    pc1.tabu.fitted = bn.fit(pc1.tabu,pc1.disc.data)
    
    pc1.tabu.pred<- predict(pc1.tabu.fitted$Defective, pc1.test.data) #2nd parameter should be pc1.test.data
    
    table(pc1.tabu.pred, pc1.test.data[, "Defective"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc1.given.tabu <- as.numeric(as.character(pc1.tabu.pred))
    pc1.pred.tabu <- as.numeric(as.character(pc1.test.data[,"Defective"]))
    accuracy(f = pc1.given.tabu , x = pc1.pred.tabu)  #print the accuracy
    
    #graphviz.plot(pc1.tabu)
}