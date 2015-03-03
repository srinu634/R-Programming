runTABU = function(debug) {
  
  
  
  score1 <- c("bde","k2","aic")
  attributes <- names(pc5.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - TABU algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc5.tabu <<- empty.graph(attributes)
    
    
    pc5.tabu <<- cextend (  tabu(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
   
    
    if(debug){
      print(str(pc5.test.data))
    }
    
    pc5.tabu.fitted <<- bn.fit(pc5.tabu,pc5.disc.data)
    
    if( debug){
      print(str(pc5.tabu.fitted))
    }
    
    pc5.tabu.pred <<- predict(pc5.tabu.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
    
    
    table(pc5.tabu.pred, pc5.test.data[, "M"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc5.given.tabu <<- as.numeric(as.character(pc5.tabu.pred))
    pc5.pred.tabu <<- as.numeric(as.character(pc5.test.data[,"M"]))
    accuracy(f = pc5.given.tabu , x = pc5.pred.tabu)  #print the accuracy
    
    png('./plots/tabu.png',units="in", width=11, height=8.5, res=300)
    graphviz.plot(pc5.tabu)
    dev.off()
    
    #graphviz.plot(pc5.tabu)
  }

}