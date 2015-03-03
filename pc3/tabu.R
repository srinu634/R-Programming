runTABU = function(debug) {
  
  
  
  score1 <- c("bde","k2","aic")
  attributes <- names(pc3.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - TABU algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc3.tabu <<- empty.graph(attributes)
    
    
    pc3.tabu <<- cextend (  tabu(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
   
    
    if(debug){
      print(str(pc3.test.data))
    }
    
    pc3.tabu.fitted <<- bn.fit(pc3.tabu,pc3.disc.data)
    
    if( debug){
      print(str(pc3.tabu.fitted))
    }
    
    pc3.tabu.pred <<- predict(pc3.tabu.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
    
    
    table(pc3.tabu.pred, pc3.test.data[, "L"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc3.given.tabu <<- as.numeric(as.character(pc3.tabu.pred))
    pc3.pred.tabu <<- as.numeric(as.character(pc3.test.data[,"L"]))
    accuracy(f = pc3.given.tabu , x = pc3.pred.tabu)  #print the accuracy
    
    png('./plots/tabu.png',units="in", width=11, height=8.5, res=300)
    graphviz.plot(pc3.tabu)
    dev.off()
    
    #graphviz.plot(pc3.tabu)
  }

}