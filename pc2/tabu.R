runTABU = function(debug) {
  
  
  
  score1 <- c("bde","k2","aic")
  attributes <- names(pc2.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - TABU algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc2.tabu <<- empty.graph(attributes)
    
    
    pc2.tabu <<- cextend (  tabu(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
   
    
    if(debug){
      print(str(pc2.test.data))
    }
    
    pc2.tabu.fitted <<- bn.fit(pc2.tabu,pc2.disc.data)
    
    if( debug){
      print(str(pc2.tabu.fitted))
    }
    
    pc2.tabu.pred <<- predict(pc2.tabu.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
    
    
    table(pc2.tabu.pred, pc2.test.data[, "K"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc2.given.tabu <<- as.numeric(as.character(pc2.tabu.pred))
    pc2.pred.tabu <<- as.numeric(as.character(pc2.test.data[,"K"]))
    accuracy(f = pc2.given.tabu , x = pc2.pred.tabu)  #print the accuracy
    
    png('./plots/tabu.png',units="in", width=11, height=8.5, res=300)
    graphviz.plot(pc2.tabu)
    dev.off()
    
    #graphviz.plot(pc2.tabu)
  }

}