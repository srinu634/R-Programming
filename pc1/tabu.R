runTABU = function(debug,i) {
  
  
  
  score1 <- c("bde","k2","aic")
 
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - TABU algorithm")
      print("Score considered: ")
    }
    
    print(i)
    
    pc1.tabu <<- empty.graph(names(pc1.disc.data))
    
    
    pc1.tabu <<- cextend (  tabu(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
   
    
    if(debug){
      print(str(pc1.test.data))
    }
    
    pc1.tabu.fitted <<- bn.fit(pc1.tabu,pc1.disc.data)
    
    if( debug){
      print(str(pc1.tabu.fitted))
    }
    
    pc1.tabu.pred <<- predict(pc1.tabu.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
    
    
    print ( table(pc1.tabu.pred, pc1.test.data[, "L"]) )#output the prediction matrix
    #Change the outputs to numeric values; 
    pc1.given.tabu <<- as.numeric(as.character(pc1.tabu.pred))
    pc1.pred.tabu <<- as.numeric(as.character(pc1.test.data[,"L"]))
    accuracy(f = pc1.given.tabu , x = pc1.pred.tabu)  #print the accuracy
    
    png(paste("./plots/tabu_",i,".png",sep=""),units="in", width=11, height=8.5, res=300)
    graphviz.plot(pc1.tabu)
    dev.off()
    
    #graphviz.plot(pc1.tabu)
  }

}