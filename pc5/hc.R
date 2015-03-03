runHC = function(debug) {
  score1 <- c("aic","k2","bde")
  attributes <- names(pc5.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc5.hc = empty.graph(attributes)
    
    
    pc5.hc = cextend (  hc(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
    pc5.hc.fitted = bn.fit(pc5.hc,pc5.disc.data)
    
    pc5.hc.pred<- predict(pc5.hc.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
    
    table(pc5.hc.pred, pc5.test.data[, "M"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc5.given.hc <- as.numeric(as.character(pc5.hc.pred))
    pc5.pred.hc <- as.numeric(as.character(pc5.test.data[,"M"]))
    accuracy(f = pc5.given.hc , x = pc5.pred.hc)  #print the accuracy
    
    #graphviz.plot(pc5.hc)
  
  } #for 
  
  
  if(debug){
    print("Done with Score Based - Hill climbing Algorithm")
  }
  
  png(".//plots//hill_climb"+score1+".png",units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.hc)
  dev.off()
}