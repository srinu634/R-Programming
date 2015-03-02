runHC = function(debug) {
  score1 <- c("aic","k2","bde")
  attributes <- names(pc2.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc2.hc = empty.graph(attributes)
    
    
    pc2.hc = cextend (  hc(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
    pc2.hc.fitted = bn.fit(pc2.hc,pc2.disc.data)
    
    pc2.hc.pred<- predict(pc2.hc.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
    
    table(pc2.hc.pred, pc2.test.data[, "K"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc2.given.hc <- as.numeric(as.character(pc2.hc.pred))
    pc2.pred.hc <- as.numeric(as.character(pc2.test.data[,"K"]))
    accuracy(f = pc2.given.hc , x = pc2.pred.hc)  #print the accuracy
    
    #graphviz.plot(pc2.hc)
  
  } #for 
  
  
  if(debug){
    print("Done with Score Based - Hill climbing Algorithm")
  }
  
  png(".//plots//hill_climb"+score1+".png",units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.hc)
  dev.off()
}