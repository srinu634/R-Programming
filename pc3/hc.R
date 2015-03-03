runHC = function(debug) {
  score1 <- c("aic","k2","bde")
  attributes <- names(pc3.disc.data)
  for( i in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered is: ")
    }
    
    print(i)
    
    pc3.hc = empty.graph(attributes)
    
    
    pc3.hc = cextend (  hc(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE,score=i) ) # cextend :: makes sure that all edges are directed
    
    pc3.hc.fitted = bn.fit(pc3.hc,pc3.disc.data)
    
    pc3.hc.pred<- predict(pc3.hc.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
    
    table(pc3.hc.pred, pc3.test.data[, "L"]) #output the prediction matrix
    #Change the outputs to numeric values; 
    pc3.given.hc <- as.numeric(as.character(pc3.hc.pred))
    pc3.pred.hc <- as.numeric(as.character(pc3.test.data[,"L"]))
    accuracy(f = pc3.given.hc , x = pc3.pred.hc)  #print the accuracy
    
    #graphviz.plot(pc3.hc)
  
  } #for 
  
  
  if(debug){
    print("Done with Score Based - Hill climbing Algorithm")
  }
  
  png(".//plots//hill_climb"+score1+".png",units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.hc)
  dev.off()
}