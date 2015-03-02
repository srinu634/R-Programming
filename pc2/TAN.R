runTAN = function(debug) {
  
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  pc2.tan <<- tree.bayes(pc2.disc.data, "K")
  #graphviz.plot(pc2.tan)
  pc2.fitted <<- bn.fit(pc2.tan, pc2.disc.data)
  #coefficients(pc2.fitted)
  pc2.pred.tan <<- predict(pc2.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  #pc2.pred.tan <- as.numeric(pc2.pred.tan)
  #pc2.perf.tan <- performance(pc2.pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(pc2.pred.tan, pc2.test.data[, "K"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.tan <<- as.numeric(as.character(pc2.pred.tan))
  pc2.pred.tan <<- as.numeric(as.character(pc2.test.data[,"K"]))
  accuracy(f = pc2.given.tan , x = pc2.pred.tan)  #print the accuracy
  
  #score(pc2.tan, pc2.disc.data, type = "bde")
  #score(pc2.bn,pc2.disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  
  png('./plots/tan.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.tan)
  dev.off()
}