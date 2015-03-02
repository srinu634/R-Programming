runTAN = function(debug) {
  
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  pc1.tan <<- tree.bayes(pc1.disc.data, "L")
  #graphviz.plot(pc1.tan)
  pc1.fitted <<- bn.fit(pc1.tan, pc1.disc.data)
  #coefficients(pc1.fitted)
  pc1.pred.tan <<- predict(pc1.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  #pc1.pred.tan <- as.numeric(pc1.pred.tan)
  #pc1.perf.tan <- performance(pc1.pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(pc1.pred.tan, pc1.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.tan <<- as.numeric(as.character(pc1.pred.tan))
  pc1.pred.tan <<- as.numeric(as.character(pc1.test.data[,"L"]))
  accuracy(f = pc1.given.tan , x = pc1.pred.tan)  #print the accuracy
  
  #score(pc1.tan, pc1.disc.data, type = "bde")
  #score(pc1.bn,pc1.disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  
  png('./plots/tan.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc1.tan)
  dev.off()
}