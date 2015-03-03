runTAN = function(debug) {
  
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  pc3.tan <<- tree.bayes(pc3.disc.data, "L")
  #graphviz.plot(pc3.tan)
  pc3.fitted <<- bn.fit(pc3.tan, pc3.disc.data)
  #coefficients(pc3.fitted)
  pc3.pred.tan <<- predict(pc3.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  #pc3.pred.tan <- as.numeric(pc3.pred.tan)
  #pc3.perf.tan <- performance(pc3.pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(pc3.pred.tan, pc3.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.tan <<- as.numeric(as.character(pc3.pred.tan))
  pc3.pred.tan <<- as.numeric(as.character(pc3.test.data[,"L"]))
  accuracy(f = pc3.given.tan , x = pc3.pred.tan)  #print the accuracy
  
  #score(pc3.tan, pc3.disc.data, type = "bde")
  #score(pc3.bn,pc3.disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  
  png('./plots/tan.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.tan)
  dev.off()
}