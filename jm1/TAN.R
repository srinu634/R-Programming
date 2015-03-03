runTAN = function(debug) {
  
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  jm1.tan <<- tree.bayes(jm1.disc.data, "v")
  #graphviz.plot(jm1.tan)
  jm1.fitted <<- bn.fit(jm1.tan, jm1.disc.data)
  #coefficients(jm1.fitted)
  jm1.pred.tan <<- predict(jm1.fitted$v, jm1.test.data) #2nd parameter should be jm1.test.data
  #jm1.pred.tan <- as.numeric(jm1.pred.tan)
  #jm1.perf.tan <- performance(jm1.pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(jm1.pred.tan, jm1.test.data[, "v"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  jm1.given.tan <<- as.numeric(as.character(jm1.pred.tan))
  jm1.pred.tan <<- as.numeric(as.character(jm1.test.data[,"v"]))
  accuracy(f = jm1.given.tan , x = jm1.pred.tan)  #print the accuracy
  
  #score(jm1.tan, jm1.disc.data, type = "bde")
  #score(jm1.bn,jm1.disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  
  png('./plots/tan.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(jm1.tan)
  dev.off()
}