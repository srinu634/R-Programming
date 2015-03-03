runTAN = function(debug) {
  
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  pc5.tan <<- tree.bayes(pc5.disc.data, "M")
  #graphviz.plot(pc5.tan)
  pc5.fitted <<- bn.fit(pc5.tan, pc5.disc.data)
  #coefficients(pc5.fitted)
  pc5.pred.tan <<- predict(pc5.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  #pc5.pred.tan <- as.numeric(pc5.pred.tan)
  #pc5.perf.tan <- performance(pc5.pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(pc5.pred.tan, pc5.test.data[, "M"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.tan <<- as.numeric(as.character(pc5.pred.tan))
  pc5.pred.tan <<- as.numeric(as.character(pc5.test.data[,"M"]))
  accuracy(f = pc5.given.tan , x = pc5.pred.tan)  #print the accuracy
  
  #score(pc5.tan, pc5.disc.data, type = "bde")
  #score(pc5.bn,pc5.disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  
  png('./plots/tan.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.tan)
  dev.off()
}