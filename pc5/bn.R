runBN = function(debug) {
  
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  pc5.bn  <<- naive.bayes(pc5.disc.data, "M")
  pc5.pred.bn  <<- predict(pc5.bn, pc5.test.data)  #2nd parameter should be pc5.test.data
 
  
  print ( table(pc5.pred.bn,pc5.test.data[,"M"]) )  #output the prediction matrix

  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  pc5.given.bn  <<- as.numeric(as.character(pc5.pred.bn))
  pc5.predicted.bn  <<- as.numeric(as.character(pc5.disc.data[,"M"]))
  print(  accuracy(f = pc5.given.bn , x = pc5.predicted.bn) ) #print the accuracy
  

  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  
  png('./plots/naive_bayes.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.bn)
  dev.off()

}