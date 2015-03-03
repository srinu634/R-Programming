runBN = function(debug) {
  
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  jm1.bn  <<- naive.bayes(jm1.disc.data, "v")
  jm1.pred.bn  <<- predict(jm1.bn, jm1.test.data)  #2nd parameter should be jm1.test.data
 
  
  print ( table(jm1.pred.bn,jm1.test.data[,"v"]) )  #output the prediction matrix

  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  jm1.given.bn  <<- as.numeric(as.character(jm1.pred.bn))
  jm1.predicted.bn  <<- as.numeric(as.character(jm1.disc.data[,"v"]))
  print(  accuracy(f = jm1.given.bn , x = jm1.predicted.bn) ) #print the accuracy
  

  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  
  png('./plots/naive_bayes.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(jm1.bn)
  dev.off()

}