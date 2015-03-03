runBN = function(debug,i) {
  
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  pc1.bn  <<- naive.bayes(pc1.disc.data, "L")
  pc1.pred.bn  <<- predict(pc1.bn, pc1.test.data)  #2nd parameter should be pc1.test.data
 
  
  print ( table(pc1.pred.bn,pc1.test.data[,"L"]) )  #output the prediction matrix

  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  pc1.given.bn  <<- as.numeric(as.character(pc1.pred.bn))
  pc1.predicted.bn  <<- as.numeric(as.character(pc1.test.data[,"L"]))
  print(  accuracy(f = pc1.given.bn , x = pc1.predicted.bn) ) #print the accuracy
  

  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  

  
  png('./plots/naive_bayes.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc1.bn)
  dev.off()

}