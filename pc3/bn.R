runBN = function(debug) {
  
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  pc3.bn  <<- naive.bayes(pc3.disc.data, "L")
  pc3.pred.bn  <<- predict(pc3.bn, pc3.test.data)  #2nd parameter should be pc3.test.data
 
  
  print ( table(pc3.pred.bn,pc3.test.data[,"L"]) )  #output the prediction matrix

  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  pc3.given.bn  <<- as.numeric(as.character(pc3.pred.bn))
  pc3.predicted.bn  <<- as.numeric(as.character(pc3.disc.data[,"L"]))
  print(  accuracy(f = pc3.given.bn , x = pc3.predicted.bn) ) #print the accuracy
  

  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  
  png('./plots/naive_bayes.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.bn)
  dev.off()

}