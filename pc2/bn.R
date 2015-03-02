runBN = function(debug) {
  
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  pc2.bn  <<- naive.bayes(pc2.disc.data, "K")
  pc2.pred.bn  <<- predict(pc2.bn, pc2.test.data)  #2nd parameter should be pc2.test.data
 
  
  print ( table(pc2.pred.bn,pc2.test.data[,"K"]) )  #output the prediction matrix

  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  pc2.given.bn  <<- as.numeric(as.character(pc2.pred.bn))
  pc2.predicted.bn  <<- as.numeric(as.character(pc2.test.data[,"K"]))
  print(  accuracy(f = pc2.given.bn , x = pc2.predicted.bn) ) #print the accuracy
  

  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  
  png('./plots/naive_bayes.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.bn)
  dev.off()

}