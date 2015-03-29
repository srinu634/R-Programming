startProcess = function(debug,i){ 
  setwd(paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\",dataset.name,sep=""))
  
  preprocess(debug,i)
  
  runClassifiers(debug,i)

  # Include an arc from class node to every other node
  len <<- length(disc.data) #Number of attributes
  from <<- NULL
  for( j in 1:(len-1)){
    from <<- c( from,c(   names(disc.data)[length(disc.data)] ) ) 
  }
  #from
  to <<- NULL
  
  for(j in 1:(len-1) ){
    to <<- c(to, names(disc.data)[j]  )
  }
  #to
  
  whitelist.arcs <<- data.frame(from,to)
  setwd(paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\",dataset.name,
              "\\plots",sep=""))
  
  
  #Build a BAN by including arc from Classification Node to every other node
  
  
  runClassifiers(debug,i)
  
  
}

runClassifiers = function ( debug,i) {
  #Bayesian
  if(debug) {
    print("In Naive Bayes classifier"  )   #output the prediction matrix
  }
  
  #Building a Naive Bayes classifier
  bn  <<- naive.bayes(disc.data, as.character(names(disc.data)[length(disc.data)]) )
  pred.bn  <<- predict(bn, test.data,prob=TRUE)  #2nd parameter should be test.data
  
  
  print ( table(pred.bn, test.data[,length(test.data)] ) )  #output the prediction matrix
  
  #Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
  given.bn  <<- as.numeric(as.character(pred.bn))
  predicted.bn  <<- as.numeric(as.character(test.data[,length(test.data)]))
  print(  accuracy(f = given.bn , x = predicted.bn) ) #print the accuracy
  
  #bn.auc <- sapply( test.data, as.numeric )
  #print (  colAUC( bn.auc[,- length(test.data)] , as.numeric( predicted.bn) , plotROC=TRUE ) )
  
  
  #   if( i==1  )
  #     auc.bn <<- colAUC(  bn.auc[,- length(test.data)] , as.numeric( pred.bn) , plotROC=TRUE )
  #   else{
  #     
  #     auc.bn <<- rbind( auc.bn,colAUC(  bn.auc[,- length(test.data)] , as.numeric( pred.bn) , plotROC=TRUE ) )
  #    }
  #   
  #   print (  colAUC(  bn.auc[,- length(test.data)] , as.numeric( pred.bn) , plotROC=TRUE ) )
  #   colAUC(  bn.auc[,- length(test.data)] , as.numeric( pred.bn) , plotROC=TRUE )
  #   
  
  
  
  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
  
  drawPlot("\\BN",bn,paste("bn",i,sep="")) ;
  
  #End of Naive Bayes
  
  
  #TAN
  if(debug){
    print("Running TAN")
  }
  ################ Tree Augmented Network classifier###########################
  tan <- tree.bayes(disc.data, names(disc.data)[length(disc.data)] )
  #graphviz.plot(tan)
  fitted <- bn.fit(tan , disc.data)
  #coefficients(fitted)
  pred.tan <<- predict(fitted, test.data,prob=TRUE) #2nd parameter should be test.data
  #pred.tan <- as.numeric(pred.tan)
  #perf.tan <- performance(pred.tan,  measure = "tpr",x.measure =   "fpr")
  print ( table(pred.tan, test.data[,length(test.data)]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  given.tan <<- as.numeric(as.character(pred.tan))
  pred.tan <<- as.numeric(as.character(test.data[,length(test.data)]))
  accuracy(f = given.tan , x = pred.tan)  #print the accuracy
  
  #   tan.auc <- sapply( test.data, as.numeric )
  #   print (  colAUC( tan.auc[,- length(test.data)] , as.numeric( pred.tan) , plotROC=TRUE ) )
  #   
  
  #score(tan, disc.data, type = "bde")
  #score(tan,disc.data,type="bde")
  if(debug){
    print(" Done with Running TAN")
  }
  
  
  #   tan.auc <- sapply( test.data, as.numeric )
  #   print (  colAUC( tan.auc[,- length(test.data)] , as.numeric( pred.tan) , plotROC=TRUE ) )
  #   
  #   
  #   if( i==1  )
  #     auc.tan <<- colAUC(  tan.auc[,- length(test.data)] , as.numeric( pred.tan) , plotROC=TRUE )
  #   else{
  #     
  #     auc.tan <<- rbind( auc.tan,colAUC(  tan.auc[,- length(test.data)] , as.numeric( pred.tan) , plotROC=TRUE ) )
  #   }
  #   
  #   print (  colAUC(  tan.auc[,- length(test.data)] , as.numeric( pred.tan) , plotROC=TRUE ) )
  #   
  #   
  drawPlot("\\TAN",tan,paste("tan",i,sep="")) ;
  
  #TAN : end
  
  
  
  attribute <<- names(data.d) #Global variable
  whitelist.arcs <<- NULL
  
  #########Score Based
  
  
  #HIll climb
  score1 <- c("aic","k2","bde")
  
  if( debug) { print("Starting Hill CLimb Algorithms")}
  for( j in score1) {
    
    if(debug){
      print("Running Score Based - Hill climbing Algorithm")
      print("Score considered : ")
    }
    
    print(i)
    string = "L" ;
    hc = empty.graph( names( disc.data ))
    
    
    
    hc.fitted = bn.fit(hc,disc.data,score=score1[j],whitelist=whitelist.arcs)
    
    hc.pred<- predict(hc.fitted, test.data,node = names(disc.data)[length(disc.data)] ) #2nd parameter should be test.data
    
    print ( table(hc.pred, test.data[,length(test.data)]) ) #output the prediction matrix
    #Change the outputs to numeric values; 
    given.hc <- as.numeric(as.character(hc.pred))
    pred.hc <- as.numeric(as.character(test.data[,length(test.data)]))
    accuracy(f = given.hc , x = pred.hc)  #print the accuracy
    
    #     hc.auc <- sapply( test.data, as.numeric )
    #     
    
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\HC"
    else
      temp.path = "\\HC\\BAN" 
    #     test.data.numeric <- sapply( test.data, as.numeric )
    #     
    #     
    #     if( identical(j,"aic" ) ){
    #       if( i==1  )
    #         auc.hc.aic <<- colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.hc) , plotROC=TRUE )
    #       else{
    #         
    #         auc.hc.aic <<- rbind( auc.hc.aic,colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.hc) , plotROC=TRUE ) )
    #       }  
    #     }
    #     else if( identical(j,"k2") ) { 
    #       if( i==1  )
    #         auc.hc.k2 <<- colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.hc) , plotROC=TRUE )
    #       else{
    #         
    #         auc.hc.k2 <<- rbind( auc.hc.k2,colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.hc) , plotROC=TRUE ) )
    #       }  
    #     }
    #     else {
    #       if( i==1  )
    #         auc.hc.bde <<- colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.hc) , plotROC=TRUE )
    #       else{
    #         
    #         auc.hc.bde <<- rbind( auc.hc.bde,colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.hc) , plotROC=TRUE ) )
    #       }  
    #     }
    
    drawPlot(temp.path,hc,paste("hc_",j,"_",i,sep="") ) ;
    
    print("Print the prior probs")
     test = getPriorProbsPos(hc.fitted, test.data) 
    print(test)
  }
  #HIllclimb : end
  
  
  #TABU
  score1 <- c("bde","k2","aic")
  
  for( j in score1) {
    
    if(debug){
      print("Running Score Based - TABU algorithm")
      print("Score considered: ")
    }
    
    print(i)
    
    tabu = empty.graph(names(disc.data))
    
    
    tabu = cextend (  tabu(disc.data,whitelist = whitelist.arcs,debug=FALSE,score=j) ) # cextend :: makes sure that all edges are directed
    
    
    
    if(debug){
      print(str(test.data))
    }
    
    tabu.fitted = bn.fit(tabu,disc.data)
    
    if( debug){
      print(str(tabu.fitted))
    }
    
    tabu.pred = predict(tabu.fitted, test.data,node= names(disc.data)[length(disc.data)]) #2nd parameter should be test.data
    
    
    print ( table(tabu.pred, test.data[,length(test.data)]) )#output the prediction matrix
    #Change the outputs to numeric values; 
    given.tabu <<- as.numeric(as.character(tabu.pred))
    pred.tabu <<- as.numeric(as.character(      test.data[,length(test.data)]      ))
    accuracy(f = given.tabu , x = pred.tabu)  #print the accuracy
    
    # tabu.auc <- sapply( test.data, as.numeric )
    # print (  colAUC(  tabu.auc[,- length(test.data)] , as.numeric( pred.tabu) , plotROC=TRUE ) )
    
    
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\TABU"
    else
      temp.path = "\\TABU\\BAN" 
    
    #
    test.data.numeric <- sapply( test.data, as.numeric )
    
    
    #     if( identical(j,"aic" ) ){
    #       if( i==1  )
    #         auc.tabu.aic <<- colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.tabu) , plotROC=TRUE )
    #       else{
    #         
    #         auc.tabu.aic <<- rbind( auc.tabu.aic,colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.tabu) , plotROC=TRUE ) )
    #       }  
    #     }
    #     else if( identical(j,"k2") ) { 
    #       if( i==1  )
    #         auc.tabu.k2 <<- colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.tabu) , plotROC=TRUE )
    #       else{
    #         
    #         auc.tabu.k2 <<- rbind( auc.tabu.k2,colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.tabu) , plotROC=TRUE ) )
    #       }  
    #     }
    #     else {
    #       if( i==1  )
    #         auc.tabu.bde <<- colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.tabu) , plotROC=TRUE )
    #       else{
    #         
    #         auc.tabu.bde <<- rbind( auc.tabu.bde,colAUC(  test.data.numeric [,- length(test.data)] , as.numeric( pred.tabu) , plotROC=TRUE ) )
    #       }  
    #     }
    #
    
    
    drawPlot(temp.path,tabu,paste("tabu_",j,"_",i,sep="") ) ;
  }
  
  #TABU : end
  
  
  #########Hybrid
  
  if(debug) {
    print("Running Hybrid Max-Min Hill climb Algorithm")
  }
  
  #attributes <- names(disc.data)
  
  if(debug) {
    print("Initialising Graph")
  }
  
  mmhc = empty.graph(names(disc.data))
  
  mmhc = cextend (  mmhc(disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  if(debug) {
    print("Learning the Parameters")
  }
  
  mmhc.fitted = bn.fit(mmhc,disc.data)
  
  mmhc.pred = predict(mmhc.fitted, test.data,node=as.character(names(disc.data)[length(disc.data)])) #2nd parameter should be test.data
  
  print( table(mmhc.pred, test.data[,length(test.data)]     )) #output the prediction matrix
  #Change the outputs to numeric values; 
  given.mmhc <- as.numeric(as.character(mmhc.pred))
  pred.mmhc <- as.numeric(as.character(  test.data[,length(test.data)]     )  )
  print( accuracy(f = given.mmhc , x = pred.mmhc) )  #print the accuracy
  
  
  
  if(  identical(whitelist.arcs,NULL) ) 
    temp.path = "\\MMHC"
  else
    temp.path = "\\MMHC\\BAN" 
  
  # 
  mmhc.auc <- sapply( test.data, as.numeric )
  ##print (  colAUC(  mmhc.auc[,- length(test.data)] , as.numeric( pred.mmhc) , plotROC=TRUE ) )
  
  mmhc.auc <- sapply( test.data, as.numeric )
  #print (  colAUC( mmhc.auc[,- length(test.data)] , as.numeric( pred.mmhc) , plotROC=TRUE ) )
  
  #   
  #   if( i==1  )
  #     auc.mmhc <<- colAUC(  mmhc.auc[,- length(test.data)] , as.numeric( pred.mmhc) , plotROC=TRUE )
  #   else{
  #     
  #     auc.mmhc <<- rbind( auc.mmhc,colAUC(  mmhc.auc[,- length(test.data)] , as.numeric( pred.mmhc) , plotROC=TRUE ) )
  #   }
  #   
  #   print (  colAUC(  mmhc.auc[,- length(test.data)] , as.numeric( pred.mmhc) , plotROC=TRUE ) )
  #   #
  
  drawPlot(temp.path,mmhc,paste("mmhc",i,sep="")) ;
  
  #MMHC : end
  
  rsmax2 = empty.graph(names(disc.data))
  
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  rsmax2 = cextend (  rsmax2(disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  rsmax2.fitted = bn.fit(rsmax2,disc.data)
  
  rsmax2.pred<- predict(rsmax2.fitted, test.data,node=as.character(names(disc.data)[length(disc.data)])) #2nd parameter should be test.data
  
  print( table(rsmax2.pred, test.data[,length(test.data)]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  given.rsmax2 <- as.numeric(as.character(rsmax2.pred))
  pred.rsmax2 <- as.numeric(as.character(test.data[,length(test.data)]))
  print( accuracy(f = given.rsmax2 , x = pred.rsmax2)  ) #print the accuracy
  
  if(  identical(whitelist.arcs,NULL) ) 
    temp.path = "\\RSMAX2"
  else
    temp.path = "\\RSMAX2\\BAN" 
  
  rsmax2.auc <- sapply( test.data, as.numeric )
  ##print (  colAUC(  rsmax2.auc[,- length(test.data)] , as.numeric( pred.rsmax2) , plotROC=TRUE ) )
  
  
  
  # 
  rsmax2.auc <- sapply( test.data, as.numeric )
  #print (  colAUC(  rsmax2.auc[,- length(test.data)] , as.numeric( pred.rsmax2) , plotROC=TRUE ) )
  
  rsmax2.auc <- sapply( test.data, as.numeric )
  # print (  colAUC( rsmax2.auc[,- length(test.data)] , as.numeric( pred.rsmax2) , plotROC=TRUE ) )
  
  
  #  if( i==1  )
  #    auc.rsmax2 <<- colAUC(  rsmax2.auc[,- length(test.data)] , as.numeric( pred.rsmax2) , plotROC=TRUE )
  #  else{
  #    
  #    auc.rsmax2 <<- rbind( auc.rsmax2,colAUC(  rsmax2.auc[,- length(test.data)] , as.numeric( pred.rsmax2) , plotROC=TRUE ) )
  #  }
  #  
  #  print (  colAUC(  rsmax2.auc[,- length(test.data)] , as.numeric( pred.rsmax2) , plotROC=TRUE ) )
  #  #
  #  
  
  drawPlot(temp.path,rsmax2,paste("RSMAX2",i,sep="")) ;
  #RSMAX2:end
  # runHITON(debug,i)
  
  
  
  #Constraint Based
  
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  gs = cextend (  gs(disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  # see set.arc to set the arc directions
  #class(gs)
  # modelstring(gs)
  # arcs(gs)
  # gs$arcs ## To see the info about arcs
  # gs$nodes
  
  
  gs.fitted = bn.fit(gs,disc.data)
  
  gs.pred = predict(gs.fitted, test.data,method="parents",node=as.character(names(disc.data)[length(disc.data)])) #2nd parameter should be test.data
  
  print( table(gs.pred, test.data[,length(test.data)] )  ) #output the prediction matrix
  #Change the outputs to numeric values; 
  given.gs <- as.numeric(as.character(gs.pred))
  pred.gs <- as.numeric(as.character(    test.data[,length(test.data)]    ))
  print(  accuracy(f = given.gs , x = pred.gs) ) #print the accuracy
  
  
  if(  identical(whitelist.arcs,NULL) ) 
    temp.path = "\\GS"
  else
    temp.path = "\\GS\\BAN" 
  
  gs.auc <- sapply( test.data, as.numeric )
  #print (  colAUC(  gs.auc[,- length(test.data)] , as.numeric( pred.gs) , plotROC=TRUE ) )
  
  
  # 
  gs.auc <- sapply( test.data, as.numeric )
  #print (  colAUC(  gs.auc[,- length(test.data)] , as.numeric( pred.gs) , plotROC=TRUE ) )
  
  gs.auc <- sapply( test.data, as.numeric )
  #print (  colAUC( gs.auc[,- length(test.data)] , as.numeric( pred.gs) , plotROC=TRUE ) )
  
  
  #  if( i==1  )
  #    auc.gs <<- colAUC(  gs.auc[,- length(test.data)] , as.numeric( pred.gs) , plotROC=TRUE )
  #  else{
  #    
  #    auc.gs <<- rbind( auc.gs,colAUC(  gs.auc[,- length(test.data)] , as.numeric( pred.gs) , plotROC=TRUE ) )
  #  }
  #  
  #  print (  colAUC(  gs.auc[,- length(test.data)] , as.numeric( pred.gs) , plotROC=TRUE ) )
  #  #
  #
  
  pos.gs = getPriorProbsPos(gs.fitted , disc.data)
  neg.gs = getPriorProbsNeg(gs.fitted, disc.data)
  
  #print(pos.gs)
  #print(neg.gs)
  
  
  drawPlot(temp.path,gs,paste("gs",i,sep="")) ;
  
} # runclassifiers