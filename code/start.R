startProcess = function(debug){ 
  
  print("Running Classifiers")
  setwd(  results.path  )
  
  whitelist <- NULL
  
  for( i in 1:10){
    preprocess(debug,i)
    runClassifiers(debug,i,whitelist)
  }
  
  writeToExcel(); # Write results to excel
}

startProcessBAN = function(debug){
  
  print("Running BAN Classifiers")
  setwd( results.path  )

  #Build a BAN by including arc from Classification Node to every other node
 
  for(i in 1:10){
    preprocess(debug,i)
    
    len <<- length(disc.data) #Number of attributes
    from <- NULL
    for( j in 1:(len-1)){
      from <- c( from,c(   names(disc.data)[length(disc.data)] ) ) 
    }
    #from
    to <- NULL
    
    for(j in 1:(len-1) ){
      to <- c(to, names(disc.data)[j]  )
    }
    #to
    whitelist <- data.frame(from,to)
    setwd( plots.path )
    
    # Include an arc from class node to every other node
    runClassifiers(debug,i,whitelist)
  }
  
  writeToExcelBAN();

}

runClassifiers = function ( debug,i,whitelist.arcs ) {
  
  #Bayesian
  
  if( identical(whitelist.arcs,NULL) ) {
    
  print("Naive Bayes classifier"  )   #output the prediction matrix
  
  #Building a Naive Bayes classifier
  nb  <<- naive.bayes(disc.data, as.character(names(disc.data)[length(disc.data)]) )
  drawPlot("\\BN",nb,paste("bn",i,sep="")) ;
  
  nb <<- as.graphNEL(nb) #Change it to a graph structure so that it can be used by Grain Package
  nb <<- compile( grain(nb, disc.data , smooth = 0.001) ) #With parameter Smoothing
  pred.nb <<- predict( nb , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
                 predictors = names(test.data)[-length(test.data)], type = "distribution")
  auc.nb[i] <<-  auc ( roc( test.data[,length(test.data)] , data.frame(pred.nb$pred)[,1]) ) 
  
  print( paste ( "AUC IS:" , auc.nb[i]) )
  #For help, Type :: ?predict.grain
  
  
  
  if(debug) {
    print("Done with Naive Bayes classifier"  )    #output the prediction matrix
  }
 
  #End of Naive Bayes
  
  
  #TAN
    print("Running TAN")

  ################ Tree Augmented Network classifier###########################
  tan <- tree.bayes(disc.data, names(disc.data)[length(disc.data)] )
  drawPlot("\\TAN",tan,paste("tan",i,sep="")) ;
  tan <- as.graphNEL(tan) #Change it to a graph structure so that it can be used by Grain Package
  tan <- compile( grain(tan, disc.data , smooth = 0.001) ) #With parameter Smoothing
  pred.tan <- predict( tan , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
                       predictors = names(test.data)[-length(test.data)], type = "distribution")
  auc.tan[i] <<-  auc ( roc( test.data[,length(test.data)] , data.frame(pred.tan$pred)[,1]) ) 
  print( paste ( "AUC IS:" , auc.tan[i]) )
  
  if(debug){    print(" Done with Running TAN")   }

  
  #TAN : end
  
  } #if:
 
  attribute <<- names(data.d) #Global variable
  whitelist.arcs <<- NULL
  
  #########Score Based
  
  
  #HIll climb
  score1 <- c("aic","k2","bde")
  
  print("Starting Hill CLimb Algorithms")
  for( j in score1) {
    
    print( paste("Score considered : " ,j,sep="") )
  
    hc = empty.graph( names( disc.data ))
    hc = cextend ( hc ( disc.data, whitelist = whitelist.arcs , score=j ))
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\HC"
    else
      temp.path = "\\HC\\BAN" 
    drawPlot(temp.path,hc,paste("hc_",j,"_",i,sep="") ) ;
    hc <- as.graphNEL(hc) #Change it to a graph structure so that it can be used by Grain Package
    hc <- compile( grain(hc, disc.data , smooth = 0.1) ) #With parameter Smoothing
    pred.hc <- predict( hc , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
                          predictors = names(test.data)[-length(test.data)], type = "distribution")
    
    auc.val <- auc ( roc( test.data[,length(test.data)] , data.frame(pred.hc$pred)[,1]) )
    print( paste ( "AUC IS:" , auc.val) )
    
    if( j == "aic"){ 
      auc.hc.aic[i] <<- auc.val  }
    else if (j =="k2") {
      auc.hc.k2[i] <<- auc.val
    }
    else {
      auc.hc.bde[i] <<- auc.val
    }
   
    
   
    
  }
  #HIllclimb : end
  
  
  #TABU
  score1 <- c("aic","k2","bde")
  
  print("Starting TABU Algorithms")
  
  for( j in score1) {
    
    print( paste("Score considered : " ,j,sep="") )
    
    tabu = empty.graph( names( disc.data ))
    tabu = cextend ( tabu ( disc.data, whitelist = whitelist.arcs , score=j ))
    if(  identical(whitelist.arcs,NULL) ) 
      temp.path = "\\TABU"
    else
      temp.path = "\\TABU\\BAN" 
    drawPlot(temp.path,tabu,paste("tabu_",j,"_",i,sep="") ) ;
    tabu <- as.graphNEL(tabu) #Change it to a graph structure so that it can be used by Grain Package
    tabu <- compile( grain(tabu, disc.data , smooth = 0.001) ) #With parameter Smoothing
    pred.tabu <- predict( tabu , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
                         predictors = names(test.data)[-length(test.data)], type = "distribution")
   
    auc.val <- auc ( roc( test.data[,length(test.data)] , data.frame(pred.tabu$pred)[,1]) )
    print( paste ( "AUC IS:" , auc.val) )
    
    if( j == "aic"){ 
      auc.tabu.aic[i] <<- auc.val  }
    else if (j =="k2") {
      auc.tabu.k2[i] <<- auc.val
    }
    else {
      auc.tabu.bde[i] <<- auc.val
    }
  
  }
  
  #TABU : end
  
  
  #########Hybrid

    print("Running mmhc")
  
  
  #mmhc
  mmhc = empty.graph( names( disc.data ))
  mmhc = cextend ( mmhc ( disc.data, whitelist = whitelist.arcs ,score="bde" ))
  if(  identical(whitelist.arcs,NULL) ) 
    temp.path = "\\MMHC"
  else
    temp.path = "\\MMHC\\BAN" 
  drawPlot(temp.path,mmhc,paste("mmhc",i,sep="")) ;
  mmhc <- as.graphNEL(mmhc) #Change it to a graph structure so that it can be used by Grain Package
  mmhc <- compile( grain(mmhc, disc.data , smooth = 0.001) ) #With parameter Smoothing
  pred.mmhc <- predict( mmhc , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
                      predictors = names(test.data)[-length(test.data)], type = "distribution")
  auc.mmhc[i] <<-  auc ( roc( test.data[,length(test.data)] , data.frame(pred.mmhc$pred)[,1]) ) 
  print( paste ( "AUC IS:" , auc.mmhc[i]) )
  

 
  
  #MMHC : end
  
  print("Running RSMAX2")
  rsmax2 = empty.graph( names( disc.data ))
  rsmax2 = cextend ( rsmax2 ( disc.data, whitelist = whitelist.arcs  ))
  if(  identical(whitelist.arcs,NULL) ) 
    temp.path = "\\RSMAX2"
  else
    temp.path = "\\RSMAX2\\BAN" 
  drawPlot(temp.path,rsmax2,paste("RSMAX2",i,sep="")) ;
  rsmax2 <- as.graphNEL(rsmax2) #Change it to a graph structure so that it can be used by Grain Package
  rsmax2 <- compile( grain(rsmax2, disc.data , smooth = 0.001) ) #With parameter Smoothing
  pred.rsmax2 <- predict( rsmax2 , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
                        predictors = names(test.data)[-length(test.data)], type = "distribution")
  auc.rsmax2[i] <<-  auc ( roc( test.data[,length(test.data)] , data.frame(pred.rsmax2$pred)[,1]) ) 
  print( paste ( "AUC IS:" , auc.rsmax2[i]) )
  #RSMAX2:end

  #Constraint Based
  print("Running Grow-Shrink ")
  gs = empty.graph( names( disc.data ))
  gs = cextend ( gs ( disc.data, whitelist = whitelist.arcs  ))
  if(  identical(whitelist.arcs,NULL) ) 
    temp.path = "\\GS"
  else
    temp.path = "\\GS\\BAN" 
  drawPlot(temp.path,gs,paste("gs",i,sep="")) ;
  gs <- as.graphNEL(gs) #Change it to a graph structure so that it can be used by Grain Package
  gs <- compile( grain(gs, disc.data , smooth = 0.001) ) #With parameter Smoothing
  pred.gs <- predict( gs , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
                          predictors = names(test.data)[-length(test.data)], type = "distribution")
  auc.gs[i] <<-  auc ( roc( test.data[,length(test.data)] , data.frame(pred.gs$pred)[,1]) ) 
  print( paste ( "AUC IS:" , auc.gs[i]) )
  
#   mmpc = empty.graph( names( disc.data ))
#   mmpc =  cextend (  mmpc ( disc.data, whitelist = whitelist.arcs ) )
#   if(  identical(whitelist.arcs,NULL) ) 
#     temp.path = "\\MMPC"
#   else
#     temp.path = "\\MMPC\\BAN" 
#   drawPlot(temp.path,mmpc,paste("MMPC",i,sep="")) ;
#   mmpc <- as.graphNEL(mmpc) #Change it to a graph structure so that it can be used by Grain Package
#   mmpc <- compile( grain(mmpc, disc.data , smooth = 0.001) ) #With parameter Smoothing
#   pred.mmpc <- predict( mmpc , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
#                       predictors = names(test.data)[-length(test.data)], type = "distribution")
#   getAUC(pred.mmpc,test.data)
  
  
  
} # runclassifiers