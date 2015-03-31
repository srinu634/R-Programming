# nb  <<- naive.bayes(disc.data, as.character(names(disc.data)[length(disc.data)]) )
# nb <<- as.graphNEL(nb) #Change it to a graph structure so that it can be used by Grain Package
# nb <<- compile( grain(nb, disc.data , smooth = 0.0001) ) #With parameter Smoothing
# 
# pred.nb <<- predict( nb , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
#                      predictors = names(test.data)[-length(test.data)], type = "distribution")

# getAUC ( pred.bn, test.data )

getAUC = function ( prediction, testset ) {
  
  print("In getAUC")
  x <<- data.frame( prediction$pred )
 # print ( prediction$pred)
  y  <- x[,1] >= x[,2] # if TRUE , then predictied as Non defective or else as Defective.
  
  #print ( y )
  
  pred.vals <- NULL
  pred.labels <- NULL
  count.N = as.integer(0)
  count.Y = as.integer(0)
  

  for( i in 1: NROW(testset) ) {
    if( y[i] == TRUE ) # Non-Defective
    {
      pred.vals[i] <- x[,1][i]
      pred.labels[i] <- as.character('N')
      count.N = as.integer ( count.N + 1 )
    }
    else #Defective
    {
     pred.vals[i] <- x[,2][i] 
     pred.labels[i] <- as.character('Y')
     count.Y = as.integer ( count.Y + 1 )
    }
  }
  
  print(  count.N )
  print( count.Y )

  pred.labels <- data.frame (  factor(pred.labels , levels=c("N" ,"Y") )  )
  pred.vals <- data.frame( pred.vals)
  #pred.labels <- pred.labels - rep(as.integer(1) , times = length(labels)  )
 
 
 pred <- prediction ( pred.vals , pred.labels )
 #print(pred)
 #plot ( performance(pred,"sens","spec") )
 
 pred1 <- prediction ( x[,2] , testset[,length(testset)])
 #print( performance (pred1,"fpr","tpr") )
 
 #data.frame ( labels, pred.vals) #Predicted Labels along with their prior probs
  actual <- as.numeric ( factor ( testset[,length(testset)] , levels = c("N","Y") ) ) - rep( as.integer(1) , times = length(labels) )
  
 
 #print(actual)
   
  
 
  #prediction
  
} #getAUC