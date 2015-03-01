preprocess  = function(debug) {
  
  pc1 <<- read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc1.arff") #Load the dataset
  
  Letters <<- c(letters,LETTERS)
  colnames(pc1) <<- Letters[1:38]

  print( str(pc1))
  
  pc1.test <<- pc1[(9*NROW(pc1)/10):NROW(pc1),] # Build a Test set
  pc1 <<- pc1[1:(9*NROW(pc1)/10-1),]
  
  # str(pc1) 
  
  # Change the Strings Y,N to 1,0
  
  if( debug){
    print("Converting N,Y to 0,1")
  }
  
  pc1$L  <<- as.numeric(factor(pc1$L , levels=c("N" ,"Y") ) ) 
  pc1.test$L  <<- as.numeric(pc1.test$L , levels=c("N" ,"Y") ) 
  
  
  if( debug){
    print("Discretising data")
  }
  
  #discretize the data for bayesian networks
  pc1.disc  <<- chiM(pc1, alpha = 0.05) 
  pc1.test.disc  <<- chiM(pc1.test,alpha=0.05)
  
  
  #load the data into a data frame
  pc1.disc.data  <<- data.frame(pc1.disc$Disc.data)
  pc1.test.data  <<- data.frame(pc1.test.disc$Disc.data)
 
  
  if( debug){
    print("Factorising Discretised data")
  }
  
  #Change all the discretized values into factors
  pc1.disc.data[,names(pc1)]  <<- lapply(pc1.disc.data[,names(pc1)] , factor) 
  pc1.test.data[,names(pc1.test)]  <<- lapply(pc1.test.data[,names(pc1.test)] , factor) 
  
  
  if( debug){
    print("Reomving Single factored variables")
  }
  
  
  #Bayesian Networks can't cope up with single factored variables , so we have to remove all the single factored variables
  excludevars  <<- NULL
  
  for ( i in 1:length(names(pc1.disc.data) ) ) {
    print(length( levels( pc1.disc.data[,i]) ) )
    print(length( levels( pc1.test.data[,i]) ) )
    
    if ( length( levels( pc1.disc.data[,i]) ) == 1 ) {
      excludevars  <<- c(excludevars,names(pc1.disc.data)[i])
    } 
    if ( length( levels( pc1.test.data[,i] ) ) == 1) {
      excludevars  <<- c(excludevars,names(pc1.test.data)[i])
    } 
  } #for
  
  
  
  excludevars  <<- unique(excludevars)
  
  if( debug){
    print("Single factored variables are:")
    print(excludevars)
  } 
  
  excludevars  <<-  names(pc1) %in% excludevars
  pc1.disc.data  <<- pc1.disc.data[ !excludevars  ]
  pc1.test.data  <<- pc1.test.data[ !excludevars  ]


} #preprocess