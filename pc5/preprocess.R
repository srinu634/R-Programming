preprocess  = function(debug) {
  
  pc5 <<- read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc5.arff") #Load the dataset
  
 # pc5 <<- sample(pc5) #Create a random permutation of the data
  
 # print(pc5)
  Letters <<- c(letters,LETTERS)
  colnames(pc5) <<- Letters[1:39]
  

 # print( str(pc5))
  
  pc5.test <<- pc5[(9*NROW(pc5)/10):NROW(pc5),] # Build a Test set
  pc5 <<- pc5[1:(9*NROW(pc5)/10-1),]
  
  # str(pc5) 
  
  # Change the Strings Y,N to 1,0
  
  if( debug){
    print("Converting N,Y to 0,1")
  }
  
  pc5$M  <<- as.numeric(factor(pc5$M , levels=c("FALSE" ,"TRUE") ) ) 
  pc5.test$M  <<- as.numeric(pc5.test$M , levels=c("FALSE" ,"TRUE") ) 
  
  
  if( debug){
    print("Discretising data")
  }
  
  #discretize the data for bayesian networks
  pc5.disc  <<- chiM(pc5, alpha = 0.05) 
  pc5.test.disc  <<- chiM(pc5.test,alpha=0.05)
  
  
  #load the data into a data frame
  pc5.disc.data  <<- data.frame(pc5.disc$Disc.data)
  pc5.test.data  <<- data.frame(pc5.test.disc$Disc.data)
 
  
  if( debug){
    print("Factorising Discretised data")
  }
  
  #Change all the discretized values into factors
  pc5.disc.data[,names(pc5)]  <<- lapply(pc5.disc.data[,names(pc5)] , factor) 
  pc5.test.data[,names(pc5.test)]  <<- lapply(pc5.test.data[,names(pc5.test)] , factor) 
  
  
  if( debug){
    print("Reomving Single factored variables")
  }
  
  
  #Bayesian Networks can't cope up with single factored variables , so we have to remove all the single factored variables
  excludevars  <<- NULL
  
  for ( i in 1:length(names(pc5.disc.data) ) ) {
    print(length( levels( pc5.disc.data[,i]) ) )
    print(length( levels( pc5.test.data[,i]) ) )
    
    if ( length( levels( pc5.disc.data[,i]) ) == 1 ) {
      excludevars  <<- c(excludevars,names(pc5.disc.data)[i])
    } 
    if ( length( levels( pc5.test.data[,i] ) ) == 1) {
      excludevars  <<- c(excludevars,names(pc5.test.data)[i])
    }
   # if( length(levels( pc5.disc.data[,i])) != length(levels( pc5.test.data[,i]))) #AS they are not able to cope up with such instances
    #  excludevars  <<- c(excludevars,names(pc5.test.data)[i])
  } #for
  
  
  
  excludevars  <<- unique(excludevars)
  
  if( debug){
    print("Single factored variables are:")
    print(excludevars)
  } 
  
  excludevars  <<-  names(pc5) %in% excludevars
  pc5.disc.data  <<- pc5.disc.data[ !excludevars  ]
  pc5.test.data  <<- pc5.test.data[ !excludevars  ]


} #preprocess