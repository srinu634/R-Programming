preprocess  = function(debug) {
  
  pc3 <<- read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc3.arff") #Load the dataset
  
 # pc3 <<- sample(pc3) #Create a random permutation of the data
  
 # print(pc3)
  Letters <<- c(letters,LETTERS)
  colnames(pc3) <<- Letters[1:38]

 # print( str(pc3))
  
  pc3.test <<- pc3[(9*NROW(pc3)/10):NROW(pc3),] # Build a Test set
  pc3 <<- pc3[1:(9*NROW(pc3)/10-1),]
  
  # str(pc3) 
  
  # Change the Strings Y,N to 1,0
  
  if( debug){
    print("Converting N,Y to 0,1")
  }
  
  pc3$L  <<- as.numeric(factor(pc3$L , levels=c("N" ,"Y") ) ) 
  pc3.test$L  <<- as.numeric(pc3.test$L , levels=c("N" ,"Y") ) 
  
  
  if( debug){
    print("Discretising data")
  }
  
  #discretize the data for bayesian networks
  pc3.disc  <<- chiM(pc3, alpha = 0.05) 
  pc3.test.disc  <<- chiM(pc3.test,alpha=0.05)
  
  
  #load the data into a data frame
  pc3.disc.data  <<- data.frame(pc3.disc$Disc.data)
  pc3.test.data  <<- data.frame(pc3.test.disc$Disc.data)
 
  
  if( debug){
    print("Factorising Discretised data")
  }
  
  #Change all the discretized values into factors
  pc3.disc.data[,names(pc3)]  <<- lapply(pc3.disc.data[,names(pc3)] , factor) 
  pc3.test.data[,names(pc3.test)]  <<- lapply(pc3.test.data[,names(pc3.test)] , factor) 
  
  
  if( debug){
    print("Reomving Single factored variables")
  }
  
  
  #Bayesian Networks can't cope up with single factored variables , so we have to remove all the single factored variables
  excludevars  <<- NULL
  
  for ( i in 1:length(names(pc3.disc.data) ) ) {
    print(length( levels( pc3.disc.data[,i]) ) )
    print(length( levels( pc3.test.data[,i]) ) )
    
    if ( length( levels( pc3.disc.data[,i]) ) == 1 ) {
      excludevars  <<- c(excludevars,names(pc3.disc.data)[i])
    } 
    if ( length( levels( pc3.test.data[,i] ) ) == 1) {
      excludevars  <<- c(excludevars,names(pc3.test.data)[i])
    }
   # if( length(levels( pc3.disc.data[,i])) != length(levels( pc3.test.data[,i]))) #AS they are not able to cope up with such instances
    #  excludevars  <<- c(excludevars,names(pc3.test.data)[i])
  } #for
  
  
  
  excludevars  <<- unique(excludevars)
  
  if( debug){
    print("Single factored variables are:")
    print(excludevars)
  } 
  
  excludevars  <<-  names(pc3) %in% excludevars
  pc3.disc.data  <<- pc3.disc.data[ !excludevars  ]
  pc3.test.data  <<- pc3.test.data[ !excludevars  ]


} #preprocess