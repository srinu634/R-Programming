preprocess  = function(debug) {
  
  pc2 <<- read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc2.arff") #Load the dataset
  
  #pc2.temp <<- sample(pc2,) #Randomise the data
  
  #pc2 <<- pc2.temp
  
 
  
  
 # pc2 <<- sample(pc2) #Create a random permutation of the data
  
 # print(pc2)
  Letters <<- c(letters,LETTERS)
  colnames(pc2) <<- Letters[1:37]

 #print( str(pc2))
  
  pc2.test <<- pc2[(9*NROW(pc2)/10):NROW(pc2),] # Build a Test set
  pc2 <<- pc2[1:(9*NROW(pc2)/10-1),]
  
  # str(pc2) 
  
  # Change the Strings Y,N to 1,0
  
  if( debug){
    print("Converting N,Y to 0,1")
  }
  
  pc2$K  <<- as.numeric(factor(pc2$K , levels=c("N" ,"Y") ) ) 
  pc2.test$K  <<- as.numeric(pc2.test$K , levels=c("N" ,"Y") ) 
  
  
  if( debug){
    print("Discretising data")
  }
  
  #discretize the data for bayesian networks
  pc2.disc  <<- chiM(pc2, alpha = 0.05) 
  pc2.test.disc  <<- chiM(pc2.test,alpha=0.05)
  
  
  #load the data into a data frame
  pc2.disc.data  <<- data.frame(pc2.disc$Disc.data)
  pc2.test.data  <<- data.frame(pc2.test.disc$Disc.data)
 
  
  if( debug){
    print("Factorising Discretised data")
  }
  
  #Change all the discretized values into factors
  pc2.disc.data[,names(pc2)]  <<- lapply(pc2.disc.data[,names(pc2)] , factor) 
  pc2.test.data[,names(pc2.test)]  <<- lapply(pc2.test.data[,names(pc2.test)] , factor) 
  
  
  if( debug){
    print("Reomving Single factored variables")
  }
  
  
  #Bayesian Networks can't cope up with single factored variables , so we have to remove all the single factored variables
  excludevars  <<- NULL
  
  for ( i in 1:length(names(pc2.disc.data) ) ) {
    print(length( levels( pc2.disc.data[,i]) ) )
    print(length( levels( pc2.test.data[,i]) ) )
    
    if ( length( levels( pc2.disc.data[,i]) ) == 1 ) {
      excludevars  <<- c(excludevars,names(pc2.disc.data)[i])
    } 
    if ( length( levels( pc2.test.data[,i] ) ) == 1) {
      excludevars  <<- c(excludevars,names(pc2.test.data)[i])
    } 
  } #for
  
  
  
  excludevars  <<- unique(excludevars)
  
  if( debug){
    print("Single factored variables are:")
    print(excludevars)
  } 
  
  excludevars  <<-  names(pc2) %in% excludevars
  pc2.disc.data  <<- pc2.disc.data[ !excludevars  ]
  pc2.test.data  <<- pc2.test.data[ !excludevars  ]


} #preprocess