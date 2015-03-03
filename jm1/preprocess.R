preprocess  = function(debug) {
  
  jm1 <<- read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\jm1.arff") #Load the dataset
  
 # jm1 <<- sample(jm1) #Create a random permutation of the data
  
 # print(jm1)
  Letters <<- c(letters,LETTERS)
  colnames(jm1) <<- Letters[1:22]

 # print( str(jm1))
 
  jm1.temp <<- chiM(jm1, alpha = 0.05) 
 
  jm1 <<- jm1.temp$Disc.data
 
  jm1$v  <<- as.numeric(factor(jm1$v , levels=c("N" ,"Y") ) ) 
  jm1.test <<- jm1[(3*NROW(jm1)/4):NROW(jm1),] # Build a Test set
  jm1.test$v  <<- as.numeric(jm1.test$v , levels=c("N" ,"Y") )  
  jm1 <<- jm1[1:(3*NROW(jm1)/4-1),]
  
  # str(jm1) 
  
  # Change the Strings Y,N to 1,0
  
  if( debug){
    print("Converting N,Y to 0,1")
  }
  
 
  
  
  
  if( debug){
    print("Discretising data")
  }
  
  
  
  
  #load the data into a data frame
  jm1.disc.data  <<- data.frame(jm1)
  jm1.test.data  <<- data.frame(jm1.test)
 
  
  if( debug){
    print("Factorising Discretised data")
  }
  
  #Change all the discretized values into factors
  jm1.disc.data[,names(jm1)]  <<- lapply(jm1.disc.data[,names(jm1)] , factor) 
  jm1.test.data[,names(jm1.test)]  <<- lapply(jm1.test.data[,names(jm1.test)] , factor) 
  
  
  if( debug){
    print("Reomving Single factored variables")
  }
  
  
  #Bayesian Networks can't cope up with single factored variables , so we have to remove all the single factored variables
  excludevars  <<- NULL
  
  for ( i in 1:length(names(jm1.disc.data) ) ) {
   # print(length( levels( jm1.disc.data[,i]) ) )
   # print(length( levels( jm1.test.data[,i]) ) )
    
    if ( length( levels( jm1.disc.data[,i]) ) == 1 ) {
      excludevars  <<- c(excludevars,names(jm1.disc.data)[i])
    } 
    if ( length( levels( jm1.test.data[,i] ) ) == 1) {
      excludevars  <<- c(excludevars,names(jm1.test.data)[i])
    } 
  } #for
  
  
  
  excludevars  <<- unique(excludevars)
  
  if( debug){
    print("Single factored variables are:")
    print(excludevars)
  } 
  
  excludevars  <<-  names(jm1) %in% excludevars
  jm1.disc.data  <<- jm1.disc.data[ !excludevars  ]
  jm1.test.data  <<- jm1.test.data[ !excludevars  ]


} #preprocess