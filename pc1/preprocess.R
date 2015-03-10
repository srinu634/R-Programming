preprocess  = function(debug,i) {
 
  
  pc1 <<- read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc1.arff") #Load the dataset
  
  Letters <<- c(letters,LETTERS)
  colnames(pc1) <<- c ( Letters[1:length(pc1)] )
  
  pc1.temp <<- pc1[sample.int(nrow(pc1)),] #Shuffle the rows 
  
  
 # pc1.temp <<- pc1.temp[ , c( length(pc1.temp) , (1 : (length(pc1.temp) -1 )) ) ]
 # pc1<<- pc1[ , c( length(pc1) , (1 : (length(pc1) -1 ) ) ]

  
  pc1 <<- pc1.temp
  
 # pc1 <<- sample(pc1) #Create a random permutation of the data
 
  pc1.temp <<- mdlp(pc1)
 
  pc1 <<- data.frame( pc1.temp$Disc.data )
  
 # print(pc1)
 

 # print( str(pc1))
  
  pc1.test <<- pc1[(2*NROW(pc1)/3):NROW(pc1),] # Build a Test set
  pc1 <<- pc1[1:(2*NROW(pc1)/3-1),]
  
 
  
  pc1$L  <<- as.numeric(factor(pc1$L , levels=c("N" ,"Y") ) ) 
  pc1.test$L  <<- as.numeric(pc1.test$L , levels=c("N" ,"Y") ) 
  
  
  if( debug){
    print("Discretising data")
  }
  
  
  
  
  #load the data into a data frame
  pc1.disc.data  <<- data.frame(pc1)
  pc1.test.data  <<- data.frame(pc1.test)
 
  
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
    #print(length( levels( pc1.disc.data[,i]) ) )
   # print(length( levels( pc1.test.data[,i]) ) )
    
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