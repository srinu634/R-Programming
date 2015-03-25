preprocess  = function(debug,i) {
 
  
  pc5 <<- read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc5.arff") #Load the dataset
  
  Letters <<- c(letters,LETTERS)
  colnames(pc5) <<- c ( Letters[1:length(pc5)] )
  
  pc5.temp <<- pc5[sample.int(nrow(pc5)),] #Shuffle the rows 
  
  
 # pc5.temp <<- pc5.temp[ , c( length(pc5.temp) , (1 : (length(pc5.temp) -1 )) ) ]
 # pc5<<- pc5[ , c( length(pc5) , (1 : (length(pc5) -1 ) ) ]

  
  pc5 <<- pc5.temp
  
 # pc5 <<- sample(pc5) #Create a random permutation of the data
 
  pc5.temp <<- mdlp(pc5)
 
  pc5 <<- data.frame( pc5.temp$Disc.data )
  
 # print(pc5)
 

 # print( str(pc5))
  
  pc5.test <<- pc5[(2*NROW(pc5)/3):NROW(pc5),] # Build a Test set
  pc5 <<- pc5[1:(2*NROW(pc5)/3-1),]
  
 
  
  pc5$M  <<- as.numeric(factor(pc5$M , levels=c("FALSE" ,"TRUE") ) ) 
  pc5.test$M  <<- as.numeric(pc5.test$M , levels=c("FALSE" ,"TRUE") ) 
  
  
  if( debug){
    print("Discretising data")
  }
  
  
  
  
  #load the data into a data frame
  pc5.disc.data  <<- data.frame(pc5)
  pc5.test.data  <<- data.frame(pc5.test)
 
  
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
    #print(length( levels( pc5.disc.data[,i]) ) )
   # print(length( levels( pc5.test.data[,i]) ) )
    
    if ( length( levels( pc5.disc.data[,i]) ) == 1 ) {
      excludevars  <<- c(excludevars,names(pc5.disc.data)[i])
    } 
    if ( length( levels( pc5.test.data[,i] ) ) == 1) {
      excludevars  <<- c(excludevars,names(pc5.test.data)[i])
    } 
    
   
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