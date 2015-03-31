#Preprocess the Data 
# This includes:
# a) Discretizing the data using the method of Fayyad & Irani
# b) Also, for better graphical representation, the variables of the data have been changed to alphabets
# c) Building the Test set. 2/3rd , 1/3rd split.
# d) Removing the Single Factored Variables. ( As they don't add any significance to the network )

preprocess  = function(debug,i) { 
  
  if(debug) { print(dataset)}
  
  print("Shuffling the dataset :Row-wise")
  dataset <<- dataset[sample.int(nrow(dataset)),] #Shuffle the rows 
  
 # print( str(pc1))
  
  test <<- dataset[(2*NROW(dataset)/3):NROW(dataset),] # Build a Test set
  data.d <<- dataset[1:(2*NROW(dataset)/3-1),]

 
  #load the data into a dataframe
  disc.data  <<- data.frame(data.d)
  test.data  <<- data.frame(test)
 
  
  if( debug){
    print("Factorising Discretised data")
  }
  
  #Change all the discretized values into factors
  disc.data[,names(data.d)]  <<- lapply(disc.data[,names(data.d)] , factor) 
  test.data[,names(test)]  <<- lapply(test.data[,names(test)] , factor) 
  
  
  if( debug){
    print("Reomving Single factored variables")
  }
  
  
  #Bayesian Networks can't cope up with single factored variables , so we have to remove all the single factored variables
  excludevars  <<- NULL
  
  for ( i in 1:length(names(disc.data) ) ) {
    #print(length( levels( disc.data[,i]) ) )
   # print(length( levels( test.data[,i]) ) )
    
    if ( length( levels( disc.data[,i]) ) == 1 ) {
      excludevars  <<- c(excludevars,names(disc.data)[i])
    } 
    if ( length( levels( test.data[,i] ) ) == 1) {
      excludevars  <<- c(excludevars,names(test.data)[i])
    } 
  
  } #for
  
  
  
  excludevars  <<- unique(excludevars)
  
 
    print("Removing Single factored variables: ")
    print(excludevars)
  
  
  excludevars  <<-  names(data.d) %in% excludevars
  disc.data  <<- disc.data[ !excludevars  ]
  test.data  <<- test.data[ !excludevars  ]


} #preprocess