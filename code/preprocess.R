#Preprocess the Data 
# This includes:
# a) Discretizing the data using the method of Fayyad & Irani
# b) Also, for better graphical representation, the variables of the data have been changed to alphabets
# c) Building the Test set. 2/3rd , 1/3rd split.
# d) Removing the Single Factored Variables. ( As they don't add any significance to the network )

preprocess  = function(debug,i) {
 
  
  data.d <<- read.arff( paste("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\",dataset.name,
  ".arff",sep="")) #Load the data.dset
  
  if(debug) { print(data.d)}
  
  Letters <<- c(letters,LETTERS)
  colnames(data.d) <<- c ( Letters[1:length(data.d)] )
  
  if( debug) 
  {print("Discretising the data")}
  
  data.d <<- mdlp( data.d)$Disc.data
  
  if(debug) { print(data.d)}
  
  data.d <<- data.d[sample.int(nrow(data.d)),] #Shuffle the rows 
  
 # print( str(pc1))
  
  test <<- data.d[(2*NROW(data.d)/3):NROW(data.d),] # Build a Test set
  data.d <<- data.d[1:(2*NROW(data.d)/3-1),]
  
 
  
 # pc1$L  <<- as.numeric(factor(pc1$L , levels=c("N" ,"Y") ) ) 
  #test$L  <<- as.numeric(test$L , levels=c("N" ,"Y") ) 
  
  
  if( debug){
    print("Discretising data.d")
  }
  
  
  
  
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
  
  if( debug){
    print("Single factored variables are:")
    print(excludevars)
  } 
  
  excludevars  <<-  names(data.d) %in% excludevars
  disc.data  <<- disc.data[ !excludevars  ]
  test.data  <<- test.data[ !excludevars  ]


} #preprocess