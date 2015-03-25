startProcessBAN = function(debug,i) { 
  
  setwd("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\pc5")
  preprocess(debug,i)
  
  len <<- length(pc5.disc.data) #Number of attributes
  from <<- NULL
  for( j in 1:(len-1)){
    from <- c( from,c(   names(pc5)[length(pc5)] ) ) 
  }
  #from
  to <<- NULL
  
  for(j in 1:(len-1) ){
    to <<- c(to, attribute[j]  )
  }
  #to
  
  whitelist.arcs <<- data.frame(from,to)
  setwd("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\pc5\\plots")
  
  #Build a BAN by including arc from Classification Node to every other node
  
  
  runHC(debug,i) 
  runTABU(debug,i)
  
  
  #########Hybrid
  
  runMMHC(debug,i)
  runRSMAX2(debug,i)
  #runHITON(debug,i)
  
  
  
  #Constraint Based
  
  runGS(debug,i)
  #runIAMB(debug,i)
  #runMMPC(debug,i)
  # runFASTIAMB(debug,i)
  # runINTERIAMB(debug,i)
  
}
