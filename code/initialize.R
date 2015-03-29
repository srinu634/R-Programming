initialize = function( ) { 
  
  #Declare Necessary golbal variables
  
  common.path <<- paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\",dataset.name,"\\plots",sep="")
  
  dir.create(common.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
 
  
  paths <<- c("\\BN", "\\TAN","\\HC","\\HC\\BAN","\\TABU","\\TABU\\BAN","\\GS","\\GS\\BAN","\\MMHC","\\MMHC\\BAN","\\RSMAX2","\\RSMAX2\\BAN")
  
  #Create Appropriate Directories
  for ( i in 1:length(paths)){
    newpath = paste(common.path,paths[i],sep="")
    dir.create(newpath, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  #Accuracy for each bayesian network
  
  
  
  
}