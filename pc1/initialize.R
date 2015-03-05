initialize = function( ) { 
  
  #Declare Necessary golbal variables
  
  dataset.name <<- "pc1"
  common.path <<- paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\",dataset.name,"\\plots",sep="") 
 
  
  dir.create(common.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  
  #Create Appropriate Directories
  
  paths <<- c("\\BN", "\\TAN","\\HC","\\HC\\BAN","\\TABU","\\TABU\\BAN","\\GS","\\GS\\BAN","\\MMHC","\\MMHC\\BAN","\\RSMAX2","\\RSMAX2\\BAN")
  
  for ( i in 1:length(paths)){
    newpath = paste(common.path,paths[i],sep="")
    dir.create(newpath, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
 
  
  
  
  
  
  

}