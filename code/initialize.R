initialize = function( debug ) { 
  
  #Declare Necessary golbal variables
  
  if(debug){
    print( paste("Creating ",dataset.name," Directory at path ",results.path,sep="") )
  }
  
  
  dir.create( results.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
 
  if(debug){
    print( paste( "Creating plots Directories at path:",plots.path,sep="") )
  }
  
  dir.create(plots.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
 
  
  paths <- c("\\BN", "\\TAN","\\HC","\\HC\\BAN","\\TABU","\\TABU\\BAN","\\GS",
              "\\GS\\BAN","\\MMHC","\\MMHC\\BAN","\\RSMAX2","\\RSMAX2\\BAN")
            #"\\MMPC","\\MMPC\\BAN"
  
  #Create Appropriate Directories
  for ( i in 1:length(paths)){
    newpath = paste(plots.path,paths[i],sep="")
    dir.create(newpath, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  #Initialize AUC Values 
  auc.nb <<- NULL
  auc.tan <<- NULL
  auc.hc.aic <<- NULL
  auc.hc.k2 <<- NULL 
  auc.hc.bde <<- NULL
  auc.tabu.aic <<- NULL 
  auc.tabu.k2 <<- NULL
  auc.tabu.bde <<- NULL
  auc.mmhc <<- NULL
  auc.rsmax2 <<- NULL
  auc.gs  <<- NULL

}