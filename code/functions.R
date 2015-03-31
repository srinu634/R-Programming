initialize = function( debug ) { 
  
  #Declare Necessary golbal variables
  
  dir.create(results.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  dir.create(plots.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  
  
  paths <<- c("\\BN", "\\TAN","\\HC","\\HC\\BAN","\\TABU","\\TABU\\BAN","\\GS",
              "\\GS\\BAN","\\MMHC","\\MMHC\\BAN","\\RSMAX2","\\RSMAX2\\BAN","\\MMPC","\\MMPC\\BAN")
  
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
  
  
  print(  paste ( "Reading the dataset" , dataset.name,sep=" ") )
  dataset <<- read.arff( paste ( dataset.path,dataset.name,".arff",sep="") ) #Load the data.dset
  
  print("")
  if(debug) { print(dataset)}
  
  Letters <<- c(letters,LETTERS)
  colnames(dataset) <<- c ( Letters[1:length(dataset)] )
  
  print("Discretising the data")
  dataset <<- mdlp( dataset)$Disc.data
}


drawPlot = function(path,g,gname ) {
    #path: path to store the graph . Relative path
    #g : graph structure
    #gname :  name of the graph
    
    
    specific.path <- paste( plots.path , path,sep="")
  
    
    
    Letters <<- c(letters,LETTERS)
    colnames(data.d) <<- Letters[1:length(data.d)]

  
  setwd(specific.path)
  png(paste(gname,".png",sep="") ,units="in", width=11, height=8.5, res=300)
  graphviz.plot(g)
  dev.off()
  
  
  setwd(plots.path)
  
}




 writeToExcel = function () {
  
  
  setwd( results.path )
  
  
  if( file.exists("Results.csv")  )
    file.remove("Results.csv") 
  
  
  #Write the results to An Excel FIle
  RESULTS <- rbind(auc.nb, auc.tan,auc.hc.k2,auc.hc.bde,auc.hc.aic,auc.tabu.k2, auc.tabu.bde,auc.tabu.aic,
                   
                   auc.gs,auc.mmhc,auc.rsmax2)
 
    write.csv(RESULTS,file = "Results.csv")  
}


  writeToExcelBAN = function() {
  
  
  setwd( results.path )
  
  
  if( file.exists("Results_BAN.csv")  )
    file.remove("Results_BAN.csv") 
  
  
  #Write the results to An Excel FIle
  RESULTS <- rbind(auc.nb, auc.tan,auc.hc.k2,auc.hc.bde,auc.hc.aic,auc.tabu.k2, auc.tabu.bde,auc.tabu.aic,
                   
                   auc.gs,auc.mmhc,auc.rsmax2)
  
  write.csv(RESULTS,file = "Results_BAN.csv")  
}