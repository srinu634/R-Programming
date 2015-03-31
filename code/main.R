  library(infotheo)
  library(foreign)
  library(bnlearn)
  library(discretization)
  library(pROC)
  library(forecast)
  library(base)
  library(gRain)
  library(caTools)
  library(pcalg)

  code.path <<- "C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\code"  # Path where the code exists.
  setwd( code.path ) #SEt the path to the directory where the code is.
  
  source("start.R")
  source("preprocess.R")
  source("functions.R")
  
 #############################
  runForDataset  = function ()  {
    print( paste(" Running Classifiers for dataset: ",dataset.name,sep="") )
    dataset.path  <<- "C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\" #Directoty where Dataset Exists   
    results.path <<- paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\" ,dataset.name,sep="") #Place where you want to save your results
    plots.path <<- paste(results.path,"\\plots",sep="") #Saves the plots in the results path
    common.path <<- "C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\"
    debug = TRUE

    initialize(debug) ;
    
    startProcess(debug) #2/3rd , 1/3rd split repeated for 10 times
    
    print("BUilding BAN Networks")    
    startProcessBAN(debug) #2/3rd , 1/3rd split repeated for 10 times

    setwd( common.path )      
  }


dataset.name <<-  paste("jm1") ; runForDataset( )
dataset.name <<-  paste("mc1") ; runForDataset( )
dataset.name <<-  paste("mc2") ; runForDataset(  )
dataset.name <<-  paste("pc1") ; runForDataset(  )
dataset.name <<-  paste("pc2") ; runForDataset(  )
dataset.name <<-  paste("pc3") ; runForDataset(  )
dataset.name <<-  paste("pc4") ; runForDataset( )
dataset.name <<-  paste("pc5") ;runForDataset(  )
  
  
  