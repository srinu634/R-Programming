

drawPlot = function(path,g,gname ) {
    #path: path to store the graph . Relative plot
    #g : graph structure
    #gname :  name of the graph
  
    common.path <<- paste("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming","\\",dataset.name,
                        "\\plots\\",sep="")
    
    specific.path <<- paste( common.path , path,sep="")
  
    
    
    Letters <<- c(letters,LETTERS)
    colnames(data.d) <<- Letters[1:length(data.d)]

  
  setwd(specific.path)
  png(paste(gname,".png",sep="") ,units="in", width=11, height=8.5, res=300)
  graphviz.plot(g)
  dev.off()
  
  
  setwd(common.path)
  
  
}