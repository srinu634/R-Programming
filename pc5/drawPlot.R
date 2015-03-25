

drawPlot = function(path,g,gname ) {
    #path: path to store the graph . Relative plot
    #g : graph structure
    #gname :  name of the graph
  
    common.path = "C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\pc5\\plots\\"
    
    specific.path <<- paste( common.path , path,sep="")
  
    
    
    Letters <<- c(letters,LETTERS)
    colnames(pc5) <<- Letters[1:length(pc5)]

  
  setwd(specific.path)
  png(paste(gname,".png",sep="") ,units="in", width=11, height=8.5, res=300)
  graphviz.plot(g)
  dev.off()
  
  setwd("C:\\Users\\redhawk\\Documents\\GitHub\\R-Programming\\pc5\\plots")
  
  
}