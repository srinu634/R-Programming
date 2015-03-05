runGS = function(debug,i) {
  pc1.gs = empty.graph(names(pc1.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.gs = cextend (  gs(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
 # see set.arc to set the arc directions
 #class(pc1.gs)
 # modelstring(pc1.gs)
 # arcs(pc1.gs)
 # pc1.gs$arcs ## To see the info about arcs
 # pc1.gs$nodes
  
  
  pc1.gs.fitted = bn.fit(pc1.gs,pc1.disc.data)
  
  pc1.gs.pred<- predict(pc1.gs.fitted$L, pc1.test.data) #2nd parameter should be pc1.test.data
  
  print( table(pc1.gs.pred, pc1.test.data[, "L"])  ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.gs <- as.numeric(as.character(pc1.gs.pred))
  pc1.pred.gs <- as.numeric(as.character(pc1.test.data[,"L"]))
 print(  accuracy(f = pc1.given.gs , x = pc1.pred.gs) ) #print the accuracy
  
  
 if(  identical(whitelist.arcs,NULL) ) 
   temp.path = "\\GS"
 else
   temp.path = "\\GS\\BAN" 
 
 
 drawPlot(temp.path,pc1.gs,paste("gs",i,sep="")) ;
    
  
  
  
  
  
}