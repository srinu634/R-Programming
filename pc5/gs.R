runGS = function(debug) {
  pc5.gs = empty.graph(names(pc5.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.gs = cextend (  gs(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
 # see set.arc to set the arc directions
 #class(pc5.gs)
 # modelstring(pc5.gs)
 # arcs(pc5.gs)
 # pc5.gs$arcs ## To see the info about arcs
 # pc5.gs$nodes
  
  
  pc5.gs.fitted = bn.fit(pc5.gs,pc5.disc.data)
  
  pc5.gs.pred<- predict(pc5.gs.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  print( table(pc5.gs.pred, pc5.test.data[, "M"])  ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.gs <- as.numeric(as.character(pc5.gs.pred))
  pc5.pred.gs <- as.numeric(as.character(pc5.test.data[,"M"]))
 print(  accuracy(f = pc5.given.gs , x = pc5.pred.gs) ) #print the accuracy
  
  
  
 
 png('./plots/grow_shrink.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(pc5.gs)
 dev.off()
  
  
  
  
  
}