runGS = function(debug) {
  pc3.gs = empty.graph(names(pc3.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc3.gs = cextend (  gs(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
 # see set.arc to set the arc directions
 #class(pc3.gs)
 # modelstring(pc3.gs)
 # arcs(pc3.gs)
 # pc3.gs$arcs ## To see the info about arcs
 # pc3.gs$nodes
  
  
  pc3.gs.fitted = bn.fit(pc3.gs,pc3.disc.data)
  
  pc3.gs.pred<- predict(pc3.gs.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
  print( table(pc3.gs.pred, pc3.test.data[, "L"])  ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.gs <- as.numeric(as.character(pc3.gs.pred))
  pc3.pred.gs <- as.numeric(as.character(pc3.test.data[,"L"]))
 print(  accuracy(f = pc3.given.gs , x = pc3.pred.gs) ) #print the accuracy
  
  
  
 
 png('./plots/grow_shrink.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(pc3.gs)
 dev.off()
  
  
  
  
  
}