runGS = function(debug) {
  pc2.gs = empty.graph(names(pc2.disc.data))
  #whitelist.arcs = data.frame(from,to) #Arcs to be included in the graph
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc2.gs = cextend (  gs(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
 # see set.arc to set the arc directions
 #class(pc2.gs)
 # modelstring(pc2.gs)
 # arcs(pc2.gs)
 # pc2.gs$arcs ## To see the info about arcs
 # pc2.gs$nodes
  
  
  pc2.gs.fitted = bn.fit(pc2.gs,pc2.disc.data)
  
  pc2.gs.pred<- predict(pc2.gs.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
  print( table(pc2.gs.pred, pc2.test.data[, "K"])  ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.gs <- as.numeric(as.character(pc2.gs.pred))
  pc2.pred.gs <- as.numeric(as.character(pc2.test.data[,"K"]))
 print(  accuracy(f = pc2.given.gs , x = pc2.pred.gs) ) #print the accuracy
  
  
  
 
 png('./plots/grow_shrink.png',units="in", width=11, height=8.5, res=300)
 graphviz.plot(pc2.gs)
 dev.off()
  
  
  
  
  
}