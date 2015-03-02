runHITON = function(debug) {
  pc2.si.hiton.pc = empty.graph(names(pc2.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc2.si.hiton.pc =   cextend ( si.hiton.pc(pc2.disc.data,whitelist = whitelist.arcs,debug=FALSE))  # cextend :: makes sure that all edges are directed
  
  pc2.si.hiton.pc.fitted = bn.fit(pc2.si.hiton.pc,pc2.disc.data)
  
  pc2.si.hiton.pc.pred<- predict(pc2.si.hiton.pc.fitted$K, pc2.test.data) #2nd parameter should be pc2.test.data
  
  print( table(pc2.si.hiton.pc.pred, pc2.test.data[, "K"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc2.given.si.hiton.pc <- as.numeric(as.character(pc2.si.hiton.pc.pred))
  pc2.pred.si.hiton.pc <- as.numeric(as.character(pc2.test.data[,"K"]))
  print( accuracy(f = pc2.given.si.hiton.pc , x = pc2.pred.si.hiton.pc) )  #print the accuracy
  
  png('./plots/hiton.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc2.si.hiton.pc)
  dev.off()

}