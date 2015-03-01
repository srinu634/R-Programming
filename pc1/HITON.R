runHITON = function(debug) {
  pc1.si.hiton.pc = empty.graph(names(pc1.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.si.hiton.pc = cextend (  si.hiton.pc(pc1.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.si.hiton.pc.fitted = bn.fit(pc1.si.hiton.pc,pc1.disc.data)
  
  pc1.si.hiton.pc.pred<- predict(pc1.si.hiton.pc.fitted$L, pc1.disc.data) #2nd parameter should be pc1.test.data
  
  print( table(pc1.si.hiton.pc.pred, pc1.disc.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.si.hiton.pc <- as.numeric(as.character(pc1.si.hiton.pc.pred))
  pc1.pred.si.hiton.pc <- as.numeric(as.character(pc1.disc.data[,"L"]))
  print( accuracy(f = pc1.given.si.hiton.pc , x = pc1.pred.si.hiton.pc) )  #print the accuracy
  
  png('./plots/hiton.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc1.si.hiton.pc)
  dev.off()

}