runHITON = function(debug) {
  pc5.si.hiton.pc = empty.graph(names(pc5.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc5.si.hiton.pc = cextend (  si.hiton.pc(pc5.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc5.si.hiton.pc.fitted = bn.fit(pc5.si.hiton.pc,pc5.disc.data)
  
  pc5.si.hiton.pc.pred<- predict(pc5.si.hiton.pc.fitted$M, pc5.test.data) #2nd parameter should be pc5.test.data
  
  print( table(pc5.si.hiton.pc.pred, pc5.test.data[, "M"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.si.hiton.pc <- as.numeric(as.character(pc5.si.hiton.pc.pred))
  pc5.pred.si.hiton.pc <- as.numeric(as.character(pc5.test.data[,"M"]))
  print( accuracy(f = pc5.given.si.hiton.pc , x = pc5.pred.si.hiton.pc) )  #print the accuracy
  
  png('./plots/hiton.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc5.si.hiton.pc)
  dev.off()

}