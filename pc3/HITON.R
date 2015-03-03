runHITON = function(debug) {
  pc3.si.hiton.pc = empty.graph(names(pc3.disc.data))
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc3.si.hiton.pc = cextend (  si.hiton.pc(pc3.disc.data,whitelist = whitelist.arcs,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc3.si.hiton.pc.fitted = bn.fit(pc3.si.hiton.pc,pc3.disc.data)
  
  pc3.si.hiton.pc.pred<- predict(pc3.si.hiton.pc.fitted$L, pc3.test.data) #2nd parameter should be pc3.test.data
  
  print( table(pc3.si.hiton.pc.pred, pc3.test.data[, "L"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc3.given.si.hiton.pc <- as.numeric(as.character(pc3.si.hiton.pc.pred))
  pc3.pred.si.hiton.pc <- as.numeric(as.character(pc3.test.data[,"L"]))
  print( accuracy(f = pc3.given.si.hiton.pc , x = pc3.pred.si.hiton.pc) )  #print the accuracy
  
  png('./plots/hiton.png',units="in", width=11, height=8.5, res=300)
  graphviz.plot(pc3.si.hiton.pc)
  dev.off()

}