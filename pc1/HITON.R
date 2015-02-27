runHITON = function(debug) {
  pc1.si.hiton.pc = empty.graph(attributes)
 
  #str(whitelist.arcs)
  #names(whitelist.arcs)
  pc1.si.hiton.pc = cextend (  si.hiton.pc(pc1.disc.data,whitelist = NULL,debug=FALSE) ) # cextend :: makes sure that all edges are directed
  
  pc1.si.hiton.pc.fitted = bn.fit(pc1.si.hiton.pc,pc1.disc.data)
  
  pc1.si.hiton.pc.pred<- predict(pc1.si.hiton.pc.fitted$Defective, pc1.disc.data) #2nd parameter should be pc1.test.data
  
  print( table(pc1.si.hiton.pc.pred, pc1.disc.data[, "Defective"]) ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc1.given.si.hiton.pc <- as.numeric(as.character(pc1.si.hiton.pc.pred))
  pc1.pred.si.hiton.pc <- as.numeric(as.character(pc1.disc.data[,"Defective"]))
  print( accuracy(f = pc1.given.si.hiton.pc , x = pc1.pred.si.hiton.pc) )  #print the accuracy
}