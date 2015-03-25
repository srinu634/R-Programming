runGS = function(debug,i) {
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
  
  print( table(pc5.gs.pred, pc5.test.data[,length(pc5.test.data)] )  ) #output the prediction matrix
  #Change the outputs to numeric values; 
  pc5.given.gs <- as.numeric(as.character(pc5.gs.pred))
  pc5.pred.gs <- as.numeric(as.character(    pc5.test.data[,length(pc5.test.data)]    ))
 print(  accuracy(f = pc5.given.gs , x = pc5.pred.gs) ) #print the accuracy
  
  
 if(  identical(whitelist.arcs,NULL) ) 
   temp.path = "\\GS"
 else
   temp.path = "\\GS\\BAN" 
 
 pc5.gs.auc <- sapply( pc5.test.data, as.numeric )
 #print (  colAUC(  pc5.gs.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.gs) , plotROC=TRUE ) )
 
 
 # 
 pc5.gs.auc <- sapply( pc5.test.data, as.numeric )
# print (  colAUC(  pc5.gs.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.gs) , plotROC=TRUE ) )
 
 pc5.gs.auc <- sapply( pc5.test.data, as.numeric )
# print (  colAUC( pc5.gs.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.gs) , plotROC=TRUE ) )
 
 
 if( i==1  )
   auc.gs <<- colAUC(  pc5.gs.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.gs) , plotROC=TRUE )
 else{
   
   auc.gs <<- rbind( auc.gs,colAUC(  pc5.gs.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.gs) , plotROC=TRUE ) )
 }
 
 print (  colAUC(  pc5.gs.auc[,- length(pc5.test.data)] , as.numeric( pc5.pred.gs) , plotROC=TRUE ) )
 #
 
 drawPlot(temp.path,pc5.gs,paste("gs",i,sep="")) ;
    
  
  
  
  
  
}