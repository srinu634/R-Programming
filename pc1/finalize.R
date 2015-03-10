#
auc.bn.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.bn)[i]  )
  #print ( mean( auc.bn[, i ] ) )

  auc.bn.avg <- c( auc.bn.avg, mean(auc.bn[,i])) 
}

auc.bn.avg

#
auc.tan.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.tan)[i]  )
  #print ( mean( auc.tan[, i ] ) )
  
  auc.tan.avg <- c( auc.tan.avg, mean(auc.tan[,i])) 
}

auc.tan.avg

#
auc.hc.aic.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.hc.aic)[i]  )
  #print ( mean( auc.hc.aic[, i ] ) )
  
  auc.hc.aic.avg <- c( auc.hc.aic.avg, mean(auc.hc.aic[,i])) 
}
auc.hc.aic.avg


#
auc.hc.k2.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.hc.k2)[i]  )
  #print ( mean( auc.hc.k2[, i ] ) )
  
  auc.hc.k2.avg <- c( auc.hc.k2.avg, mean(auc.hc.k2[,i])) 
}

auc.hc.k2.avg



#
auc.hc.bde.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.hc.bde)[i]  )
  #print ( mean( auc.hc.bde[, i ] ) )
  
  auc.hc.bde.avg <- c( auc.hc.bde.avg, mean(auc.hc.bde[,i])) 
}

auc.hc.bde.avg

#
auc.tabu.k2.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.tabu.k2)[i]  )
  #print ( mean( auc.tabu.k2[, i ] ) )
  
  auc.tabu.k2.avg <- c( auc.tabu.k2.avg, mean(auc.tabu.k2[,i])) 
}
auc.tabu.k2.avg




#
auc.tabu.bde.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.tabu.bde)[i]  )
  #print ( mean( auc.tabu.bde[, i ] ) )
  
  auc.tabu.bde.avg <- c( auc.tabu.bde.avg, mean(auc.tabu.bde[,i])) 
}
auc.tabu.bde.avg






#
auc.tabu.aic.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.tabu.aic)[i]  )
  #print ( mean( auc.tabu.aic[, i ] ) )
  
  auc.tabu.aic.avg <- c( auc.tabu.aic.avg, mean(auc.tabu.aic[,i])) 
}
auc.tabu.aic.avg






#

auc.gs.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.tan)[i]  )
  #print ( mean( auc.gs[, i ] ) )
  
  auc.gs.avg <- c( auc.gs.avg, mean(auc.gs[,i])) 
}

auc.gs.avg





#
auc.mmhc.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.tan)[i]  )
  #print ( mean( auc.mmhc[, i ] ) )
  
  auc.mmhc.avg <- c( auc.mmhc.avg, mean(auc.mmhc[,i])) 
}

auc.mmhc.avg








#

auc.rsmax2.avg <- NULL

for( i in 1: ( length(pc1.disc.data) -1 ) ) {
  #print(  str(auc.tan)[i]  )
  #print ( mean( auc.rsmax2[, i ] ) )
  
  auc.rsmax2.avg <- c( auc.rsmax2.avg, mean(auc.rsmax2[,i])) 
}

auc.rsmax2.avg




