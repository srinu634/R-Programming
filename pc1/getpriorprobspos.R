library(bnlearn)

getPrioProbsPos <- function(network, evidence_data) {
  pos <<- NULL
  b <<- nrow(evidence_data)
  for (i in 1:b) {
    
    temp <<- evidence_data[i,];
    temp <<-  temp[-length(evidence_data)]
    
    #print(temp)
    evi <<- paste( "(", names(evidence_data[- (length(evidence_data)  )]), "=='",
                 
                 sapply(temp, as.character), "')",
                 sep = "", collapse = "&" )
    
   # print( evi)
   eva <<- paste( "(" , names(evidence_data)[length(evidence_data)] , "=='" , 
                  sapply( evidence_data[i,][length(evidence_data)] , as.character) ,"')",sep="" )
   #print(eva)
   
   pos[i] <<- cpquery(network, eval(parse(text=eva)), eval(parse(text=evi))  ) #AS L is the classification node for this network
    print ( pos[i])
  }
  return(pos)
}

