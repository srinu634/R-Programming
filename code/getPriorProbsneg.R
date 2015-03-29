library(bnlearn)

getPriorProbsNeg <- function(network, evidence_data) {
  
 if(debug){ print("IN negative prior probabilities") }
  neg <<- NULL
  b <<- nrow(evidence_data)
  for (i in 1:b) {
    
    temp <<- evidence_data[i,];
    temp <<-  temp[-length(evidence_data)]
    
    #print(temp)
    evi <<- paste("(", names(evidence_data[- (length(evidence_data) -1 )]), "=='",
                  
                  sapply(temp, as.character), "')",
                  sep = "", collapse = " & ")
    
    # print( evi)
    eva <<- paste( "(" , names(evidence_data)[length(evidence_data)] , "=='" , 
                   "N'" ,")",sep="" )
    #print(eva)
    
    
    
    neg[i] <<- cpquery(network, eval(parse(text=eva)), eval(parse(text=evi))   ) #AS L is the classification node for this network
    #print ( neg[i])
  }

 if(debug){ print("Done with:negative prior probabilities") }
 return(neg)
}
