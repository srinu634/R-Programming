library(gRain)


net = mmhc(disc.data)
net = cextend(net)

net = as.graphNEL ( net )


#net = cpdag(net)


#netfit = bn.fit(net,disc.data)

#net = as.grain( netfit )

net.smoothed <- compile( grain( net, disc.data, smooth=  .3 ) )


pred = predict(net.smoothed , response = c( names(disc.data)[length(disc.data)] ), newdata = test.data,
               predictors = names(test.data)[-length(test.data)], type = "distribution")

graphviz.plot(net)


##############
dG <- dag(~A:B)

dat <-as.table( parray(c("A","B"), levels=c(2,2), values=c(0,0,2,3)))

gr.dG <- compile( grain( dG, dat ,smooth = .2)  )

