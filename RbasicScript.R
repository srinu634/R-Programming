plotgraph = function(x_params , y_params) { 
	plot(x= x_params , y=y_params)
}

#########################################################

sample = c(1,2,3,4)
sample1 = c(5,6,7,8)

mean(sample)
mean(sample1)


#########################################################


arrx  = rep(0,10)
arry = rep(0,10)

for ( i in 1:10 ) {
	arry[i] = i*i*i*i
	arrx[i] = i
	print(arrx[i])
	print(arry[i])
}

plot(x=arrx,y=arry)

plotgraph(arrx,arry) 

#########################################################

n = c(1,2,3,4)
n = c(n,5)
n

boolvalues = c(T,F,T,T,F)
n[boolvalues]

n[10]   #Out of range

#########################################################

names(n) = c("ONE","TWO","THREE","FOUR","FIVE")
n

name = c("ONE","TWO","THREE","FOUR","FIVE")

n[name]

#########################################################
women
#MATRIX