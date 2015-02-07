
N = rep(0,10)
generation=rep(0,10)
N[1]=1
generation[1]=1
for (i in 2:10) {
N[i] = 2*N[i-1]
generation[i]=i
print (N[i])
}
plot(N~generation)