library(sirt)
# (1) simulate data
set.seed(789)
N <- 200
probs <- c(.5 , .3 , .2 )
alpha0 <- .5
alpha <- alpha0*probs
alpha <- matrix( alpha , nrow=N , ncol=length(alpha) , byrow=TRUE  )
alpha
x <- dirichlet.simul(alpha)
x

# (2) estimate Dirichlet parameters
dirichlet.mle(x)
##   $alpha
##   [1] 0.24507708 0.14470944 0.09590745
##   $alpha0
##   [1] 0.485694
##   $xsi
##   [1] 0.5045916 0.2979437 0.1974648