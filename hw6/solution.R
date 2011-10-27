# Let's benchmark a few ways to calculate
# \sum_{i=0}^n x^i

library(expm)

h.naive <- function(x, n){
    x.sum <- 0
    for (i in 0:n){
        x.sum <- x.sum + x^i
    }
    return (x.sum)
}

h.apply <- function(x, n){
    return (1+sum(sapply(1:n, function(i) x^i)))
}

h.matrix.power <- function(x, n){
    ltf.m <- matrix(c(x, 0, 1, 1), ncol=2)
    v <- c(1, 1)
    return (ltf.m %^% n %*% v)
}

h.matrix <- function(x, n){
    ltf.m <- matrix(c(x, 0, 1, 1), ncol=2) -> m
    v <- c(1, 1)
    for (i in 2:n){
        m <- ltf.m %*% m
    }
    return (m %*% v)
}

h.cumprod <- function(x, n) 1 + sum(cumprod(rep(x, n)))
