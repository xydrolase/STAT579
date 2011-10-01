sd.short <- function(smp){
    n <- length(smp)
    smp.mean <- sum(smp) / n
    smp.sq.mean <- sum(smp^2) / n 

    biased.sd <- smp.sq.mean - smp.mean^2
    return (sqrt(biased.sd * n / (n-1)))
}

sd.long <- function(smp){
    n <- length(smp)
    smp.mean <- sum(smp) / n 
    smp.ss <- (smp - smp.mean)^2

    smp.dev <- sqrt(sum(smp.ss)/(n-1))
    return (smp.dev)
}

num <- rep(c(1001, 1002), times=5) 
print (c(sd.short(num), sd.long(num), sd(num)))

num.1 <- rep(c(100000000000001, 100000000000002), times=5)
print (c(sd.short(num.1), sd.long(num.1), sd(num.1)))
