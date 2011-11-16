gcd <- function(m, n){
    r <- m %% n
    while (r > 0){
        m <- r
        r <- n %% r
        n <- m
    }
    return (n)
}

### problem 2

order.matrix <- function(x){
    x.order <- order(as.vector(x))
    return (cbind(x.order %% nrow(x) + 1, x.order %/% nrow(x) + 1))
}

### problem 3

polaroid <- function(x){
    R <- sqrt(sum(x^2))
    theta <- rep(0, length(x) - 1)
    theta[1] <- acos(x[1]/R)
    c <- R*sin(theta[1])
    for (i in 2:(length(x)-1)){
        theta[i] <- acos(x[i] / c)    
        c <- c * sin(theta[i])
    }
    if (x[length(x)] < 0){
        theta[1] <- 2*pi - theta[1]
        theta[-1] <- pi - theta[-1]
    }
    return (c(R, theta))
}

normalize <- function(x){
    sqrt.ss <- sqrt(apply(x^2, 1, sum))
    return (sweep(x, 1, sqrt.ss, '/'))
}

y <- matrix(rnorm(5000), ncol=5)
z <- normalize(y)
p.val <- apply(z, 2, function(x) ks.test(x, 'punif', min=-1, max=1)$p.val)

# Visualize inspect theta_1 is Chi-squared distributed.
w <- t(apply(y, 1, polaroid))
h <- hist(w[, 1]^2, plot=F)
f.chisq <- function(x) dchisq(x, df=5)
xy <- curve(f.chisq, 0, max(h$breaks))

layout(matrix(c(1, 2, 3, 4), byrow=T, ncol=2))
hist(w[,1]^2, freq=F, ylim=c(0, max(xy$y, h$density)))
lines(xy)
for (i in 2:4){
    hist(w[, i])
}

# Test for if uniform distributed
ks.test(w[, 2], 'punif', min=0, max=2*pi)
ks.test(w[, 3], 'punif', min=0, max=pi)
ks.test(w[, 4], 'punif', min=0, max=pi)

### Problem 4

# Quadratic New-Raphson method
newton.quad <- function(f, f.fd, f.sd, x0, eps, max.iter=100){
    # f.fd := first derivative of function f
    # f.sd := second derivative of function f

    # Initialization
    iter <- 1
    x1 <- NA

    while ((is.na(x1) || abs(x1-x0) > eps) && iter < max.iter){
        # Quadratic coefficients
        a <- f.sd(x0)
        b <- 2*f.fd(x0) - 2*f.sd(x0)*x0 - 2
        c <- 2*f(x0) - 2*x0*f.fd(x0)+f.sd(x0)*x0^2

        # Discriminant of quadratic formula
        delta <- b^2 - 4*a*c

        if (delta < 0){
            # No real solution, find the point where the derivative of function
            # h(x) = f(x) - x equals 0.
            x1 <- (f.sd(x0) * x0 + 1 - f.fd(x0)) / f.sd(x0)
        } else {
            # Candidates of two x1
            x1.cand <- (-b + c(1, -1) * sqrt(delta))/2/a
            # Choose a x1 that is closer to x0
            x1 <- x1.cand[which.min(abs(x1.cand-x0))]
        }
        cat("[ITER]", iter, "/", x1, fill=T)

        # Update x0
        x0 <- x1
    }

    if (abs(x1-x0) > eps){
        return (NULL)
    } else {
        return (x1)
    }
}

# 4(a)
fa <- function(x) cos(x) - x
fa.fd <- function(x) -sin(x) - 1
fa.sd <- function(x) -cos(x)
newton.quad(fa, fa.fd, fa.sd, 1, 1e-6, 100)

### Problem 5

### Problem 6
log.ll <- function(x, theta) {-length(x) * log(2*pi) + sum(log(1-cos(x-theta)))}
x <- c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52, 2.50)
theta <- seq(-pi, pi, length.out=500)
plot(theta, sapply(theta, log.ll, x=x), typ='l') 

log.ll2 <- function(x){
    # X is theta here
    x.i <- c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52, 2.50)
    return (-length(x.i) * log(2*pi) + sum(log(1-cos(x.i-x))))
}
optimize(log.ll2, c(-pi, pi), tol=1e-8, maximum=T)
newton <- function(f, f.prime, x0, eps, max.iter){
    x1 <- x0 - f(x0)/f.prime(x0)
    iter <- 1
    while(abs(x1 - x0) > eps & iter < max.iter){
        x0 <- x1
        x1 <- x0 - f(x0)/f.prime(x0)
        cat("[ITER]", iter, "/", x1, fill=T)
        iter <- iter + 1
    }

    return (x1)
}

# First derivative of log-likelihood function
dl <- function(theta){
    x.i <- c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52, 2.50)
    return (-sum(sin(x.i-theta)/(1-cos(x.i-theta))))
}

# Second derivative
d2l <- function(theta){
    x.i <- c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52, 2.50)
    return (-sum(1/(1-cos(x.i-theta))))
}

newton(dl, d2l, 0, 1e-6, 100)
newton(dl, d2l, -1.5, 1e-6, 100)
newton(dl, d2l, -2.0, 1e-6, 100)
newton(dl, d2l, -2.7, 1e-6, 100)
