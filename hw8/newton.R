newton.quad <- function(f, f.fd, f.sd, x0, eps, max.iter=100,
                        verbose=F){
    # f.fd := first derivative of function f
    # f.sd := second derivative of function f

    # Initialization
    iter <- 1
    x1 <- NA

    while ((is.na(x1) || abs(x1-x0) > eps) && iter < max.iter){
        # Quadratic coefficients
        if (!is.na(x1)) x0 <- x1 # update x0

        a <- 0.5*f.sd(x0)
        b <- f.fd(x0) - f.sd(x0)*x0
        c <- f(x0) - x0*f.fd(x0) + 0.5*f.sd(x0)*x0^2

        if (abs(a) > 0){
            # Discriminant of quadratic formula
            delta <- b^2 - 4*a*c

            if (delta < 0){
                # No real solution, find the point where the derivative of function
                # g(x) equals 0.
                x1 <- x0 - f.fd(x0)/f.sd(x0) 
            } else {
                # Candidates of two x1
                x1.cand <- (-b + c(1, -1) * sqrt(delta))/2/a
                # Choose a x1 that is closer to x0
                x1 <- x1.cand[which.min(abs(x1.cand-x0))]
            }
        } else {
            # Not quadratic equation
            x1 <- -c/b
        }

        if (verbose){
            cat("[ITER]", iter, "/", x1, fill=T)
        }

        iter <- iter + 1
    }

    if (abs(x1-x0) > eps){
        return (NULL)
    } else {
        return (x1)
    }
}

# 4(a)
fa <- function(x) cos(x) - 2*x
fa.fd <- function(x) -sin(x) - 2
fa.sd <- function(x) -cos(x)
(xa <- newton.quad(fa, fa.fd, fa.sd, 1, 1e-6, 100, verbose=T))
 
print ("part (b)")
# 4(b)
fb <- function(x) log(x) - exp(-x) - x
fb.fd <- function(x) 1/x + exp(-x) - 1
fb.sd <- function(x) -1/x^2 - exp(-x)
xb <- newton.quad(fb, fb.fd, fb.sd, 2, 1e-6, 100, verbose=T)

print ("part (c)")
# 4(c)
fc <- function(x) x^3 - 2*x - 3
fc.fd <- function(x) 3*x^2 - 2
fc.sd <- function(x) 6*x
xc <- newton.quad(fc, fc.fd, fc.sd, 0, 1e-6, 100, verbose=T)

print ("part (d)")
# 4(d)
fd <- function(x) x^3 - 7*x^2 + 13*x - 8
fd.fd <- function(x) 3*x^2 - 14 * x + 13 
fd.sd <- function(x) 6*x - 14
xd <- newton.quad(fd, fd.fd, fd.sd, 1.9, 1e-6, 100, verbose=T)

print ("part (e)")
fe <- function(x) log(x)*exp(-x) - x
fe.fd <- function(x) exp(-x)/x - log(x) * exp(-x) - 1
fe.sd <- function(x) -(exp(-x)*x + exp(-x))/(x^2) - exp(-x)/x + log(x) * exp(-x)
xe <- newton.quad(fe, fe.fd, fe.sd, 2, 1e-6, 100, verbose=T)
