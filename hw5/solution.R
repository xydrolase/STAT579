library(MASS)
mycars <- as.matrix(Cars93[, c('Min.Price', 'Max.Price', 
                       'MPG.city', 'MPG.highway', 
                       'EngineSize', 'Length', 'Weight')])

layout(matrix(1:8, ncol=2))
apply(mycars, 2, function(x) hist(x))

car.mean <- apply(mycars, 2, mean)
n <- nrow(mycars)
car.se <- apply(mycars, 2, sd)/sqrt(n)
# Assume normality
car.ci <- rbind(car.mean - car.se * qnorm(0.99), 
                car.mean + car.se * qnorm(0.99))

(cars.stats <- list(Cars.Means=car.mean,
                   Cars.Std.Errors=car.se,
                   Cars.CI.99=car.ci))
cars.stats

# ----- Problem 2 ------
(mean.income <- tapply(state.x77[, 'Income'], 
                      INDEX=state.region,
                      FUN=mean))


(max.illiteracy.rate <- tapply(state.x77[, 'Illiteracy'],
                              INDEX=state.division,
                              FUN=max))

(count.state.region <- tapply(rep(1, times=nrow(state.x77)), 
                             INDEX=state.region,
                             FUN=sum))
sum(count.state.region)

(count.state.division <- tapply(rep(1, times=nrow(state.x77)),
                                INDEX=state.division,
                                FUN=sum))

state.size <- cut(state.x77[, 'Population'], 
                  breaks=c(0, 2000, 10000, Inf),
                  labels=c('Small', 'Medium', 'Large'))

state.group <- factor(paste(state.region, state.size, sep='/'))

(med.hs.grad <- tapply(state.x77[, "HS Grad"],
                      INDEX=state.group,
                      FUN=median))

# ---- Problem 3 ----
cor.matrix <- function(x){
    cov.m <- cov(x)
    xx.sd <- sqrt(diag(cov(x)))
    return(sweep(sweep(cov.m, MARGIN=1,
                 STATS=xx.sd, FUN='/'),
                 MARGIN=2, STATS=xx.sd,
                 FUN='/'))
}

x <- matrix(rnorm(16), ncol=4)
all.equal(cor(x), cor.matrix(x))

# ---- Problem 4 ----
(cars.mad <- apply(mycars, 2, mad))
(cars.mad.alt <- apply(mycars, 2, 
                       function(x) 1.4826 * median(abs(x-median(x)))))
all.equal(cars.mad.alt, cars.mad)
