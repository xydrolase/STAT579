craps <- function(n){
    results <- matrix(rep(0, 2*n), nrow=2)
    # first round
    dice.sum <- replicate(n, sum(ceiling(6*runif(2))))
    win.idx <- which(dice.sum %in% c(7, 11))
    results[, win.idx]  <- c(1, 1)
    
    remain <- (1:n)[-win.idx]
    first.round <- dice.sum[-win.idx]
    round <- 2
    while(length(remain)){
        dice.sum <- replicate(length(remain), sum(ceiling(6*runif(2))))
        win.idx <- dice.sum == first.round
        results[, remain[win.idx]] <- c(1, round)
        lose.idx <- dice.sum %in% c(7, 11)
        results[, remain[lose.idx]] <- c(0, round)
        
        # update vector
        remain <- remain[!(win.idx | lose.idx)]
        first.round <- first.round[!(win.idx | lose.idx)]

        round <- round + 1
    }
    return (results)
}

sim.craps <- data.frame(t(craps(10000)))
names(sim.craps) <- c('outcome', 'round')
sim.craps$outcome <- factor(sim.craps$outcome, levels=c(0, 1), labels=c('L', 'W'))
table(sim.craps$outcome)
hist(sim.craps$round, main="Histogram of rounds of gameplay")
