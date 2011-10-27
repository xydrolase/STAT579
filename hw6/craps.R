craps <- function(n){
    results <- matrix(rep(0, 2*n), nrow=2)
    # first round
    dice.sum <- replicate(n, sum(ceiling(6*runif(2))))
    win.idx <- which(dice.sum %in% c(7, 11))
    results[, win.idx]  <- c(1, 1)
    
    remain <- (1:n)[-win.idx]
    print (remain)
    prev.round <- dice.sum[-win.idx]
    round <- 2
    while(length(remain)){
        dice.sum <- replicate(length(remain), sum(ceiling(6*runif(2))))
        win.idx <- dice.sum == prev.round
        results[, remain[win.idx]] <- c(1, round)
        lose.idx <- dice.sum %in% c(7, 11)
        results[, remain[lose.idx]] <- c(0, round)
        
        # update vector
        remain <- remain[!(win.idx | lose.idx)]
        print (remain)
        prev.round <- dice.sum[!(win.idx | lose.idx)]

        round <- round + 1
    }
    return (results)
}
