###################################################
### chunk number 1: 
###################################################
#line 7 "prob2.Rnw"
d <- as.matrix(read.table("http://www.public.iastate.edu/~maitra/stat579/datasets/fbp-img.dat", header=F))


###################################################
### chunk number 2: 
###################################################
#line 11 "prob2.Rnw"
image(d, col=gray((1:256)/256))


###################################################
### chunk number 3: 
###################################################
#line 18 "prob2.Rnw"
midpoints <- function(x){
    return (apply(
        matrix(x[c(1, rep(2:(length(x)-1), each=2), length(x))],
            ncol=2, byrow=T),
        MARGIN=1,
        FUN=mean
        ))
}

intervals <- function(x){
    return (matrix(
        x[c(1, rep(2:(length(x)-1), each=2), length(x))], 
        ncol=2, byrow=T))
}


###################################################
### chunk number 4: 
###################################################
#line 40 "prob2.Rnw"
compress <- function(m, endpoints){
    mid.pts <- midpoints(endpoints)

    m.dim <- dim(m)

    data.pts <- as.vector(m)
    bins <- cbind(intervals(endpoints), mid.pts)

    # Compress the data into bins
    apply(bins, 1, 
        function(x){
            data.pts[x[1] < data.pts & data.pts <= x[2]] <<- x[3]
        })

    return (matrix(data.pts, nrow=m.dim[1], ncol=m.dim[2]))
}


###################################################
### chunk number 5: 
###################################################
#line 61 "prob2.Rnw"
range.compress <- function(m, scale){
    v <- as.vector(m)
    end.pts <- seq(min(v), max(v), length.out=scale+1)
    return (compress(m, end.pts))
}


###################################################
### chunk number 6: 
###################################################
#line 69 "prob2.Rnw"
par(mar=c(2.2, 2.2, 1.2, 1.2))
layout(matrix(c(1,2,3,4), ncol=2, byrow=2))
sapply(c(4,8,16), 
    function(x){
        image(range.compress(d, x), 
        col=gray((1:256)/256),
        main=paste(x, 'colors', sep=' '))
    }) -> p 
image(d, col=gray((1:256)/256), main="Original grayscale")


###################################################
### chunk number 7: 
###################################################
#line 81 "prob2.Rnw"
quantile.compress <- function(m, scale){
    v <- as.vector(m)
    end.pts <- quantile(v, probs=seq(from=0, to=1, length.out=scale+1))
    return (compress(m, end.pts))
}


###################################################
### chunk number 8: 
###################################################
#line 89 "prob2.Rnw"
par(mar=c(2.2, 2.2, 1.2, 1.2))
layout(matrix(c(1,2,3,4), ncol=2, byrow=2))
sapply(c(4,8,16), 
    function(x){ 
        image(quantile.compress(d, x),
        col=gray((1:256)/256),
        main=paste(x, 'colors', sep=' '))
    }) -> p
image(d, col=gray((1:256)/256), main="Original grayscale")


###################################################
### chunk number 9: 
###################################################
#line 104 "prob2.Rnw"
layout(c(1))
hist(as.vector(d))


###################################################
### chunk number 10: 
###################################################
#line 117 "prob2.Rnw"
d.truncated <- d
d.truncated[d.truncated < 1000] <- 0
image(d.truncated, col=gray((1:256)/256))


