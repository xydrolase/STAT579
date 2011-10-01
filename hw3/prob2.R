d <- as.matrix(read.table("http://www.public.iastate.edu/~maitra/stat579/datasets/fbp-img.dat", header=F))
image(d, col=gray((1:256)/256))

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
        ncol=2, byrow=T) + 
        matrix(rep(c(1, -1), each=length(x)-1), ncol=2))
}

range.compress <- function(m, scale){
    v <- as.vector(m)
    end.pts <- seq(min(v), max(v), length.out=scale+1)
    return (compress(m, end.pts))
}

quantile.compress <- function(m, scale){
    v <- as.vector(m)
    end.pts <- quantile(v, probs=seq(from=0, to=1, length.out=scale+1))
    return (compress(m, end.pts))
}

compress <- function(m, endpoints){
    mid.pts <- midpoints(endpoints)
    # Tag the endpoints such that they can be easily identified in the sorted
    # vector. And mix them into the data point for sorting.

    m.dim <- dim(m)

    full.pts <- c(endpoints + 1i, as.vector(m))
    pts.order <- order(full.pts)
    marked.indices <- which(Im(full.pts[pts.order]) > 0)

    bins <- cbind(intervals(marked.indices), mid.pts)

    # Compress the data into bins
    apply(bins, 1, 
        function(x){
            full.pts[pts.order[seq(from=x[1], to=x[2])]] <<- x[3]
        })

    return (matrix(Re(full.pts[-(1:length(marked.indices))]),
        nrow=m.dim[1], ncol=m.dim[2]))
}

par(mar=c(2.2, 2.2, 1.2, 1.2))
layout(matrix(c(1,2,3,4), ncol=2, byrow=2))
sapply(c(4,8,16), 
    function(x){
        image(range.compress(d, x), 
        col=gray((1:256)/256),
        main=paste(x, 'colors', sep=' '))
    })
image(d, col=gray((1:256)/256), main="Original grayscale")

layout(matrix(c(1,2,3,4), ncol=2, byrow=2))
sapply(c(4,8,16), 
    function(x){ 
        image(quantile.compress(d, x),
        col=gray((1:256)/256),
        main=paste(x, 'colors', sep=' '))
    })
image(d, col=gray((1:256)/256), main="Original grayscale")
