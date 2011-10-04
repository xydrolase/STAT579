TIMEPOINTS <- 11

standardize <- function(m){
    # Standarize a matrix by substracting means for each row and divide the
    # data points by its standard deviation.

    n.cols <- dim(m)[2]
    # Calculate mean of mean abundance level on each line (probe)
    # and replicate it N_timepoints times to span a matrix with the same
    # dimesion as m.
    m.means <- matrix(rep(apply(m, MARGIN=1, FUN=mean), times=n.cols),
        ncol=n.cols)
    v.sds <- apply(m, MARGIN=1, FUN=sd)

    # Standarize the original matrix m

    # For a matrix, the arithmatic division is performed columnwise. 
    # Since v.sds is a 28810x1 column vector, the following operatioin will
    # gurantee each probe's expression levels got divided by its own stddev.
    return ((m - m.means)/v.sds)
}

arab.array <- read.csv("http://www.public.iastate.edu/~maitra/stat579/datasets/diurnaldata.csv", header=T)
dim(arab.array)

arab.expr.lvl <- as.matrix(arab.array[,2:23])

# Convert the 22810x22 matrix to a 22810x11x2 array 
arab.replicates <- array(arab.expr.lvl, 
    c(dim(arab.expr.lvl)[1], TIMEPOINTS, 2))


# Calculate the mean abundance level across two replicates
arab.means <- apply(arab.replicates, MARGIN=c(1,2), FUN=mean)
arab.means.scaled <- standardize(arab.means)


# Read in the 20 observation of expression levels
array.means <- as.matrix(read.table('http://maitra.public.iastate.edu/stat579/datasets/micromeans.dat', header=F))
array.means.scaled <- standardize(array.means)


# Replicate scaled means for Arabidopsis data for 20 copys on the 3rd dimension
# Vectorize the matrix first, replicate, and map it to 3d space (array).

# So this is what the data will look like:

#  ^ z=20
#  |     _. x=28810
#  |     /|
#  |    /---/
#  |   /xxx/
#  |  /yyy/
#  | /zzz/
#  |/___/
#  +---------------> y = 11

#which will be replicated along the z-axis twenty fold
arab.means.replicated <- array(as.vector(array.means.scaled), 
    dim=c(dim(arab.means.scaled)[1], TIMEPOINTS, 20))

# Now replicate the 20x11 matrix 28810 folds along x-axis.

#  ^ z=20
#  |     _. x=28810
#  |     /|
#  |----/---
#  |xxx/xxx|
#  |yy/yyyy|
#  |z/zzzzz|
#  |/wwwwww|
#  +---------------> y = 11

# We wish to replicate the 20x11 matrix in the 1st dimension such that, 
# within the first two dimensions, we see,

# Mx,1 Mx,2 Mx,3 ... Mx,11  \
# Mx,1 Mx,2 Mx,3 ... Mx,11   | 
#           ...              |
#           ...              |-- 28810 rows
#           ...              | 
# Mx,1 Mx,2 Mx,3 ... Mx,11   |
# Mx,1 Mx,2 Mx,3 ... Mx,11  /

# where x is the index of the 3rd dimension.

# in array.means.scaled, what we have is,

# M1,1 M1,2 M1,3 ... M1,11
# M2,1 M2,2 M2,3 ... M2,11
#         ...

# Remember that R does all matrice stuff (storing, vectorizing) in columnwise fashion,
# so transpose the matrix array.means.scaled (now 11x20), vectorize, and replicate each element for 28810 times, will generate the data that looks like:

# M1,1 M1,1 ... (28810 times) M1,2 M1,2 .. (28810 times) ... (11 items) 
# M2,1 M2,1 ... (28810 times) ...

# Think about after vectorizing, you get a (11x20) elements row vector (v),
# [M1,1, M1,2 ... M1,20 M2,1 M2,2 ... M11, 1 ... M11, 20]
# Use rep(v, each=28810), you can think this process as growing a column out of every element in this row vector, using its value.
# So when you convert this replicated vector into a 3d array, all the data fit into the correct spot.

# Consider fill these data points into a 28810x11x20 array, will look exactly like what we show above (28810x11 matrix), within the first two dimensions, but 20 different matrices on different Z-values.

array.means.replicated <- array(
    rep(as.vector(t(array.means.scaled)),
        each=dim(arab.means.scaled)[1]),
    dim=c(dim(arab.means.scaled)[1], TIMEPOINTS, 20)
)

# Note that the matrices spanned on z-y axis (20x11) and x-y axis (28810x11) are orthogonal to each other. The overlapped segment contains 11 elements.
# Now we superimpose two 3d cube, compute the differences between two points that overlap with each other, which gives a 28810x11x20 array.
# Compute the squared value for each point in this 3d space, and sum over y-axis. This will end up with a 28810x20 matrix where each point indicates the Euclidean distances from the 11 timepoints in X-th probe to the 11 timepoints in Z-th row of the 20x11 matrix.

euclid.dist.comp <- (arab.means.replicated - array.means.replicated)^2
euclid.dist <- apply(euclid.dist.comp, MARGIN=c(1, 3), FUN=sum)

# We don't necessarily need to compute the square root of each point, since if Dx > Dy, Dx^2 > Dy^2 as well. Now we just find the Z that is closest to each given X using apply() function again.

# For each row in the 28810x10 matrix, we use function order() to sort the distances (squared) in increasing order, and the 1st element in sorted orders is the Z which minimizes the Euclidean distance.

closest.mean <- apply(euclid.dist, MARGIN=1,
    function(x) order(x)[1])

closest.tbl <- table(closest.mean)
print(closest.tbl)
