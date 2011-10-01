bill <- read.table("http://maitra.public.iastate.edu/stat579/datasets/senate-109.txt", sep="\t", header=T)
names(bill)[1] <- 'bill_type'

# Keep the bill type only
bill[,1] <- sub('^([^_]+)_.*', '\\1', bill[,1])
bill.tbl <- table(bill[,1])

# Visualize the bill types
par(mar=c(12, 4.1, 4.1, 2.1))
plot(1:length(bill.tbl), bill.tbl, xaxt='n', typ='h', lwd=2,
    xlab='', ylab='Count of bill')
axis(1, at=1:length(bill.tbl), las=2, labels=names(bill.tbl),
    cex.axis=0.7)
axis(2)

# Extract only the votes
vote <- as.matrix(bill[, 3:102])
# Perform matrix multiplication, which gives us a 441x441 matrix. The diagnol terms of this matrix is just the count of votes for each bill
vote.count <- diag(vote %*% t(vote))
# Check if we get the congruent missing counts
all(vote.count == bill$missing_votes)

# Construct a subset of data.frame including only bills Bill Frist voted for.
vote.senate <- subset(vote, vote[,100] != 0)
# Check if our subsetting is correct
dim(vote.senate)[1] == sum(abs(vote[,100]))

vote.leader <- vote.senate[,100]
# Note that vote.leader is a vector. If we column by column take the column vector from the matrix, and perform multiplication elementwise, we can get relative vote with regard to majority leader's vote for each senate as a column vector.

# Columnwise arithmatic multiplication on vector elements.
vote.relative <- vote.leader * vote.senate 

# Map the relative vote matrix to a 3 dimensional array, where the 3rd dimension stores the votes as logical values with regard to vote against, indifferently, with respectively
vote.choice <- outer(vote.relative, -1:1, '==')
# Now aggregate the number of votes against/with or indifferently
# Apply summation on rows for three vote choices
vote.aggregated <- apply(vote.choice, MARGIN=c(1,3), FUN=sum)
# Now columns are summations with regards to vote against, indifferently and with.

# Construct a new data.frame, such that the count of votes, the bill type and the vote choice(against, with, indifferently) are matched
vote.tbl <- aggregate(as.vector(vote.aggregated),
       by=list(bill_type=rep(factor(bill[vote[,100] !=0 ,1]), times=3),
               choice=rep(factor(c('against', 'indifferently', 'with')), 
               each=dim(vote.aggregated)[1]
              )),
       FUN=sum)

names(vote.tbl)[3] <- 'count'

# Visualize this data.frame using ggplot2
library(ggplot2)
p <- ggplot(vote.tbl, 
    aes(x=bill_type, y=count, fill=choice))
print(
p + opts(axis.text.x=theme_text(angle=-90, hjust=0)) +
    xlab('Bill Type') + ylab('Count of votes') + geom_bar()
)
