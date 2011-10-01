# Read dataset
d <- read.table("http://maitra.public.iastate.edu/stat579/datasets/student-apt.dat", header=F)
names(d) <- c('disp', 'apt', 'math', 'lang', 'gknol')

# Factorize disciplines 
disp_levels <- c('tech', 'arch', 'med')
index <- 1
for (lvl in disp_levels){
    d$disp[which(d$disp == index)] <- lvl
    index <- index +1
}

d$disp <- factor(d$disp, levels=disp_levels)

# Plot pairwise scatterplot 
library(ggplot2)
plot_pairs <- matrix(data=c('apt', 'math', 'lang', 'gknol'), 
                    nrow=2, ncol=2)

for (nr in 1:2){
    filename <- paste(plot_pairs[nr, 1], '_', plot_pairs[nr, 2], '.pdf',
                sep="")

    print(filename)
    pdf(filename)

    p <- ggplot(d, aes(x=d[, plot_pairs[nr, 1]],
                       y=d[, plot_pairs[nr, 2]]))
    # output the actual graph
    print(p + geom_point(aes(colour=disp)) + 
        scale_x_continuous(plot_pairs[nr, 1]) + 
        scale_y_continuous(plot_pairs[nr, 2]))

    dev.off()
}
