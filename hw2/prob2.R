
wind <- read.csv("wind.csv", header=T)

# Summary of measurements
print(mean(wind))
print(sd(wind))
print(apply(wind, 2, quantile, probes=c(0, 0.25, 0.5, 0.75, 1)))
print(apply(wind, 2, IQR))

n <- dim(wind)[2]
for (i in 1:n){
    if (dev.cur()[1] == 1){
        pdf('wind.pdf')

        plot(x=i/2*cos(2*pi-wind[,i]*pi/180+pi/2),
            y=i/2*sin(2*pi-wind[,i]*pi/180+pi/2),
            xlab='Cos(wind angle) scaled by season', 
            ylab='Sin(wind angle) scaled by season',
            xlim=c(-n/2, n/2), ylim=c(-n/2, n/2),
            col=i, pch=i,
            main="Wind direction by season")
    }
    else {
        points(x=i/2*cos(2*pi-wind[,i]*pi/180+pi/2), 
                y=i/2*sin(2*pi-wind[,i]*pi/180+pi/2),
                col=i, pch=i)
    }

    lines(x=i/2*cos(seq(0, 2*pi, length.out=180)),
            y=i/2*sin(seq(0, 2*pi, length.out=180)),
            col=i, lty='dotted'
          )
}

abline(v=c(0), col='lightgray', lty='dotted')
abline(h=c(0), col='lightgray', lty='dotted')
legend("topright", names(wind), col=1:dim(wind)[2], pch=1:dim(wind)[2])
dev.off()
