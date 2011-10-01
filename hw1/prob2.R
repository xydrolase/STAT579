# Read dataset
cars <- read.table("http://maitra.public.iastate.edu/stat579/datasets/cars.dat", header=T)

attach(cars)

# (why would stupid Americans keep resisting to switch to metric system?)
# 1 mph = 1.4666667 ft/sec
# 1 mile = 1.6093 km
# 1 foot = 0.3048 m
speed_ftps <- speed * 1.46667
# Plot speed against distance (I guess this is the braking distance, though no 
# such information is mentioned in the question).
pdf('speed_vs_dist_ft.pdf')
plot(x=speed_ftps, y=dist, 
    xlab='Speed (feet/s)', ylab='Braking distance (feet)',
    main='Plotting braking distance against speed')
dev.off()

# Unit conversion, again.
speed_mps <- speed * 1.6093 * 1000 / 3600 
dist_m <- dist * 0.3048

detach(cars)

pdf('speed_vs_dist_m.pdf')
plot(x=speed_mps, y=dist_m, 
    xlab='Speed (m/s)', ylab='Braking distance (meters)',
    main='Plotting braking distance against speed')
dev.off()
