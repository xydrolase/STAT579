# R program to problem 3 of HW1 / STAT 579

# Convert temperature from Celsius scale to Fahrenheit
temp.f <- pressure$temperature * 9 / 5 + 32
pres.f <- data.frame(temperature=temp.f, pressure=pressure$pressure)

# Linear regression
fit <- lm(temperature ~ 0 + pressure, data=pres.f)
summary(fit)

# Plot temperature against pressure with fitted line
pdf('temp_pres.pdf')
plot(x=pres.f$pressure, y=pres.f$temperature)
# Draw regression line
abline(a=0, b=fit$coefficients[1])
dev.off()

pdf('fitted_residual.pdf')
plot(x=fit$fitted.values, y=fit$residuals,
    xlab='Fitted values', ylab='Residuals',
    main='Residual plot of linear fit of temperature against pressure')
dev.off()

# A new model...
pres.full <- data.frame(temperature=temp.f,
    pres=pressure$pressure,
    pres.sq=pressure$pressure^2,
    pres.cubic=pressure$pressure^3)

# Fit it with a MLR
fit.full <- lm(temperature ~ pres + pres.sq + pres.cubic, data=pres.full)
pdf('pres_mlr.pdf')
plot(x=pres.f$pressure, y=pres.f$temperature, 
    xlab='Pressure (mm/Hg)', ylab='Temperature (F)',
    main='Fit water vapor temperature against pressures')
lines(x=pres.full$pres, y=fit.full$fitted.values)
dev.off()

# Residual plot for MLR
pdf('fitted_residuals_mlr.pdf')
plot(x=fit.full$fitted.values, y=fit.full$residuals,
    xlab='Fitted values', ylab='Residuals',
    main='Residual plot of multiple linear regression of temperature against pressures')
dev.off()
