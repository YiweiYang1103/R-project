# Spline Line
splinedata<- read.csv(
  "https://rgaykroyd.github.io/MATH3823/Datasets/engine-36.csv", header=T)
head(splinedata)
cor(splinedata)
# exploring the data
x <- splinedata$size
y <- splinedata$wear
ggplot(splinedata, aes(x = x,y = y)) +
  geom_point() +  
  geom_smooth()+
  labs(x = "Size", y = "Wear")
summary(splinedata)

# modelling
myfit1 = smooth.spline(splinedata$size,splinedata$wear,lambda = 0.001)
curve(myfit1(x,deriv=1), 0, 10, lwd=1.5,xlab="x", ylab="First derivative")
abline(v=x, col="grey"); abline(h=0, col="grey")

residuals <- splinedata$wear - predict(
  myfit1, x = splinedata$size)$y
# Calculate standard deviation of residuals
residuals_sd <- sd(residuals)
# Output standard deviation of residuals
print(residuals_sd)

# visualise mtfit
plot(splinedata$size,splinedata$wear,
     xlim=c(1,3),ylim=c(1,5),pch=16)
fit.locations = seq(0,10,0.01)
fitted = predict(myfit1, fit.locations)
lines(fitted,col="blue")


# Create a vector of lambda values
lambda_values <- 10^seq(-4,5,by=1) 
# Create an empty plot
plot(splinedata$size, splinedata$wear, 
     xlim = c(1, 3), ylim = c(1.5, 5), 
     pch = 16, xlab = "Size", ylab = "Wear")
results <- list()
for (lambda_val in lambda_values) {
  myfit <- smooth.spline(splinedata$size, 
                         splinedata$wear, lambda = lambda_val)
  fitted <- predict(myfit, fit.locations)
  lines(fitted, 
        col = rainbow(length(lambda_values))[
          which(lambda_values == lambda_val)])
  lam <- lambda_val
  cv <- myfit$cv.crit
  
}
# Add legend
legend("topright", 
       legend = paste("lambda =", lambda_values), 
       col = rainbow(
         length(lambda_values)), lty = 4, cex = 0.6)
print(cv)

# Create a vector of lambda values
lambda_values <- 10^seq(-5, 5, by = 0.1)  
plot(splinedata$size, splinedata$wear, 
     xlim = c(1, 3), ylim = c(1.5, 5), 
     pch = 16, xlab = "Size", ylab = "Wear")
# Fit smoothing splines with different lambda values 
results <- list()
cv_values <- numeric(length(lambda_values))  
for (i in seq_along(lambda_values)) {
  lambda_val <- lambda_values[i]
  myfit <- smooth.spline(
    splinedata$size, splinedata$wear, lambda = lambda_val)
  fitted <- predict(myfit, fit.locations)
  # Plot the fitted curve
  lines(fitted, col = rainbow(length(lambda_values))[i])
  print(lambda_val)
  print(myfit$cv.crit)
  # Store cross-validation value
  cv_values[i] <- myfit$cv.crit
}
legend("topright", 
       legend = paste("lambda =", lambda_values), 
       col = rainbow(length(lambda_values)), 
       lty = 1, cex = 0.6)
# Plot cross-validation values
plot(lambda_values, cv_values, 
     type = "l", xlab = "lambda", 
     ylab = "Cross-validation criterion", 
     xlim = c(1e-05, 1e+03),log = "x")

# predicted values
predicted_values <- numeric(length(lambda_values))

size_to_predict <- 2.6
for (i in seq_along(lambda_values)) {
  lambda_val <- lambda_values[i]
  myfit <- smooth.spline(
    splinedata$size, splinedata$wear, lambda = lambda_val)
  predicted_values[i] <- predict(myfit, size_to_predict)$y
  print(lambda_val)
  print(predict(myfit, size_to_predict)$y)
}

plot(lambda_values, predicted_values, 
     type = "l", xlab = "lambda", 
     ylab = "Predicted Wear for Size 2.6L", 
     ylim = c(2.6, max(predicted_values)), log = "x")

