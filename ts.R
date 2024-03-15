# Load necessary libraries
library(TSA)
library(tseries)
library(forecast)
 
#setting the directory to the file folder
setwd("C:/Users/munta/Downloads")
# Read the CSV file
data <- read.csv("Case_study.csv")

#Preliminary analysis of orders
#Part 1:
Yt <- ts(data$V88) # Retreiving v88 as its given according to my reg number

#plotting the time series 
plot(Yt, main="Time Series Plot of V88", xlab="Time", ylab="V88")

#Part 2:

# Displaying the ACF of the original series
acf(Yt, main="ACF of Original Series")

# Displaying the ACF after first differencing
Yt_diff1 <- diff(Yt, differences=1)
acf(Yt_diff1, main="ACF after First Differencing")
plot(Yt_diff1)

# Displaying the ACF after second differencing (if needed)
Yt_diff2 <- diff(Yt, differences=2)
acf(Yt_diff2, main="ACF after Second Differencing")
plot(Yt_diff2)

# ADF test on the first differenced series
adf_test_diff1 <- adf.test(Yt_diff1, alternative="stationary")
print(adf_test_diff1)


# ADF test on the second differenced series
adf_test_diff2 <- adf.test(Yt_diff2, alternative="stationary")
print(adf_test_diff2)

#part 3
# Calculating and plotting the PACF for the second differenced series
pacf(Yt_diff1)
pacf(Yt_diff2)
#Step 2: Estimation and selection of ARIMA Models

# Defining the ranges for p and q
pmax <- 4
qmax <- 4
d <- 2  # Replace with the actual order of differencing determined from part 2

# Initializing a data frame to store the results
results <- data.frame(p=integer(), q=integer(), AIC=double(), BIC=double())

# Loop over the range of p and q values to fit ARIMA models and calculate AIC/BIC
for (p in 1:pmax) {
  for (q in 1:qmax) {
    # Fit the ARIMA model with the given p, d, q
    model <- Arima(Yt, order=c(p, d, q))
    
    # Store the results
    results <- rbind(results, data.frame(p=p, q=q, AIC=AIC(model), BIC=BIC(model)))
  }
}

# Removing models that are not allowed (0, q) and (p, 0)
results <- subset(results, !(p == 0 & q > 0) & !(p > 0 & q == 0))

# Sorting the results by AIC and BIC
results <- results[order(results$AIC),]
results_bic <- results[order(results$BIC),]

# Selecting the best three models by AIC
best_by_aic <- head(results, 3)

# Selecting the best three models by BIC
best_by_bic <- head(results_bic, 3)

# Printing the best models by AIC
print("Best models by AIC:")
print(best_by_aic)

# Printing the best models by BIC
print("Best models by BIC:")
print(best_by_bic)

#Step 3: Diagnostic tests
#part 1
# Fit ARIMA(3,2,1)
model_321 <- Arima(Yt, order=c(3,2,1))

# Fit ARIMA(1,2,1)
model_121 <- Arima(Yt, order=c(1,2,1))

# Fit ARIMA(1,2,2)
model_122 <- Arima(Yt, order=c(1,2,2))

#for ARIMA(3,2,1)
Box.test(residuals(model_321), lag=10, type="Ljung-Box")
acf(residuals(model_321), main="ACF of Residuals for ARIMA(3,2,1)")
pacf(residuals(model_321), main="PACF of Residuals for ARIMA(3,2,1)")

# for ARIMA(1,2,1) 
Box.test(residuals(model_121), lag=10, type="Ljung-Box")
acf(residuals(model_121), main="ACF of Residuals for ARIMA(1,2,1)")
pacf(residuals(model_121), main="PACF of Residuals for ARIMA(1,2,1)")

# for ARIMA(1,2,2)
Box.test(residuals(model_122), lag=10, type="Ljung-Box")
acf(residuals(model_122), main="ACF of Residuals for ARIMA(1,2,2)")
pacf(residuals(model_122), main="PACF of Residuals for ARIMA(1,2,2)")

#part 2


# Histogram of residuals
hist(residuals(model_321), main="Histogram of Residuals (model 321)", xlab="Residuals")
hist(residuals(model_121), main="Histogram of Residuals (model 121)", xlab="Residuals")
hist(residuals(model_122), main="Histogram of Residuals (model 122)", xlab="Residuals")

# QQ plot of residuals
qqnorm(residuals(model_321))
qqline(residuals(model_321), col = "red")

qqnorm(residuals(model_121))
qqline(residuals(model_121), col = "blue")

qqnorm(residuals(model_122))
qqline(residuals(model_122), col = "green")

# Shapiro-Wilk normality test
shapiro.test(residuals(model_321))
shapiro.test(residuals(model_121))
shapiro.test(residuals(model_122))

#part 4

# BY Choosing ARIMA(1,2,2) as best model
best_model <- Arima(Yt, order=c(1,2,2))

# Fitted values from the model
fitted_values <- fitted(best_model)

# Plot the original time series
plot(Yt, main="Original Time Series vs. Fitted Model", xlab="Time", ylab="Values")

# Lines function to add the fitted values to the plot
lines(fitted_values, col="red")

# Add a legend to distinguish the data
legend("topright", legend=c("Original Data", "Fitted Model"), col=c("black", "red"), lty=1)
summary(best_model)


#Step 4: Forecast


# 'model_122' is the fitted ARIMA(1,2,2) model and 'Yt' is original time series data

# Forecast h=10 steps ahead
forecast_10 <- forecast(model_122, h=10)

# Forecast h=25 steps ahead
forecast_25 <- forecast(model_122, h=25)

# Plot the original time series and h-step predictions
par(mfrow=c(2, 1)) # Set up the plot area for two plots

# Plot for h=10
plot(forecast_10, main="10-step Forecast with 95% Confidence Interval")
lines(Yt, col='blue')

# Plot for h=25
plot(forecast_25, main="25-step Forecast with 95% Confidence Interval")
lines(Yt, col='blue')

# Reset plot area to default
par(mfrow=c(1, 1))






