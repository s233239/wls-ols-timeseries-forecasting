# to ensure this script works, whenever there's a library(someName)
# make sure to run install.packages('someName') in console first
# if warning occurs, install Rtools as described

# Load excel read library
library(readxl)

# Clear variables
rm(list=ls())

# Import Excel file into R
data <- read_excel('DST_BIL54_train.xlsx')
data_test <- read_excel('DST_BIL54_test.xlsx')




# --- Exercise 1.1 ---

# X-axis
# Get time data as single numbers i.e. 2018.0
time_headers <- colnames(data)[-1] # exclude row header
years <- as.integer(substr(time_headers, 1, 4))
months <- as.integer(substr(time_headers, 6, 7))
dates <- years + (months - 1)/12

# Y-axis
# Get number of vehicles per year data excluding row header
y = t(data[1,-1])
y_test = t(data_test[1,-1])

# Plot
plot(dates, y)



# --- Exercise 2.1 ---

# Design matrix x
(x <- cbind(1, dates))

# Estimate of parameters by solving the "normal equations"
OLS <- solve(t(x)%*%x)%*%t(x)%*%y

(theta_0 = OLS[1])
(theta_1 = OLS[2])

# Create a scatter plot of the original data
plot(dates, y, xlab = "Date", ylab = "Vehicles", main = "Linear Regression")

# Add the regression line to the plot
abline(OLS[1], OLS[2], col = "red")

# Add a legend
legend("bottomright", legend = c("Data", "Linear Regression"), col = c("black", "red"), lty = 1)




# --- Exercise 2.2 ---

# Predicted values for all observations
yhat <- x%*%OLS

# Residuals (between observations and predictions)
e_ols <- y - yhat

# Sum of squared residuals
RSS_ols <- t(e_ols)%*%e_ols

# Variance of residuals
n <- length(dates) # number of observations
nparams <- 2 # number of parameters
sigma2_ols <- as.numeric(RSS_ols/(n-nparams))

# Variance-covariance matrix of parameters
v_ols <- sigma2_ols * solve(t(x) %*% x)

# Standard errors of parameters
(se_theta_0 <- (sqrt(diag(v_ols)))[1])
(se_theta_1 <- (sqrt(diag(v_ols)))[2])




# --- Exercise 2.3 ---

# Get time data as single numbers for next 12 months using test data
time_headers_test <- colnames(data_test)[-1] # exclude row header
years_test <- as.integer(substr(time_headers_test, 1, 4))
months_test <- as.integer(substr(time_headers_test, 6, 7))
dates_test <- years_test + (months_test - 1)/12

(x_next12 <- cbind(1, dates_test))

# Predicted values
yhat_pred <- x_next12%*%OLS

# Prediction variance-covariance matrix 
vmatrix_pred <- sigma2_ols*(1+(x_next12%*%solve(t(x)%*%x))%*%t(x_next12))

# Prediction intervals
y_pred_lwr <- yhat_pred - 1.96*sqrt(diag(vmatrix_pred))
y_pred_upr <- yhat_pred + 1.96*sqrt(diag(vmatrix_pred))


# Create a data frame with dates_2023 as the headers and yhat_pred as the values
table_data <- data.frame(Date = time_headers_test, yhat_pred, y_pred_lwr, y_pred_upr)

# Get LaTeX table:
library(xtable)

# Convert data frame to LaTeX table format
(latex_table <- xtable(table_data))



# --- Exercise 2.4 ---

x_min <- min(dates, dates_test)
x_max <- max(dates, dates_test)
y_min <- min(y, yhat_pred)
y_max <- max(y, yhat_pred, y_pred_upr)

# Create a scatter plot of the original data
plot(dates, y, xlab = "Date", ylab = "Vehicles", main = "Linear Regression with predicted values", xlim = c(x_min, x_max), ylim = c(y_min, y_max))

# Add a colored rectangle representing the prediction interval
polygon(c(dates_test, rev(dates_test)), c(y_pred_lwr, rev(y_pred_upr)), col = "lightblue", border = NA)

# Add the regression line to the plot
abline(OLS[1], OLS[2], col = "blue")

# Add the predicted values to the polot
points(dates_test, yhat_pred, col = "red", pch = 19)

# Add a legend
legend("bottomright", legend = c("Data", "Linear Regression", "Predicted Values"), col = c("black", "blue", "red"), lty = 1)




# --- Exercise 2.6 ---

# Plot residuals
plot(dates, e_ols, xlab="date", ylab="residuals")


# Inspect residuals
acf(e_ols, main="Series of OLS residuals")

