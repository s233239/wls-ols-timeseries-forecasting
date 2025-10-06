# to ensure this script works, whenever there's a library(someName)
# make sure to run install.packages('someName') in console first
# if warning occurs, install Rtools as described

# load excel read library
library(readxl)

# clear variables
rm(list=ls())

# import Excel file into R
data <- read_excel('DST_BIL54_train.xlsx')
data_test <- read_excel('DST_BIL54_test.xlsx')

# Get number of vehicles per year data excluding row header
y = t(data[1,-1])
y_test = t(data_test[1,-1])

time_headers <- colnames(data_test)[-1] # exclude row header
years <- as.integer(substr(time_headers, 1, 4))
months <- as.integer(substr(time_headers, 6, 7))
dates_test <- years + (months - 1)/12



# --- Exercise 1.1 ---
# Get time data as single numbers i.e. 2018.0

time_headers <- colnames(data)[-1] # exclude row header
years <- as.integer(substr(time_headers, 1, 4))
months <- as.integer(substr(time_headers, 6, 7))
dates <- years + (months - 1)/12

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
qqnorm(e_ols)
qqline(e_ols)

acf(e_ols, main="Series of OLS residuals")





# Exercise 3.1

lambda <- 0.7

# lambda vector
lambda_n <- rep(1, n)

for (i in 1:n) {
  lambda_n[i] <- lambda^(n-i)
}

# diag matrix of weight values
SIGMA_inv <- diag(lambda_n)

# define SIGMA variance-covariance matrix (inverse of weight values)
SIGMA <- solve(SIGMA_inv)




# Exercise 3.2

plot(dates, lambda_n, xlab = "Date", ylab = "Lambda", main = "WLS weights plotted against time")




# Exercise 3.3

T <- sum(lambda_n)



# Exercise 3.4

# set lambda to new value 
lambda <- 0.9

# lambda vector
lambda_n <- rep(1, n)

for (i in 1:n) {
  lambda_n[i] <- lambda^(n-i)
}

# diag matrix of weight values
SIGMA_inv <- diag(lambda_n)

# define SIGMA variance-covariance matrix (inverse of weight values)
SIGMA <- solve(SIGMA_inv)

# estimate parameters with WLS
theta_WLS <- solve(t(x)%*%solve(SIGMA)%*%x)%*%(t(x)%*%solve(SIGMA)%*%y)

# model parameters
beta_0 = theta_WLS[1]
beta_1 = theta_WLS[2]
  



# plot
# Create a scatter plot of the original data
plot(dates, y, xlab = "Date", ylab = "Vehicles", main = "WLS Linear Regression")

# Add the regression line to the plot
abline(theta_WLS[1], theta_WLS[2], col = "red")

# Add a legend
legend("bottomright", legend = c("Data", "WLS Linear Regression"), col = c("black", "red"), lty = 1)




# Exercise 3.5
# 
# yhat_wls_lambda09 <- x_next12%*%theta_WLS
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# e_wls <- y - yhat_wls
# RSS_wls <- t(e_wls)%*%solve(SIGMA)%*%e_wls
# sigma2_wls <- as.numeric(RSS_wls/(n - 2))
# 
# sqrt(sigma2_wls)
# 
# V_wls <- sigma2_wls *  solve(t(x)%*%solve(SIGMA)%*%x)
# 
# (sqrt(diag(V_wls))) 
# 









### Task 4
# Exercise 4.1: provide L and f(0)

f <- function(j) rbind(1, j)
f_0 <- f(0)

L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)

print(L)
print(f(0))


# Exercise 4.2: provide F1 and h1

i <- 1 
(F_N <-  1 * f(0)%*%t(f(0))) # N=1 ; lambda^0=1
(h_N <-  1 * f(0) * y[i])

print(F_N) #F_1
print(h_N) #h_1


# Exercise 4.3: update FN and hN recursively and provide F10 and h10

lambda = 0.9
Linv <- solve(L)

for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
}

print(F_N) #F_10
print(h_N) #h_10

# Exercise 4.4: update the model recursively up to F59 and h59
# while also calculating predictions at each step

lambda = 0.9
library(ggplot2)
df<-data.frame(date=x[,2],
               value=y[1:59])

i <- 1 
(F_N <-  1 * f(0)%*%t(f(0))) # N=1 ; lambda^0=1
(h_N <-  1 * f(0) * y[i])

for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
}

# recursively update to F59 and h59
for (i in 11:58){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  
  # estimate the parameters now
  theta_N <- solve(F_N)%*%h_N
  
  # calculate predictions
  yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N
  plot_N <- ggplot(df, aes(x=date, y=value)) +
    geom_point()+
    geom_point(data=df[1:i,], col="blue") +
    geom_line(data=df[1:i,], aes(y=yhat_N[1:i]), col="blue")+
    geom_line(data=df[1:(i+1),], aes(y=yhat_N[1:(i+1)]), col="red", linetype=2)+
    geom_point(data=df[i+1,], aes(y=yhat_N[i+1]), col="red", size=4)+
    ggtitle(paste0("N = ", i))
    theme_bw()
  print(plot_N)
}


# 4.5 one step predictions (one month)， 6-month, 12-month

# draw 3 preditions for the future
library(ggplot2)
test_set <- data.frame(date=dates_test,value=y_test)

df<-data.frame(date=x[,2],
               value=y[1:59])

lambda = 0.9


i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

for (i in 2:9){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
}

data_prediction <-data.frame(month1= rep(NA, 59+12),month6= rep(NA, 59+12),month12= rep(NA, 59+12))

# update iteratively:
for (i in 10:59){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
  
  # calculate the predicted value
  data_prediction$month1[i+1] <- t(f(1))%*%theta_N
  data_prediction$month6[i+6] <- t(f(6))%*%theta_N
  data_prediction$month12[i+12] <- t(f(12))%*%theta_N
}

X_1 <- df[,1][10:length(df[,1])]+1/12
X_6 <- df[,1][10:length(df[,1])]+1/2
X_12 <- df[,1][10:length(df[,1])]+1
pre_1 <- data_prediction$month1[11:60]
pre_6 <- data_prediction$month6[16:65]
pre_12 <- data_prediction$month12[22:71]


ggplot() +
  geom_point(aes(x = X_1, y = pre_1, col="1 Month Prediction"), size=3) +
  geom_point(aes(x = X_6, y = pre_6, col="6 Month Prediction"), size=3) +
  geom_point(aes(x = X_12, y = pre_12, col="12 Month Prediction"), size=3) +
  geom_point(aes(x = df[,1], y = df[,2]), color = "black") + # True values, trimmed to the length of predictions
  scale_color_manual(values = c("1 Month Prediction" = "red", 
                                "6 Month Prediction" = "green",
                                "12 Month Prediction" = "blue"))+
  theme_minimal() +
  labs(x = "Time", y = "Number of Vehicles in total ") +
  theme(legend.position = c(0.8, 0.4))



# 4.6

library(ggplot2)
df<-data.frame(date=x[,2],
               value=y[1:59])


# Define lambda values
lambda_values <- seq(0.55, 0.95, by = 0.01)

# Initialize dataframe to store RMSE values
rmse_df_1 <- data.frame(value_1 = numeric())
rmse_df_6 <- data.frame(value_6 = numeric())
rmse_df_12 <- data.frame(value_12 = numeric())

for (lambda in lambda_values) {
  i <- 1
  (F_N <-  (lambda^0) * f(0)%*%t(f(0)))
  (h_N <-  (lambda^0) * f(0) * y[i])
  
  
  for (i in 2:10){
    F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
    h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  }
  
  df$onestep_lamb_09  <- NA
  df$sixstep_lamb_09  <- NA
  df$twelvestep_lamb_09  <- NA
  
  # update iteratively:
  for (i in 11:59){
    F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
    h_N <- lambda * Linv %*% h_N + f(0)*y[i]
    theta_N <- solve(F_N)%*%h_N

    # # calculate the predicted value
    # df$onestep_lamb_09[i+1] <- t(f(1))%*%theta_N

    yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N

    if (i+1<=59){
      df$onestep_lamb_09[i+1] <- yhat_N[i+1]
    }
    # Six months ahead prediction
    if (i + 6 <= 59) {  # Ensure not to exceed the length of the dataset
      df$sixstep_lamb_09[i+6] <- yhat_N[i+6]
    }

    # Six months ahead prediction
    if (i + 12 <= 59) {  # Ensure not to exceed the length of the dataset
      df$twelvestep_lamb_09[i+12] <- yhat_N[i+12]
    }
  }
  
  one_step_errors <- df$value[12:59] - df$onestep_lamb_09[12:59]
  one_step_rms <- sqrt(mean(one_step_errors^2))
  
  six_step_errors <- df$value[17:59] - df$sixstep_lamb_09[17:59]
  six_step_rms <- sqrt(mean(six_step_errors^2))
  
  twelve_step_errors <- df$value[23:59] - df$twelvestep_lamb_09[23:59]
  twelve_step_rms <- sqrt(mean(twelve_step_errors^2))
 
  
  rmse_df_1 <- rbind(rmse_df_1, data.frame(value_1 = one_step_rms))
  rmse_df_6 <- rbind(rmse_df_6, data.frame(value_6 = six_step_rms))
  rmse_df_12 <- rbind(rmse_df_12, data.frame(value_12 = twelve_step_rms))
}



# 4.7, 4.8, 4.9
 
rmse_total <- data.frame(lambda = lambda_values,rmse_1=rmse_df_1,rmse_6=rmse_df_6,rmse_12=rmse_df_12)  


ggplot(rmse_total, aes(x = lambda)) +
  geom_point(aes(y = value_1, color = "value_1")) +  # Points for y1
  geom_point(aes(y = value_6, color = "value_6")) +  # Points for y6
  geom_point(aes(y = value_12, color = "value_12")) +  # Points for y12
  scale_color_manual(values = c("value_1" = "green", "value_6" = "blue", "value_12" = "red"), 
                     labels = c("RMSE 1", "RMSE 6", "RMSE_12")) +
  labs(color = "") +
  xlab("Lambda_value") +   # Set x-axis label
  ylab("RMSE") +   # Set y-axis label
  theme(legend.position = "bottom")  # Adjust legend position
 
  


# 4.10

lambda <- 0.5
n = 60

# lambda vector
lambda_n <- rep(1, n)

for (i in 1:n) {
  lambda_n[i] <- lambda^(n-i)
}

# diag matrix of weight values
SIGMA_inv <- diag(lambda_n)

plot( lambda_n, ylab = "Lambda", main = "WLS weights plotted against time")


T <- sum(lambda_n)

# 4.11

library(ggplot2)
df<-data.frame(date=x[,2],
               value=y[1:59])
lambda = 0.55

i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
}


df$onestep_lamb_055  <- NA
df$naive_model  <- NA


# update iteratively:
for (i in 11:58){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
  
  yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N
  
  df$onestep_lamb_055[i+1] <- yhat_N[i+1]
  df$naive_model[i+1] <- df$value[i]
  
}

one_step_errors_055 <- df$value[12:59] - df$onestep_lamb_055[12:59]
one_step_rms_055 <- sqrt(mean(one_step_errors_055^2))

naive_model_err <- df$value[12:59] - df$naive_model[12:59]
naive_model_rms <- sqrt(mean(naive_model_err^2))

naive_model<-data.frame(date=df[12:59,1],
               value=df[12:59,4])

one_step<-data.frame(date=df[12:59,1],
                        value=df[12:59,3])



ggplot() +
  geom_point(data = df[1:59,], aes(x = date, y = value, color = "Original Data")) +
  geom_point(data = one_step, aes(x = date, y = value, color = "One Step Prediction")) +
  geom_point(data = naive_model, aes(x = date, y = value, color = "Naive Model")) +
  labs(color = "") +
  xlab("Date") +
  ylab("Value") +
  ggtitle("1-month-ahead prediction VS naive persistence model") +
  scale_color_manual(values = c("Original Data" = "blue", "One Step Prediction" = "red", "Naive Model" = "green"))+
  theme(legend.position = "bottom")





# 4.12
library(ggplot2)
test_set <- data.frame(date=dates_test,value=y_test)

df<-data.frame(date=x[,2],
               value=y[1:59])


# Define lambda values
lambda_values <- seq(0.55, 0.95, by = 0.01)

# Initialize dataframe to store RMSE values
rmse_df_1 <- data.frame(value_1 = numeric())
rmse_df_6 <- data.frame(value_6 = numeric())
rmse_df_12 <- data.frame(value_12 = numeric())

# Perform predictions for each ?? value
for (lambda in lambda_values) {
  i <- 1
  (F_N <-  (lambda^0) * f(0)%*%t(f(0)))
  (h_N <-  (lambda^0) * f(0) * y[i])
  
  for (i in 2:10){
    F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
    h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  }
  
  # update iteratively:
  for (i in 11:59){
    F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
    h_N <- lambda * Linv %*% h_N + f(0)*y[i]
    theta_N <- solve(F_N)%*%h_N
  }
  pre_value_1 <- t(f(1))%*%theta_N
  pre_value_6 <- t(f(6))%*%theta_N
  pre_value_12 <- t(f(12))%*%theta_N
  real_1 <- test_set[1,2]
  real_6 <- test_set[6,2]
  real_12 <- test_set[12,2]
  
  errors_1 <- real_1 - pre_value_1
  rms_1 <- sqrt(mean(errors_1^2))
  
  errors_6 <- real_6 - pre_value_6
  rms_6 <- sqrt(mean(errors_6^2))
  
  errors_12 <- real_12 - pre_value_12
  rms_12 <- sqrt(mean(errors_12^2))
  
  rmse_df_1 <- rbind(rmse_df_1, data.frame(value_1 = rms_1))
  rmse_df_6 <- rbind(rmse_df_6, data.frame(value_6 = rms_6))
  rmse_df_12 <- rbind(rmse_df_12, data.frame(value_12 = rms_12))
}

dfff_1 <-data.frame(lambda=lambda_values,value=rmse_df_1)
dfff_6 <-data.frame(lambda=lambda_values,value=rmse_df_6)
dfff_12 <-data.frame(lambda=lambda_values,value=rmse_df_12)


ggplot() +
  geom_point(data = dfff_1, aes(x = lambda, y = value_1, col="future time: t = 60"), size=3) +
  geom_point(data = dfff_6, aes(x = lambda, y = value_6, col="future time: t = 65"), size=3) +
  geom_point(data = dfff_12, aes(x = lambda, y = value_12, col="future time: t = 71"), size=3) +
  scale_color_manual(values = c("future time: t = 60" = "red", 
                                "future time: t = 65" = "green",
                                "future time: t = 71" = "blue"))+
  theme_minimal() +
  labs(x = "lambda", y = "RMSE(current time:59) ") +
  theme(legend.position = c(0.5, 0.8))

# RMSE: 37.11062 lambda: 0.61 for one month prediction
# RMSE: 692.0372 lambda: 0.78 for 6 month prediction
# RMSE: 862.8594 lambda: 0.78 for 12 month prediction


lambda = 0.61   # for calculate pre_value_1

i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
}

# update iteratively:
for (i in 11:59){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
}
pre_value_1 <- t(f(1))%*%theta_N
real_1 <- test_set[1,2]
real_6 <- test_set[6,2]
real_12 <- test_set[12,2]


lambda = 0.78      # for calculate pre_value_6, pre_value_12

i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
}

# update iteratively:
for (i in 11:59){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
}

pre_value_6 <- t(f(6))%*%theta_N
pre_value_12 <- t(f(12))%*%theta_N

df_1 <- data.frame(date=test_set[1,1], value = pre_value_1)
df_6 <- data.frame(date=test_set[6,1], value = pre_value_6)
df_12 <- data.frame(date=test_set[12,1], value = pre_value_12)


ggplot() +
  geom_point(data = df[1:59,], aes(x = date, y = value, color = "training set")) +
  geom_point(data = test_set, aes(x = date, y = value, color = "testing set")) +
  geom_point(data = df_1, aes(x = date, y = value, color = "t=60")) +
  geom_point(data = df_6, aes(x = date, y = value, color = "t=65")) +
  geom_point(data = df_12, aes(x = date, y = value, color = "t=71")) +
  labs(color = "") +
  xlab("Date") +
  ylab("Value") +
  ggtitle("Forecasting at t = 59 for t = 60, t = 65 and t = 71") +
  scale_color_manual(values = c("training set" = "black", "testing set" = "blue", "t=60" = "red", "t=65" = "darkorchid", "t=71" = "green"))+
  theme(legend.position = "bottom")

