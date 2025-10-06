# to ensure this script works, whenever there's a library(someName)
# make sure to run install.packages('someName') in console first
# if warning occurs, install Rtools as described

# load library
library(readxl)
library(ggplot2)

# clear variables
rm(list=ls())

# import Excel file into R
data <- read_excel('DST_BIL54_train.xlsx')
data_test <- read_excel('DST_BIL54_test.xlsx')

# get number of vehicles per year data excluding row header
y = t(data[1,-1])
y_test = t(data_test[1,-1])

# get time data as single numbers i.e. 2018.0
time_headers <- colnames(data_test)[-1] # exclude row header
years <- as.integer(substr(time_headers, 1, 4))
months <- as.integer(substr(time_headers, 6, 7))
dates_test <- years + (months - 1)/12

time_headers <- colnames(data)[-1] # exclude row header
years <- as.integer(substr(time_headers, 1, 4))
months <- as.integer(substr(time_headers, 6, 7))
dates <- years + (months - 1)/12

# design matrix
x <- cbind(1, dates)


### Part 4
# Exercise 4.1: provide L and f(0)

f <- function(j) rbind(1, j)
f_0 <- f(0)

L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)

print(L)
print(f(0))


# Exercise 4.2: provide F1 and h1

i <- 1 
F_N <-  1 * f(0)%*%t(f(0)) # N=1 ; lambda^0=1
h_N <-  1 * f(0) * y[i]

print(F_N) #F_1
print(h_N) #h_1


# Exercise 4.3: update FN and hN recursively and provide F10 and h10

lambda = 0.9
Linv <- solve(L)

for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
}

(F_10 <- F_N)
(h_10 <- h_N)


# Exercise 4.4: update the model recursively up to F59 and h59
# while also calculating predictions at each step

df <- data.frame(date=x[,2],
               value=y[1:59])

# initialized values
lambda = 0.9
F_N <- F_10
h_N <- h_10


# recursively update to F59 and h59
for (i in 11:58){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  
  # estimate the parameters now
  theta_N <- solve(F_N)%*%h_N
  
  # calculate predictions
  yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N
  
  if (i %in% c(11, 20, 30, 40,47)){
    plot_N <- ggplot(df[1:59,], aes(x=date, y=value)) +
      geom_point() + 
      
      geom_line(data=df[1:i+11,], aes(y=yhat_N[1:i+11], col="twelve months prediction")) +
      geom_point(data=df[i+12,], aes(y=yhat_N[i+12], col="twelve months prediction"), size=4) +
      
      geom_line(data=df[1:i+5,], aes(y=yhat_N[1:i+5], col="six months prediction")) +
      geom_point(data=df[i+6,], aes(y=yhat_N[i+6], col="six months prediction"), size=4) +
      
      geom_point(data=df[1:i,], aes(col="one month prediction")) + 
      geom_line(data=df[1:i,], aes(y=yhat_N[1:i], col="one month prediction")) +
      geom_point(data=df[i+1,], aes(y=yhat_N[i+1], col="one month prediction"), size=4) +
      
      theme_minimal() +
      labs(x = "date", y = "number of vehicles") +
      scale_color_manual(values = c("blue", "red", "green"), 
                         labels = c("one month prediction", "six months prediction", "twelve months prediction")) +
      theme(legend.position = c(0.8, 0.3),
            legend.text = element_text(size = 5),
            legend.title = element_text(size = 5)) +
      xlim(2018, 2023) +
      ggtitle(paste0("N = ", i)) 
    
    print(plot_N)
  }
}


# Exercise 4.5: plot the resulting 1-month, 6-month and 12-month prediction

# initialized values
lambda = 0.9
F_N <- F_10
h_N <- h_10

# want to save predictions in dataframe
data_prediction <- data.frame(month1= rep(NA, 59+12),month6= rep(NA, 59+12),month12= rep(NA, 59+12))

# update iteratively
for (i in 11:59){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
  
  # calculate and store the predicted value
  data_prediction$month1[i+1] <- t(f(1))%*%theta_N
  data_prediction$month6[i+6] <- t(f(6))%*%theta_N
  data_prediction$month12[i+12] <- t(f(12))%*%theta_N
}

X_1 <- df[,1][11:length(df[,1])]+1/12
X_6 <- df[,1][11:length(df[,1])]+1/2
X_12 <- df[,1][11:length(df[,1])]+1
pre_1 <- data_prediction$month1[12:60]
pre_6 <- data_prediction$month6[17:65]
pre_12 <- data_prediction$month12[23:71]


ggplot() +
  geom_point(aes(x = X_1, y = pre_1, col="one month prediction"), size=3) +
  geom_point(aes(x = X_6, y = pre_6, col="six months prediction"), size=3) +
  geom_point(aes(x = X_12, y = pre_12, col="twelve months prediction"), size=3) +
  geom_point(aes(x = df[,1], y = df[,2]), color = "black") + # True values, trimmed to the length of predictions
  scale_color_manual(values = c("one month prediction" = "blue", 
                                "six months prediction" = "red",
                                "twelve months prediction" = "green"))+
  theme_minimal() +
  labs(x = "Time", y = "Number of Vehicles in total ") +
  theme(legend.position = c(0.8, 0.4))



# Exercise 4.6: repeat the iterative predictions for λ = 0.55,0.56,0.57,...,0.95

# Define lambda values
lambda_values <- seq(0.55, 0.95, by = 0.01)

# Initialize dataframe to store RMSE values
rmse_df_1 <- data.frame(value_1 = numeric())
rmse_df_6 <- data.frame(value_6 = numeric())
rmse_df_12 <- data.frame(value_12 = numeric())

for (lambda in lambda_values) {
  
  # initialize the updating values
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
    
    # calculate the predicted value
    yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N
    
    # One month ahead prediction
    if (i+1<=59){  # Ensure not to exceed the length of the dataset
      df$onestep_lamb_09[i+1] <- yhat_N[i+1]
    }
    
    # Six months ahead prediction
    if (i + 6 <= 59) {  # Ensure not to exceed the length of the dataset
      df$sixstep_lamb_09[i+6] <- yhat_N[i+6]
    }
    
    # Twelve months ahead prediction
    if (i + 12 <= 59) {  # Ensure not to exceed the length of the dataset
      df$twelvestep_lamb_09[i+12] <- yhat_N[i+12]
    }
  }
  
  # prediction errors for each forecast horizon
  one_step_errors <- df$value[12:59] - df$onestep_lamb_09[12:59]
  one_step_rms <- sqrt(mean(one_step_errors^2))
  
  six_step_errors <- df$value[17:59] - df$sixstep_lamb_09[17:59]
  six_step_rms <- sqrt(mean(six_step_errors^2))
  
  twelve_step_errors <- df$value[23:59] - df$twelvestep_lamb_09[23:59]
  twelve_step_rms <- sqrt(mean(twelve_step_errors^2))
  
  # root-mean-square of the prediction errors
  rmse_df_1 <- rbind(rmse_df_1, data.frame(value_1 = one_step_rms))
  rmse_df_6 <- rbind(rmse_df_6, data.frame(value_6 = six_step_rms))
  rmse_df_12 <- rbind(rmse_df_12, data.frame(value_12 = twelve_step_rms))
}


# plot 
rmse_total <- data.frame(lambda = lambda_values,rmse_1=rmse_df_1,rmse_6=rmse_df_6,rmse_12=rmse_df_12)  

ggplot(rmse_total, aes(x = lambda)) +
  geom_point(aes(y = value_1, color = "value_1")) +  # Points for y1
  geom_point(aes(y = value_6, color = "value_6")) +  # Points for y6
  geom_point(aes(y = value_12, color = "value_12")) +  # Points for y12
  scale_color_manual(values = c("value_1" = "blue", "value_6" = "red", "value_12" = "green"), 
                     labels = c("value_1" = "RMSE 1", "value_6" = "RMSE 6", "value_12" = "RMSE 12")) +
  labs(color = "") +
  xlab("Lambda value") +   # Set x-axis label
  ylab("RMSE") +   # Set y-axis label
  theme(legend.position = "bottom")  # Adjust legend position



# Exercise 4.7: Which value of λ is optimal for predicting 1 month ahead?

ggplot(rmse_total, aes(x = lambda)) +
  geom_point(aes(y = value_1, color = "value_1")) +  # Points for y1
  scale_color_manual(values = c("value_1" = "blue"), 
                     labels = c("RMSE 1")) +
  labs(color = "") +
  xlab("Lambda value") +   # Set x-axis label
  ylab("RMSE") +   # Set y-axis label
  theme(legend.position = "bottom")  # Adjust legend position



# Exercise 4.8: Which value of λ is optimal for predicting 6 months ahead?

ggplot(rmse_total, aes(x = lambda)) +
  geom_point(aes(y = value_6, color = "value_6")) +  # Points for y6
  scale_color_manual(values = c("value_6" = "red"), 
                     labels = c("RMSE 6")) +
  labs(color = "") +
  xlab("Lambda value") +   # Set x-axis label
  ylab("RMSE") +   # Set y-axis label
  theme(legend.position = "bottom")  # Adjust legend position



# Exercise 4.9: Which value of λ is optimal for predicting 12 months ahead?

ggplot(rmse_total, aes(x = lambda)) +
  geom_point(aes(y = value_12, color = "value_12")) +  # Points for y12
  scale_color_manual(values = c("value_12" = "green"), 
                     labels = c("RMSE 12")) +
  labs(color = "") +
  xlab("Lambda value") +   # Set x-axis label
  ylab("RMSE") +   # Set y-axis label
  theme(legend.position = "bottom")  # Adjust legend position




# Exercise 4.10: It would be problematic to make λ as small as 0.5. Why is that? 

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


(T <- sum(lambda_n))



# Exercise 4.11: Compare the 1-month-ahead prediction with λ = 0.55 to the naive persistence model

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
(one_step_rms_055 <- sqrt(mean(one_step_errors_055^2)))

naive_model_err <- df$value[12:59] - df$naive_model[12:59]
(naive_model_rms <- sqrt(mean(naive_model_err^2)))


ggplot() +
  geom_point(data = df, aes(x = date, y = value, color = "Original Data")) +
  geom_point(data = df, aes(x = date, y = onestep_lamb_055, color = "One Step Prediction")) +
  geom_point(data = df, aes(x = date, y = naive_model, color = "Naive Model")) +
  labs(color = "") +
  xlab("Date") +
  ylab("Value") +
  ggtitle("1-month-ahead prediction VS naive persistence model") +
  scale_color_manual(values = c("Original Data" = "black", "One Step Prediction" = "blue", "Naive Model" = "purple"))+
  theme(legend.position = "bottom")





# Exercise 4.12: Choose the best forecasts 
# and plot them together with the training data AND the test data 

# Test data
test_set <- data.frame(date=dates_test,value=y_test)

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
  scale_color_manual(values = c("future time: t = 60" = "blue", 
                                "future time: t = 65" = "red",
                                "future time: t = 71" = "green"))+
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
(pre_value_1 <- t(f(1))%*%theta_N)
(real_1 <- test_set[1,2])


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

(pre_value_6 <- t(f(6))%*%theta_N)
(pre_value_12 <- t(f(12))%*%theta_N)

(real_6 <- test_set[6,2])
(real_12 <- test_set[12,2])

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
  scale_color_manual(values = c("training set" = "black", "testing set" = "yellow", "t=60" = "blue", "t=65" = "red", "t=71" = "green"))+
  theme(legend.position = "bottom")

