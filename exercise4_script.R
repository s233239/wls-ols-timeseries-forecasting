
# load excel read library
library(readxl)
library(ggplot2)

# clear variables
rm(list=ls())

# import Excel file into R
project_data <- read_excel('DST_BIL54_train.xlsx') # time series of 59 monthly observations 
project_data_test <- read_excel('DST_BIL54_test.xlsx') # time series of 12 extra monthly observations

# get time axis
time_headers <- colnames(project_data)[-1] # exclude row header
years <- as.integer(substr(time_headers, 1, 4))
months <- as.integer(substr(time_headers, 6, 7))
dates <- years + (months - 1)/12

# create new dataframe of the data we are interested in
data <- data.frame(
  time = dates,
  y = as.numeric(unlist(project_data[1, -1]))
)
rownames(data) <- time_headers




# Exercise 4.1: provide L and f(0)

f <- function(j) rbind(1, j)
(f_0 <- f(0))

(L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2))


# Exercise 4.2: provide F1 and h1

i <- 1 
(F_N <-  1 * f(0)%*%t(f(0))) # N=1 ; lambda^0=1
(h_N <-  1 * f(0) * data$y[i]) # N=1


# Exercise 4.3: update FN and hN recursively and provide F10 and h10

lambda = 0.9
Linv <- solve(L)

for (i in 2:10){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*data$y[i]
}

(F_10 <- F_N)
(h_10 <- h_N)


# Exercise 4.4: update the model recursively up to F59 and h59
# while also calculating predictions at each step (one, six and twelve months ahead)
# for lambda = 0.9

# want to save predictions in dataframe (for lambda = 0.9):
data$onemonth_lamb_0.9  <- NA
data$sixmonth_lamb_0.9  <- NA
data$twelvemonth_lamb_0.9  <- NA

# initialize parameters estimate at previous step
(theta_N <- solve(F_N)%*%h_N) #theta_10

# update iteratively:
for (i in 11:58){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*data$y[i]
  
  # estimate parameters from i observations
  theta_N <- solve(F_N)%*%h_N
  
  # compute predicted values based on current parameters 
  yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N
  
  # store the one-month predictions
  data$onemonth_lamb_0.9[i+1] <- yhat_N[i+1]
  
  # store the six-months prediction
  if (!(i > 59-6)){ # index out of range
    data$sixmonth_lamb_0.9[i+6] <- yhat_N[i+6]
  }
  
  # store the twelve-months prediction
  if (!(i > 59-12)){ # index out of range
    data$twelvemonth_lamb_0.9[i+12] <- yhat_N[i+12]
  }
  
  # plot some of the one-step, six-step and twelve-step iterations
  if (i %in% c(11, 20, 30, 40,47)){
    plot_N <- ggplot(data[1:59,], aes(x=time, y=y)) +
      geom_point() + 
      
      geom_line(data=data[1:i+11,], aes(y=yhat_N[1:i+11], col="twelve months prediction")) +
      geom_point(data=data[i+12,], aes(y=yhat_N[i+12], col="twelve months prediction"), size=4) +
      
      geom_line(data=data[1:i+5,], aes(y=yhat_N[1:i+5], col="six months prediction")) +
      geom_point(data=data[i+6,], aes(y=yhat_N[i+6], col="six months prediction"), size=4) +
      
      geom_point(data=data[1:i,], aes(col="one month prediction")) + 
      geom_line(data=data[1:i,], aes(y=yhat_N[1:i], col="one month prediction")) +
      geom_point(data=data[i+1,], aes(y=yhat_N[i+1], col="one month prediction"), size=4) +
      
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

# each 1/6/12-step predictions in different plots
ggplot(data[1:59,], aes(x=time, y=y)) +
  geom_point(size=1) +
  geom_point(data=data, aes(y=data$onemonth_lamb_0.9), col="blue", size=2) +
  xlim(2018, 2023) +
  labs(x = "date", y = "number of vehicles") +
  ggtitle(paste0("one-step predictions for lambda = ", lambda))

ggplot(data[1:59,], aes(x=time, y=y)) +
  geom_point(size=1) +
  geom_point(data=data, aes(y=data$sixmonth_lamb_0.9), col="red", size=2) +
  xlim(2018, 2023) +
  labs(x = "date", y = "number of vehicles") +
  ggtitle(paste0("six-step predictions for lambda = ", lambda))

ggplot(data[1:59,], aes(x=time, y=y)) +
  geom_point(size=1) +
  geom_point(data=data, aes(y=data$twelvemonth_lamb_0.9), col="green", size=2) +
  xlim(2018, 2023) +
  labs(x = "date", y = "number of vehicles") +
  ggtitle(paste0("twelve-step predictions for lambda = ", lambda))

# plot together
ggplot(data[1:59,], aes(x=time, y=y)) +
  geom_point(size=1) +
  geom_point(data=data, aes(y=data$onemonth_lamb_0.9), col="blue", size=2) +
  geom_point(data=data, aes(y=data$sixmonth_lamb_0.9), col="red", size=2) +
  geom_point(data=data, aes(y=data$twelvemonth_lamb_0.9), col="green", size=2) +
  labs(x = "date", y = "number of vehicles") +
  xlim(2018, 2023) +
  ggtitle(paste0("predictions for lambda = ", lambda))


# Exercise 4.6: repeat the iterative predictions for λ = 0.55,0.56,0.57,...,0.95

# all different value of lambda from 0.55 to 0.95, to iterate on
lambda_it <- seq(0.55, 0.95, by = 0.01)

# initialize a dataframe to store the rms
rms_onemonth <- data.frame(
  lamb = lambda_it,
  rms = NA
)
rms_sixmonth <-data.frame(
  lamb = lambda_it,
  rms = NA
)
rms_twelvemonth <- data.frame(
  lamb = lambda_it,
  rms = NA
)
  
# iterate on the values of lambda
index <- 0
for (l in lambda_it){
  # iterate
  index <- index+1
  
  # refresh updating parameters
  i <- 1 
  (F_N <-  1 * f(0)%*%t(f(0))) 
  (h_N <-  1 * f(0) * data$y[i]) 
  
  for (i in 2:10){
    F_N <- F_N + l^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
    h_N <- l * Linv %*% h_N + f(0)*data$y[i]
  }
  
  # create new columns in data frame to store the predictions on lambda
  name1 <- paste("onemonth_lamb_", sprintf("%.2f", l), sep="")
  data[[name1]] <- NA
  name6 <- paste("sixmonth_lamb_", sprintf("%.2f", l), sep="")
  data[[name6]] <- NA
  name12 <- paste("twelvemonth_lamb_", sprintf("%.2f", l), sep="")
  data[[name12]] <- NA
  
  # update iteratively for the predictions
  for (i in 11:58){
    F_N <- F_N + l^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
    h_N <- l * Linv %*% h_N + f(0)*data$y[i]
    
    theta_N <- solve(F_N)%*%h_N
    
    yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N
    
    # store the one-month predictions
    data[[name1]][i+1] <- yhat_N[i+1]
    
    # store the six-months prediction
    if (!(i > 59-6)){ # index out of range
      data[[name6]][i+6] <- yhat_N[i+6]
    }
    
    # store the twelve-months prediction
    if (!(i > 59-12)){ # index out of range
      data[[name12]][i+12] <- yhat_N[i+12]
    }
  }
  # compute the root-mean-square for each forecast horizon and lambda=l
  # rms1 <- sqrt(mean((data$y - data[[name1]])^2, na.rm = TRUE))
  # rms6 <- sqrt(mean((data$y - data[[name6]])^2, na.rm = TRUE))
  # rms12 <- sqrt(mean((data$y - data[[name12]])^2, na.rm = TRUE))
  
  rms1 <- sqrt( sum((data$y - data[[name1]])^2, na.rm = TRUE)/(59-11) )
  rms6 <- sqrt( sum((data$y - data[[name6]])^2, na.rm = TRUE)/(59-16) )
  rms12 <- sqrt( sum((data$y - data[[name12]])^2, na.rm = TRUE)/(59-22) )
  
  
  rms_onemonth$rms[index] <-  rms1
  rms_sixmonth$rms[index] <- rms6
  rms_twelvemonth$rms[index] <- rms12 
}

#  plot the root-mean-square of the prediction errors versus λ
ggplot() +
  geom_point(data=rms_onemonth, aes(x=lamb, y=rms, color="one-step")) +
  geom_point(data=rms_sixmonth, aes(x=lamb, y=rms, color="six-step")) +
  geom_point(data=rms_twelvemonth, aes(x=lamb, y=rms, color="twelve-step")) +
  xlim(0.55, 0.95) +
  labs(x = "lambda", y = "rms",
       title = "RMS vs lambda for different forecast horizon") + 
  scale_color_manual(values = c("blue", "red", "green"), 
                     labels = c("one-step", "six-step", "twelve-step")) +
  theme(legend.position = "bottom")


# Exercise 4.7: Which value of λ is optimal for predicting 1 month ahead?

ggplot() +
  geom_point(data=rms_onemonth, aes(x=lamb, y=rms), color="blue") +
  xlim(0.55, 0.95) +
  labs(x = "lambda", y = "rms",
       title = "RMS vs lambda for one-step predictions") 


# Exercise 4.8: Which value of λ is optimal for predicting 6 months ahead?

ggplot() +
  geom_point(data=rms_sixmonth, aes(x=lamb, y=rms), color="red") +
  xlim(0.55, 0.95) +
  labs(x = "lambda", y = "rms",
       title = "RMS vs lambda for six-step predictions") 


# Exercise 4.9: Which value of λ is optimal for predicting 12 months ahead?

ggplot() +
  geom_point(data=rms_twelvemonth, aes(x=lamb, y=rms), color="green") +
  xlim(0.55, 0.95) +
  labs(x = "lambda", y = "rms",
       title = "RMS vs lambda for twelve-step predictions") 


# Exercise 4.10: It would be problematic to make λ as small as 0.5. Why is that? 

for (lambda in c(0.25, 0.45, 0.50, 0.55, 0.75)){
  N <- 59
  
  diagonal_elements <- 1 / lambda^(N - (1:N))
  sigma <- diag(diagonal_elements)
  sigma_inv <- diag(1/diagonal_elements)
  
  total_memory <- sum(lambda^(0:N-1))
  p <- 2 # number of parameters in the model
  
  # iterate until reaching the parameters estimates for N=59
  F_N <- F_10
  h_N <- h_10
  
  for (i in 11:N){
    F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
    h_N <- lambda * Linv %*% h_N + f(0)*data$y[i]
  }
  
  theta_N <- solve(F_N)%*%h_N
  yhat_N <- t(f(-(i-1):(59-i)))%*%theta_N
  
  # estimating the variance
  (variance_N <- t(data$y - yhat_N) %*% sigma_inv %*% (data$y - yhat_N) / (total_memory - p))
  
  # print results
  print(lambda)
  print(variance_N)
}


