import pandas as pd 
import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm


# exercise 1.1
training_data = pd.read_excel("DST_BIL54_train.xlsx")

number_vehicle_totally = training_data.iloc[0]
value = np.array(number_vehicle_totally)
y_train_value = value[1:]

x_train_date = []
for i in range(len(number_vehicle_totally)-1):
    x_train_date.append(2018+i/12)

x_train_date = np.array(x_train_date)
plt.figure(figsize=(10, 6))
plt.plot(x_train_date, y_train_value, marker='o', linestyle='-')
plt.title('Traning data versus years')
plt.xlabel('Date')
plt.ylabel('Number Vehicles in Total')
plt.grid(True)
plt.show()


# exercise 2.2
# we will fit a linear model: Y_t = beta_0 + beta_1 * x_t + epsilon_t
# so we have 2 parameters: beta_0 and beta_1

# Add a constant to the date variable for the intercept term
X_train_date = sm.add_constant(x_train_date)

# Fit the OLS model
model = sm.OLS(y_train_value, X_train_date)
results = model.fit()

# Get the estimated coefficients
beta_hat_0 = results.params[0]  # Estimated intercept (beta hat 0)
beta_hat_1 = results.params[1]  # Estimated slope (beta hat 1)

print("Estimated intercept (beta hat 0):", beta_hat_0)
print("Estimated slope (beta hat 1):", beta_hat_1)

# Get the predicted values from the model
y_train_predicted = results.predict(X_train_date)

plt.figure(figsize=(10, 6))
# Plot the original data and the linear regression model
plt.scatter(x_train_date, y_train_value, label='Original Data',marker='o', linestyle='-')
plt.plot(x_train_date, y_train_predicted, color='red', label='Linear Regression Model(OLS)')
plt.xlabel('date')
plt.ylabel('Number Vehicles in Total')
plt.title('Linear Regression Model(OLS)')
plt.legend()
plt.grid(True)
plt.show()

# Get the estimated standard errors of the coefficients
standard_errors = results.bse

# Print the estimated standard errors
print("Estimated Standard Errors:")
print(standard_errors)



# exercise 2.3 forecast(prediction)
testing_data = pd.read_excel("DST_BIL54_test.xlsx")
y_test_value = testing_data.iloc[0]
y_test_value = np.array(y_test_value[1:])

x_test_date=[]
for i in range(12):
    x_test_date.append(2022+(11+i)/12)
x_test_date = np.array(x_test_date)

date=[]
for i in range(12+59):
    date.append(2018+i/12)
date = np.array(date)

X_test_date = sm.add_constant(x_test_date)
# compute predictions
y_test_predicted = results.predict(X_test_date)


# Get predictions and prediction intervals
predictions = results.get_prediction(X_test_date)
prediction_intervals = predictions.summary_frame(alpha=0.05)

# Extract lower and upper prediction bounds
lower_bounds = prediction_intervals['obs_ci_lower']
upper_bounds = prediction_intervals['obs_ci_upper']


upper_bounds = pd.to_numeric(upper_bounds, errors='coerce')
lower_bounds = pd.to_numeric(lower_bounds, errors='coerce')

prediction_table = pd.DataFrame({
    'Date': x_test_date,  # Assuming x_date contains the dates
    'y_test_predicted': y_test_predicted,
    'Lower_Bounds': lower_bounds,
    'Upper_Bounds': upper_bounds
})

# ex 2.4
Date = sm.add_constant(date)
Predicted = results.predict(Date)

plt.figure(figsize=(10, 6))
# Plot the training data
plt.scatter(x_train_date, y_train_value, color='blue', label='Training Data',marker='o', linestyle='-')

# Plot the testing data
plt.scatter(x_test_date, y_test_value, color='pink', label='Testing Data',marker='o', linestyle='-')

# Plot the OLS model
plt.plot(date, Predicted, color='red', label='OLS Model')

# Plot the forecasted values
plt.plot(x_test_date, y_test_predicted, color='green', label='Forecasted Values')

# Plot the prediction intervals
plt.fill_between(x_test_date, lower_bounds, upper_bounds, color='gray', alpha=0.2, label='Prediction Intervals')

# Add labels and legend
plt.xlabel('Date')
plt.ylabel('Number Vehicles in Total')
plt.title('Fitted Model and Forecasted Values with Prediction Intervals')
plt.legend()
plt.grid(True)
# Show plot
plt.show()



# ex 2.6

# Get residuals
residuals = results.resid

# Plot histogram of residuals
plt.hist(residuals, bins=10, edgecolor='black')
plt.xlabel('Residuals')
plt.ylabel('Frequency')
plt.title('Histogram of Residuals')
plt.grid(True)
plt.show()

# Plot residuals versus fitted values
plt.scatter(results.fittedvalues, residuals, color='blue')
plt.axhline(y=0, color='red', linestyle='--')  # Add horizontal line at y=0
plt.xlabel('Fitted Values')
plt.ylabel('Residuals')
plt.title('Residuals vs Fitted Values')
plt.grid(True)
plt.show()



# ex 3.2
# Calculate weights based on decay factor lambda = 0.9
lambda_value = 0.9
weights = np.array([lambda_value ** i for i in range(len(y_train_value))])

# Reverse the order of weights and time points
r_weights = weights[::-1]

# Plot
plt.figure(figsize=(10, 6))
plt.plot(x_train_date, r_weights, marker='o')
plt.xlabel('Time')
plt.ylabel('Weight')
plt.title('Decay of Weights Over Time')
plt.grid(True)
plt.show()


# ex 3.2
sum_lambda = sum(weights)

# ex 3.4
# estimated beta_0 and beta_1 with lambda = 0.9

# Add constant to independent variable
X_train_date = sm.add_constant(x_train_date)

# Fit the model using WLS
model = sm.WLS(y_train_value, X_train_date, weights=weights)
result = model.fit()

# Get the estimated coefficients
beta_0 = result.params[0]  # Intercept
beta_1 = result.params[1]  # Slope

print("Estimated coefficients:")
print("Intercept (beta_0):", beta_0)
print("Slope (beta_1):", beta_1)


# ex 3.5
# Set different lambda values
lambda_values = [0.9, 0.8, 0.7,0.6]  # Example lambda values

# Plotting
plt.figure(figsize=(10, 6))

# Plot training data
plt.plot(x_train_date, y_train_value, color='blue', label='Training Data')
plt.plot(x_test_date, y_test_value, color='black', label='Testing Data')

# Fit WLS model for each lambda and plot predicted values
for lambda_value in lambda_values:
    # Define weights
    weights = np.array([lambda_value ** i for i in range(len(x_train_date))])
    
    # Add constant to independent variable
    X_train_date = sm.add_constant(x_train_date)
    
    # Fit the model using WLS
    model = sm.WLS(y_train_value, X_train_date, weights=weights)
    result = model.fit()
    
    # Compute predicted values for training data
    y_train_predicted= result.predict(X_train_date)
    
    # Plot predicted values for training data
    plt.plot(x_train_date, y_train_predicted, label=f'Predicted Values (λ = {lambda_value})')

    # Add constant to future time points
    X_test_date = sm.add_constant(x_test_date)
    
    # Compute forecasted values for the next 12 months
    y_test_predicted = result.predict(X_test_date)
    
    # Plot forecasted values
    plt.plot(x_test_date, y_test_predicted, linestyle='dashdot', label=f'Forecasted Values (λ = {lambda_value})')

plt.xlabel('Date')
plt.ylabel('Number vehicles in Total')
plt.title('Training Data, Predicted Values, and Forecasted Values')
plt.legend()
plt.grid(True)
plt.show()

