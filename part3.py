import pandas as pd 
import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm


# read the file, get x_train_date, y_train_value, x_test_date, y_test_value
training_data = pd.read_excel("DST_BIL54_train.xlsx")

number_vehicle_totally = training_data.iloc[0]
value = np.array(number_vehicle_totally)
y_train_value = value[1:]

x_train_date = []
for i in range(len(number_vehicle_totally)-1):
    x_train_date.append(2018+i/12)

testing_data = pd.read_excel("DST_BIL54_test.xlsx")
y_test_value = testing_data.iloc[0]
y_test_value = np.array(y_test_value[1:])

x_test_date=[]
for i in range(12):
    x_test_date.append(2022+(11+i)/12)


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

lambda_values = [0.9, 0.8, 0.7,0.6]  

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

