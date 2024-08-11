#!/usr/bin/env python
# coding: utf-8

# In[1]:


pip install pandas numpy tensorflow keras matplotlib seaborn


# In[5]:


import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
import matplotlib.pyplot as plt
import seaborn as sns

# Load data
excel_file = "C:/Users/congc/Desktop/WNV/WNV.xlsx"
data = pd.read_excel(excel_file, sheet_name=0)

# Rename columns
data.columns = ["State", "Neuroinvasive_2016", "Non_neuroinvasive_2016", "Neuroinvasive_2017", 
                "Non_neuroinvasive_2017", "Neuroinvasive_2018", "Non_neuroinvasive_2018", 
                "Neuroinvasive_2019", "Non_neuroinvasive_2019", "Neuroinvasive_2020", 
                "Non_neuroinvasive_2020", "Neuroinvasive_2021", "Non_neuroinvasive_2021", 
                "AQI_2016", "PM2.5_2016", "AQI_2017", "PM2.5_2017", "AQI_2018", "PM2.5_2018", 
                "AQI_2019", "PM2.5_2019", "AQI_2020", "PM2.5_2020", "AQI_2021", "PM2.5_2021"]

# Replace non-numeric values with NaN
data.replace('—', np.nan, inplace=True)

# Convert data to long format
data_long = pd.wide_to_long(data, 
                            stubnames=['Neuroinvasive', 'Non_neuroinvasive', 'AQI', 'PM2.5'], 
                            i='State', 
                            j='year', 
                            sep='_').reset_index()

# Convert year to numeric
data_long['year'] = data_long['year'].astype(int)

# Convert relevant columns to numeric and handle missing values
numeric_columns = ['Neuroinvasive', 'Non_neuroinvasive', 'AQI', 'PM2.5']
data_long[numeric_columns] = data_long[numeric_columns].apply(pd.to_numeric, errors='coerce')

# Handle missing values
data_long.fillna(0, inplace=True)  

# Assume 10% reduction in PM2.5
data_long['PM2.5_policy'] = data_long['PM2.5'] * 0.9

# Prepare input features (X) and target variable (y)
X = data_long[['PM2.5_policy', 'AQI', 'year']].values.astype(np.float32)
y = data_long['Neuroinvasive'].values.astype(np.float32)

# Define the deep learning model
model = Sequential([
    tf.keras.layers.Input(shape=(X.shape[1],)),
    Dense(64, activation='relu'),
    Dense(32, activation='relu'),
    Dense(1)  # Output layer for causal effect
])

model.compile(optimizer='adam', loss='mean_squared_error')

# Train the model
history = model.fit(X, y, epochs=100, batch_size=32, validation_split=0.2)

# Predict using the trained model
data_long['Neuroinvasive_policy_pred'] = model.predict(X)

# Compare actual vs predicted values using deep learning
data_long_melted = pd.melt(data_long, id_vars=['State', 'year'], value_vars=['Neuroinvasive', 'Neuroinvasive_policy_pred'],
                           var_name='Type', value_name='Count')

# Plot the results
plt.figure(figsize=(14, 8))
sns.lineplot(data=data_long_melted, x='year', y='Count', hue='Type', style='Type', markers=True, dashes=False)
plt.title('Actual vs Predicted Neuroinvasive Cases Under Policy')
plt.xlabel('Year')
plt.ylabel('Neuroinvasive Cases')
plt.legend(title='Type')
plt.show()


# In[7]:


import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from sklearn.model_selection import GridSearchCV
from scikeras.wrappers import KerasRegressor
import matplotlib.pyplot as plt
import seaborn as sns

# Load data
excel_file = "C:/Users/congc/Desktop/WNV/WNV.xlsx"
data = pd.read_excel(excel_file, sheet_name=0)

# Rename columns
data.columns = ["State", "Neuroinvasive_2016", "Non_neuroinvasive_2016", "Neuroinvasive_2017", 
                "Non_neuroinvasive_2017", "Neuroinvasive_2018", "Non_neuroinvasive_2018", 
                "Neuroinvasive_2019", "Non_neuroinvasive_2019", "Neuroinvasive_2020", 
                "Non_neuroinvasive_2020", "Neuroinvasive_2021", "Non_neuroinvasive_2021", 
                "AQI_2016", "PM2.5_2016", "AQI_2017", "PM2.5_2017", "AQI_2018", "PM2.5_2018", 
                "AQI_2019", "PM2.5_2019", "AQI_2020", "PM2.5_2020", "AQI_2021", "PM2.5_2021"]

# Replace non-numeric values with NaN
data.replace('—', np.nan, inplace=True)

# Convert data to long format
data_long = pd.wide_to_long(data, 
                            stubnames=['Neuroinvasive', 'Non_neuroinvasive', 'AQI', 'PM2.5'], 
                            i='State', 
                            j='year', 
                            sep='_').reset_index()

# Convert year to numeric
data_long['year'] = data_long['year'].astype(int)

# Convert relevant columns to numeric and handle missing values
numeric_columns = ['Neuroinvasive', 'Non_neuroinvasive', 'AQI', 'PM2.5']
data_long[numeric_columns] = data_long[numeric_columns].apply(pd.to_numeric, errors='coerce')

# Handle missing values
data_long.fillna(0, inplace=True)  

# Assume 10% reduction in PM2.5
data_long['PM2.5_policy'] = data_long['PM2.5'] * 0.9

# Prepare input features (X) and target variable (y)
X = data_long[['PM2.5_policy', 'AQI', 'year']].values.astype(np.float32)
y = data_long['Neuroinvasive'].values.astype(np.float32)

# Define the model function for KerasRegressor
def create_model(optimizer='adam', units1=64, units2=32):
    model = Sequential([
        Dense(units1, activation='relu', input_shape=(X.shape[1],)),
        Dense(units2, activation='relu'),
        Dense(1)  # Output layer for causal effect
    ])
    model.compile(optimizer=optimizer, loss='mean_squared_error')
    return model

# Wrap the Keras model for use in scikit-learn
model = KerasRegressor(model=create_model, verbose=0)

# Define the grid of hyperparameters to search
param_grid = {
    'model__optimizer': ['adam', 'rmsprop'],
    'model__units1': [32, 64, 128],
    'model__units2': [16, 32, 64],
    'batch_size': [16, 32, 64],
    'epochs': [50, 100]
}

# Use GridSearchCV to search for the best hyperparameters
grid = GridSearchCV(estimator=model, param_grid=param_grid, n_jobs=-1, cv=3)
grid_result = grid.fit(X, y)

# Print the best parameters and best score
print(f"Best: {grid_result.best_score_} using {grid_result.best_params_}")

# Train the final model using the best parameters
best_model = grid_result.best_estimator_
data_long['Neuroinvasive_policy_pred'] = best_model.predict(X)

# Compare actual vs predicted values using deep learning
data_long_melted = pd.melt(data_long, id_vars=['State', 'year'], value_vars=['Neuroinvasive', 'Neuroinvasive_policy_pred'],
                           var_name='Type', value_name='Count')

# Plot the results
plt.figure(figsize=(14, 8))
sns.lineplot(data=data_long_melted, x='year', y='Count', hue='Type', style='Type', markers=True, dashes=False)
plt.title('Actual vs Predicted Neuroinvasive Cases Under Policy')
plt.xlabel('Year')
plt.ylabel('Neuroinvasive Cases')
plt.legend(title='Type')
plt.show()


# In[ ]:




