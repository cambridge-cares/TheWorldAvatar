import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.cluster import KMeans
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
import matplotlib.pyplot as plt

# Load training data
P_injections = pd.read_excel('TrainingData_14bus.xlsx', 'Sheet1').values
Q_injections = pd.read_excel('TrainingData_14bus.xlsx', 'Sheet2').values
V_target = pd.read_excel('TrainingData_14bus.xlsx', 'Sheet3').values
theta_target = pd.read_excel('TrainingData_14bus.xlsx', 'Sheet4').values

# Load testing data
test_P_injections = pd.read_excel('TestingData_14bus.xlsx', 'Sheet1').values
test_Q_injections = pd.read_excel('TestingData_14bus.xlsx', 'Sheet2').values
V_test = pd.read_excel('TestingData_14bus.xlsx', 'Sheet3').values
theta_test = pd.read_excel('TestingData_14bus.xlsx', 'Sheet4').values

X_train = np.concatenate([P_injections, Q_injections], axis=1)
y_train = np.concatenate([V_target, theta_target], axis=1)

# Define and compile the model
model = Sequential([
    Dense(20, input_dim=X_train.shape[1], activation='relu'),
    Dense(y_train.shape[1])
])
model.compile(optimizer='adam', loss='mean_squared_error')

# Fit the model to the training data
history = model.fit(X_train, y_train, validation_split=0.2, epochs=50, batch_size=64)

# Predict the voltage magnitudes and phase angles using the trained network
X_test = np.concatenate([test_P_injections, test_Q_injections], axis=1)
predicted_output = model.predict(X_test)

# Plot the accuracy
plt.figure()
plt.plot(V_test, 'b-', linewidth=2, label='Target')
plt.plot(predicted_output[:, :14], 'r--', linewidth=2, label='Predicted')
plt.xlabel('Sample')
plt.ylabel('Voltage Magnitude')
plt.legend()
plt.title('Accuracy - Voltage Magnitude')
plt.show()

# Perform k-means clustering
kmeans = KMeans(n_clusters=3)
clusters = kmeans.fit_predict(predicted_output[:, :14])

# Plot the clusters
plt.figure()
for i, color in zip(range(3), ['red', 'green', 'blue']):
    plt.scatter(np.where(clusters==i), predicted_output[clusters==i, :14], color=color)
plt.axhline(0.95, color='black', linestyle='--')
plt.axhline(1.05, color='black', linestyle='--')
plt.xlabel('Data Points')
plt.ylabel('Values')
plt.title('K-Means Clustering')
plt.show()
