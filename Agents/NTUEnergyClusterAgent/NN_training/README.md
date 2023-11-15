# Training a Neural Network for Power System Analysis

This repository contains a Python script that trains a neural network model to predict voltage magnitudes and phase angles in a power system based on power injections. The model is trained using the Keras API of TensorFlow.

## Data Preparation

The script begins by loading training and testing data from Excel files. The training data consists of power injections (both active and reactive power) and the corresponding target voltage magnitudes and phase angles. The testing data is structured similarly.

The power injections are then concatenated to form the input features (`X_train`), and the voltage magnitudes and phase angles are concatenated to form the target variables (`y_train`).

## Model Definition and Training

The model is a simple feed-forward neural network with two layers. The first layer is a dense layer with 20 neurons and a ReLU activation function. The second layer is also a dense layer, serving as the output layer of the network. The number of neurons in the output layer is equal to the number of output variables.

The model is compiled with the Adam optimizer and the Mean Squared Error (MSE) loss function. It is then trained on the training data for 50 epochs, with a batch size of 64. During training, 20% of the training data is set aside for validation.

## Model Evaluation and Usage

After training, the model is saved for future use. It is then used to predict the voltage magnitudes and phase angles for the testing data. The accuracy of the predictions is visualized by plotting the predicted and target voltage magnitudes.

## Clustering of Predictions

The script also performs k-means clustering on the predicted voltage magnitudes. The number of clusters is set to 3. The clustering results are visualized in a scatter plot, with different colors representing different clusters. The k-means model is also saved for future use.

## Visualization

The script includes code to plot the accuracy of the model’s predictions and the results of the k-means clustering. The accuracy plot shows the target and predicted voltage magnitudes for the testing data. The clustering plot shows the predicted voltage magnitudes grouped into clusters.

## Requirements

To run this script, you will need Python and the following libraries: pandas, numpy, joblib, scikit-learn, TensorFlow, and matplotlib. You will also need the training and testing data in Excel format.
