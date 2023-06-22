import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from tensorflow.keras.models import load_model
import joblib
import logging

class ClusterModel:
    def __init__(self, NN_model_filepath, kmeans_model_filepath):
        logging.basicConfig(level=logging.DEBUG)
        # Load the models
        self.neural_net_model = load_model(NN_model_filepath)
        self.kmeans_model = joblib.load(kmeans_model_filepath)
    def run_neural_net(self, all_P_values, all_Q_values):

        # load data from KG
        # Load testing data
        #test_P_injections = pd.read_excel('TestingData_14bus.xlsx', 'Sheet1').values
        #test_Q_injections = pd.read_excel('TestingData_14bus.xlsx', 'Sheet2').values

        #X_test = np.concatenate([test_P_injections, test_Q_injections], axis=1)
        #print("shape of original test:", X_test.shape)
        X_test = np.concatenate([all_P_values, all_Q_values], axis=0).T
        #X_test = np.array([[P_value], [Q_value]])
        logging.info("shape of new test:", X_test.shape)
        # Use the neural network model to predict the voltage magnitudes and phase angles
        predicted_output = self.neural_net_model.predict(X_test)
        logging.info(predicted_output.shape)
        predicted_Vm = predicted_output[:, :14]
        predicted_Va = predicted_output[:, 14:]
        return predicted_Vm, predicted_Va

    def run_k_means(self):
        # Use the k-means model to predict the cluster each data point belongs to
        clusters = self.kmeans_model.predict(predicted_output[:, :14])
        print(clusters.shape)
        # Now you can use `predicted_output` and `clusters` for whatever you need
