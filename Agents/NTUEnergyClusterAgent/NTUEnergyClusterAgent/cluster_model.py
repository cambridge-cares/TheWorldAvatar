import numpy as np
import pandas as pd
import logging

class ClusterModel:
    def __init__(self, nn_model_filepath):
        logging.basicConfig(level=logging.DEBUG)
        
        self.input_size = 6
        self.output_size = 45
        self.output_shape = (15,3)

        # Load the models
        self.W1 = pd.read_excel(nn_model_filepath, 'W1', header=None).values
        self.W2 = pd.read_excel(nn_model_filepath, 'W2', header=None).values
        self.b1 = pd.read_excel(nn_model_filepath, 'b1', header=None).values
        self.b2 = pd.read_excel(nn_model_filepath, 'b2', header=None).values

    def run_neural_net(self, input_data):

        logging.info("Running BNN")

        try:
            data = np.asarray(input_data)
            size = data.shape           
            
            if size[0] != self.input_size:
                logging.error("input size:",size[0])
                raise Exception("Input size does not match expected size")

            if size[1] != 1:
                logging.error("input length:",size[1])
                raise Exception("Input length exceeds 1")
            
            predictions = self.predict(data)

            reshaped_predications = np.reshape(predictions, self.output_shape, 'F')
            return reshaped_predications
        
        except:
            return "BNN failed"      
    
    def predict(self, input_data):
         z1 = np.add(np.matmul(self.W1, input_data), self.b1)
         a1 = self.sigmoid(z1)
         z2 = np.add(np.matmul(self.W2, a1), self.b2)
         predictions = self.sigmoid(z2)
         return predictions
    
    def sigmoid(self, x):
        s = np.reciprocal(1.0+np.exp(-1.0*x))
        return s
        