import pandas as pd 
import matplotlib.pyplot as plt 
from tqdm import tqdm 
import numpy as np 

pipelines = pd.read_csv('OntoGasGrid/pipeline_owl_generator/pipeline_split.csv').to_numpy()
offtakes = pd.read_csv('OntoGasGrid/grid_component_owl_generator/grid_component_data.csv').to_numpy()



n_offt = len(offtakes[:,0])
n_cons = len(pipelines[:,0])
closest_connection = np.zeros((n_offt,2),dtype=object)

def connection_name_get(i):
    grid_line = pipelines[i,3]
    connect_num = pipelines[i,8]
    return grid_line + ' ' + str(connect_num) + ' Connection'
   
   

for i in tqdm(range(n_offt)):
    if offtakes[i,2] != '#VALUE!':
        dist_store = []
        max_dist = 1000
        off_lat = float(offtakes[i,2])
        off_lng = float(offtakes[i,1])
        for ii in range(n_cons):
            con_lat = float(pipelines[ii,0])
            con_lng = float(pipelines[ii,1])
            dist = np.sqrt((off_lat-con_lat)**2+(off_lng-con_lng)**2)
            if dist < max_dist:
                closest_connection[i,0] = connection_name_get(ii)
                closest_connection[i,1] = pipelines[ii,2]
                max_dist = dist 
                
closest_connection = pd.DataFrame(closest_connection).to_csv('OntoGasGrid/grid_component_owl_generator/closest connection.csv',index=False,header=False)
