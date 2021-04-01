import pandas as pd 
import numpy as np 
from tqdm import tqdm
import time 
import uuid
from EntityRDFizer import * 
import os 
import uuid 
 
pipelines = pd.read_csv('pipeline_split.csv').to_numpy()[:,:]

abox_header = np.array([['Source','Type','Target','Relation','Value']]) 
abox_full = np.array([['Source','Type','Target','Relation','Value']])
objectids_full = pipelines[:,2].astype(str)
objectids,unique_index = np.unique(objectids_full,return_index=True)
sort_index = np.argsort(unique_index)
unique_index = unique_index[sort_index]
objectids = objectids[sort_index]
count = 0 
for i in tqdm(objectids):
    owl_file = open('pipeline_abox/'+str(i)+'.owl','r')
    if count == 0:
        contents = owl_file.readlines() 
        contents = contents[1:]
        contents = contents[:-1]
        contents = "".join(contents)
        
    else:
        contents = owl_file.readlines() 
        owl_file.close()
        contents = contents[14:]
        if count != len(objectids)-1:
            contents = contents[:-1]
        contents = "".join(contents)
    count += 1

    owl_file = open('pipeline_abox/full_pipelines.owl','a')
    owl_file.write('\n'+contents)
    owl_file.close() 



