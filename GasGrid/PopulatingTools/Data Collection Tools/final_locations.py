import pandas as pd
import numpy as np 

NTS = pd.read_csv(r'PopulatingTools\Data Collection Tools\pipe_locations.csv').to_numpy()
NTS = np.append(NTS,[np.zeros(len(NTS[0,:]))],axis=0)
obj_id = NTS[0,3]

index = -1
try:
    while index < len(NTS[:,0]):
        start_index = index +1
        start = NTS[index+1,0:2]
        for i in range(len(NTS[:,0])):
            if obj_id == NTS[start_index+i,3]:
                index += 1
            else:
                break
        end = NTS[index,0:2]
        locations = np.append(start,end)
        name = NTS[index,3]
        coarse_information = [np.append(locations,name)]
        if start_index == 0:
            information_store = coarse_information
        else:
            information_store = np.append(information_store,coarse_information,axis=0)
        obj_id = NTS[index+1,3]
except:
    print(information_store)
    
information_store = pd.DataFrame(information_store)
information_store.to_csv('pipe_start_end.csv')
    