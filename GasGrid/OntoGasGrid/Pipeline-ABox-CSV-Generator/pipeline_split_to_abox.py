import pandas as pd 
import numpy as np 
from tqdm import tqdm
import uuid
 
pipelines = pd.read_csv('pipeline_split.csv').to_numpy()[:,:]

abox_full = np.array([['Source','Type','Target','Relation','Value']])
objectids_full = pipelines[:,2]
objectids,unique_index = np.unique(objectids_full,return_index=True)
sort_index = np.argsort(unique_index)
unique_index = unique_index[sort_index]
objectids = objectids[sort_index]

for i in unique_index:
    name_add = np.array([[pipelines[i,3],'Instance','GridPipeline','','']])
    object_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasObjectId','Data Property',pipelines[i,3],'',pipelines[i,2]]]) 
    abox_full = np.append(abox_full,name_add,axis=0)
    abox_full = np.append(abox_full,object_add,axis=0)

unique_index = np.append(unique_index,len(pipelines[:,0]))
abox_pipes = []
opt_split = 100 
for j in tqdm(range(len(unique_index)-1)):
    abox = np.zeros((1,5))
    for i in range(unique_index[j+1]-unique_index[j]-1):
        if i % opt_split != 0 and i != 0:
            i += unique_index[j]
            pipe_name = pipelines[i,3]
            seg_name = pipe_name+' '+str(pipelines[i,10])
            segment_add = np.array([[seg_name,'Instance','GridPipelineSegment','','']])
            subsys_add = np.array([[pipe_name,'Instance',seg_name,'http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem','']])
            start_add = np.array([[seg_name+' Start','Instance','GasPipelineStart','','']])
            end_add = np.array([[seg_name+' End','Instance','GasPipelineEnd','','']])
            tube_add = np.array([[seg_name+' Tube','Instance','GasPipelineTube','','']])
            start_part = np.array([[seg_name,'Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasStartPart','']])
            end_part = np.array([[seg_name,'Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasEndPart','']])
            tube_part = np.array([[seg_name,'Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasTubePart','']])
            lat_start = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLatitude','Data Property',seg_name+' Start','',pipelines[i,1]]])
            long_start = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLongitude','Data Property',seg_name+' Start','',pipelines[i,0]]])
            lat_end = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLatitude','Data Property',seg_name+' End','',pipelines[i+1,1]]])
            long_end = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLongitude','Data Property',seg_name+' End','',pipelines[i+1,0]]])
            diam_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasDiameter','Data Property',seg_name+' Tube','',pipelines[i,5]]])
            order_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasOrder','Data Property',seg_name,'',pipelines[i,10]]])
            abox_add = np.append(segment_add,subsys_add,axis=0)
            abox_add = np.append(abox_add,start_add,axis=0)
            abox_add = np.append(abox_add,end_add,axis=0)
            abox_add = np.append(abox_add,tube_add,axis=0)
            abox_add = np.append(abox_add,start_part,axis=0)
            abox_add = np.append(abox_add,end_part,axis=0)
            abox_add = np.append(abox_add,tube_part,axis=0)
            abox_add = np.append(abox_add,lat_start,axis=0)
            abox_add = np.append(abox_add,long_start,axis=0)
            abox_add = np.append(abox_add,lat_end,axis=0)
            abox_add = np.append(abox_add,long_end,axis=0)
            abox_add = np.append(abox_add,diam_add,axis=0)
            abox_add = np.append(abox_add,order_add,axis=0)
            abox_split = np.append(abox_split,abox_add,axis=0)
            i -= unique_index[j]
        if i % opt_split == 0 or i == 0:
            if i != 0:
                abox = np.append(abox,abox_split,axis=0)
            i += unique_index[j]
            pipe_name = pipelines[i,3]
            seg_name = pipe_name+' '+str(pipelines[i,10])
            segment_add = np.array([[seg_name,'Instance','GridPipelineSegment','','']])
            subsys_add = np.array([[pipe_name,'Instance',seg_name,'http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem','']])
            start_add = np.array([[seg_name+' Start','Instance','GasPipelineStart','','']])
            end_add = np.array([[seg_name+' End','Instance','GasPipelineEnd','','']])
            tube_add = np.array([[seg_name+' Tube','Instance','GasPipelineTube','','']])
            start_part = np.array([[seg_name,'Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasStartPart','']])
            end_part = np.array([[seg_name,'Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasEndPart','']])
            tube_part = np.array([[seg_name,'Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasTubePart','']])
            lat_start = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLatitude','Data Property',seg_name+' Start','',pipelines[i,1]]])
            long_start = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLongitude','Data Property',seg_name+' Start','',pipelines[i,0]]])
            lat_end = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLatitude','Data Property',seg_name+' End','',pipelines[i+1,1]]])
            long_end = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasLongitude','Data Property',seg_name+' End','',pipelines[i+1,0]]])
            diam_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasDiameter','Data Property',seg_name+' Tube','',pipelines[i,5]]])
            order_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasOrder','Data Property',seg_name,'',pipelines[i,10]]])
            abox_add = np.append(segment_add,subsys_add,axis=0)
            abox_add = np.append(abox_add,start_add,axis=0)
            abox_add = np.append(abox_add,end_add,axis=0)
            abox_add = np.append(abox_add,tube_add,axis=0)
            abox_add = np.append(abox_add,start_part,axis=0)
            abox_add = np.append(abox_add,end_part,axis=0)
            abox_add = np.append(abox_add,tube_part,axis=0)
            abox_add = np.append(abox_add,lat_start,axis=0)
            abox_add = np.append(abox_add,long_start,axis=0)
            abox_add = np.append(abox_add,lat_end,axis=0)
            abox_add = np.append(abox_add,long_end,axis=0)
            abox_add = np.append(abox_add,diam_add,axis=0)
            abox_add = np.append(abox_add,order_add,axis=0)
            abox_split = abox_add 
            i -= unique_index[j]
           
    abox = np.append(abox,abox_split,axis=0)
    abox = abox[1:,:]
    abox_csv = pd.DataFrame(abox)
    abox_csv.to_csv(str(objectids[j])+'.csv',index=False,header=False)
#     abox_full = np.append(abox_full,abox,axis=0)

# abox_full = pd.DataFrame(abox_full)
# abox_full.to_csv('gas_network_system_abox_test.csv',index=False,header=False)

