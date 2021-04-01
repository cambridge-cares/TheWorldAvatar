import pandas as pd 
import numpy as np 
from tqdm import tqdm
import time 
import uuid
from EntityRDFizer import * 
import os 
import uuid 
 
pipelines = pd.read_csv(r'pipeline_split.csv').to_numpy()[:,:]

abox_header = np.array([['Source','Type','Target','Relation','Value']]) 
abox_full = np.array([['Source','Type','Target','Relation','Value']])
objectids_full = pipelines[:,2].astype(str)
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
    abox = abox_header
    name_add = np.array([[pipelines[unique_index[j],3],'Instance','GridPipeline','','']])
    object_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasObjectId','Data Property',pipelines[unique_index[j],3],'',pipelines[unique_index[j],2]]]) 
    abox = np.append(abox,name_add,axis=0)
    abox = np.append(abox,object_add,axis=0)
    for i in range(unique_index[j+1]-unique_index[j]-1):
        if i % opt_split != 0 and i != 0:
            i += unique_index[j]
            pipe_name = pipelines[i,3]
            seg_name = pipe_name+' '+str(pipelines[i,10]+1)
            seg_name_next = pipe_name+' '+str(pipelines[i+1,10]+1)
            seg_name_prev  = pipe_name+' '+str(pipelines[i,10])
            segment_add = np.array([[seg_name,'Instance','GridPipelineSegment','','']])
            subsys_add = np.array([[pipe_name,'Instance',seg_name,'http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem','']])
            start_add = np.array([[seg_name+' Start','Instance','GasPipelineStart','','']])
            end_add = np.array([[seg_name+' End','Instance','GasPipelineEnd','','']])
            tube_add = np.array([[seg_name+' Tube','Instance','GasPipelineTube','','']])
            start_part = np.array([[seg_name,'Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasStartPart','']])
            end_part = np.array([[seg_name,'Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasEndPart','']])
            tube_part = np.array([[seg_name,'Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasTubePart','']])
            connection = np.array([[seg_name+' Connection','Instance','GasPipeConnection','','']])
            start_connect = np.array([[seg_name+' End','Instance',seg_name+' Connection','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#entersPipeConnection','']])
            loc_connect = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',seg_name+' Connection','',str(pipelines[i+1,1])+'#'+str(pipelines[i+1,0])]])
            start_to_tube = np.array([[seg_name+ ' Start','Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#entersSegmentPart','']])
            tube_to_end = np.array([[seg_name+' Tube','Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasSegmentPartOutput','']])
            connection_2 = np.array([[seg_name_next+' Connection','Instance','GasPipeConnection','','']])
            end_to_connection = np.array([[seg_name_prev+' Connection','Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasPipeConnectionOutput','']]) 
            start_connect_2 = np.array([[seg_name_next+' End','Instance',seg_name_next+' Connection','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#entersPipeConnection','']])

            diam_add_1 = np.array([['diameterOf'+seg_name+'Tube','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/Diameter','','']])
            diam_add_2 = np.array([['diameterOf'+seg_name+'Tube','Instance',seg_name+' Tube','http://www.ontology-of-units-of-measure.org/resource/om-2/hasPhenomenon','']])
            diam_add_3 = np.array([['_'+str(pipelines[i,5])+'Centimetres','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/Measure','','']])
            diam_add_4 = np.array([['diameterOf'+seg_name+'Tube','Instance','_'+str(pipelines[i,5])+'Centimetres','http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue','']])
            diam_add_5 = np.array([['_'+str(pipelines[i,5])+'Centimetres','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/centimetre','http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit','']])
            diam_add_6 = np.array([['http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue','Instance','_'+str(pipelines[i,5])+'Centimetres','',pipelines[i,5]]])

            order_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasOrder','Data Property',seg_name+' Connection','',pipelines[i,10]+1]])
            abox_add = np.append(segment_add,subsys_add,axis=0)
            abox_add = np.append(abox_add,start_add,axis=0)
            abox_add = np.append(abox_add,end_add,axis=0)
            abox_add = np.append(abox_add,tube_add,axis=0)
            abox_add = np.append(abox_add,start_part,axis=0)
            abox_add = np.append(abox_add,end_part,axis=0)
            abox_add = np.append(abox_add,tube_part,axis=0)
            abox_add = np.append(abox_add,start_to_tube,axis=0)
            abox_add = np.append(abox_add,tube_to_end,axis=0)
            abox_add = np.append(abox_add,connection,axis=0)
            abox_add = np.append(abox_add,end_to_connection,axis=0)
            abox_add = np.append(abox_add,start_connect,axis=0)
            abox_add = np.append(abox_add,loc_connect,axis=0)
            abox_add = np.append(abox_add,diam_add_1,axis=0)
            abox_add = np.append(abox_add,diam_add_2,axis=0)
            abox_add = np.append(abox_add,diam_add_3,axis=0)
            abox_add = np.append(abox_add,diam_add_4,axis=0)
            abox_add = np.append(abox_add,diam_add_5,axis=0)
            abox_add = np.append(abox_add,diam_add_6,axis=0)
            abox_add = np.append(abox_add,order_add,axis=0)
            abox_split = np.append(abox_split,abox_add,axis=0)
            i -= unique_index[j]
        if i % opt_split == 0 or i == 0:
            if i != 0:
                abox = np.append(abox,abox_split,axis=0)
                i += unique_index[j]
                pipe_name = pipelines[i,3]
                seg_name = pipe_name+' '+str(pipelines[i,10]+1)
                seg_name_next = pipe_name+' '+str(pipelines[i+1,10]+1)
                seg_name_prev  = pipe_name+' '+str(pipelines[i,10])
                segment_add = np.array([[seg_name,'Instance','GridPipelineSegment','','']])
                subsys_add = np.array([[pipe_name,'Instance',seg_name,'http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem','']])
                start_add = np.array([[seg_name+' Start','Instance','GasPipelineStart','','']])
                end_add = np.array([[seg_name+' End','Instance','GasPipelineEnd','','']])
                tube_add = np.array([[seg_name+' Tube','Instance','GasPipelineTube','','']])
                start_part = np.array([[seg_name,'Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasStartPart','']])
                end_part = np.array([[seg_name,'Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasEndPart','']])
                tube_part = np.array([[seg_name,'Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasTubePart','']])
                connection = np.array([[seg_name_prev+' Connection','Instance','GasPipeConnection','','']])
                start_connect = np.array([[seg_name_prev+' Connection','Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasPipeConnectionOutput','']])
                loc_connect_2 = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',seg_name+' Connection','',str(pipelines[i+1,1])+'#'+str(pipelines[i+1,0])]])
                start_to_tube = np.array([[seg_name+ ' Start','Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#entersSegmentPart','']])
                tube_to_end = np.array([[seg_name+' Tube','Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasSegmentPartOutput','']])
                connection_2 = np.array([[seg_name+' Connection','Instance','GasPipeConnection','','']])
                start_connect_2 = np.array([[seg_name+' End','Instance',seg_name+' Connection','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#entersPipeConnection','']])
                diam_add_1 = np.array([['diameterOf'+seg_name+' Tube','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/Diameter','','']])
                diam_add_2 = np.array([['diameterOf'+seg_name+'Tube','Instance',seg_name+' Tube','http://www.ontology-of-units-of-measure.org/resource/om-2/hasPhenomenon','']])
                diam_add_3 = np.array([['_'+str(pipelines[i,5])+'Centimetres','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/Measure','','']])
                diam_add_4 = np.array([['diameterOf'+seg_name+' Tube','Instance','_'+str(pipelines[i,5])+'Centimetres','http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue','']])
                diam_add_5 = np.array([['_'+str(pipelines[i,5])+'Centimetres','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/centimetre','http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit','']])
                diam_add_6 = np.array([['http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue','Data Property','_'+str(pipelines[i,5])+'Centimetres','',pipelines[i,5]]])
                order_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasOrder','Data Property',seg_name+' Connection','',pipelines[i,10]+1]])
                abox_add = np.append(segment_add,subsys_add,axis=0)
                abox_add = np.append(abox_add,start_add,axis=0)
                abox_add = np.append(abox_add,end_add,axis=0)
                abox_add = np.append(abox_add,tube_add,axis=0)
                abox_add = np.append(abox_add,start_part,axis=0)
                abox_add = np.append(abox_add,end_part,axis=0)
                abox_add = np.append(abox_add,tube_part,axis=0)
                abox_add = np.append(abox_add,connection,axis=0)
                abox_add = np.append(abox_add,start_connect,axis=0)
                abox_add = np.append(abox_add,start_to_tube,axis=0)
                abox_add = np.append(abox_add,tube_to_end,axis=0)
                abox_add = np.append(abox_add,connection_2,axis=0)
                abox_add = np.append(abox_add,start_connect_2,axis=0)
                abox_add = np.append(abox_add,loc_connect_2,axis=0)
                abox_add = np.append(abox_add,diam_add_1,axis=0)
                abox_add = np.append(abox_add,diam_add_2,axis=0)
                abox_add = np.append(abox_add,diam_add_3,axis=0)
                abox_add = np.append(abox_add,diam_add_4,axis=0)
                abox_add = np.append(abox_add,diam_add_5,axis=0)
                abox_add = np.append(abox_add,diam_add_6,axis=0)
                abox_add = np.append(abox_add,order_add,axis=0)
                abox_split = abox_add 
                i -= unique_index[j]
            if i == 0:
                i += unique_index[j]
                pipe_name = pipelines[i,3]
                seg_name = pipe_name+' '+str(pipelines[i,10]+1)
                seg_name_next = pipe_name+' '+str(pipelines[i+1,10]+1)
                seg_name_prev  = pipe_name+' '+str(pipelines[i,10])
                segment_add = np.array([[seg_name,'Instance','GridPipelineSegment','','']])
                subsys_add = np.array([[pipe_name,'Instance',seg_name,'http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem','']])
                start_add = np.array([[seg_name+' Start','Instance','GasPipelineStart','','']])
                end_add = np.array([[seg_name+' End','Instance','GasPipelineEnd','','']])
                tube_add = np.array([[seg_name+' Tube','Instance','GasPipelineTube','','']])
                start_part = np.array([[seg_name,'Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasStartPart','']])
                end_part = np.array([[seg_name,'Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasEndPart','']])
                tube_part = np.array([[seg_name,'Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasTubePart','']])
                connection = np.array([[seg_name_prev+' Connection','Instance','GasPipeConnection','','']])
                start_connect = np.array([[seg_name_prev+' Connection','Instance',seg_name+' Start','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasPipeConnectionOutput','']])
                loc_connect_2 = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',seg_name+' Connection','',str(pipelines[i+1,1])+'#'+str(pipelines[i+1,0])]])
                start_to_tube = np.array([[seg_name+ ' Start','Instance',seg_name+' Tube','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#entersSegmentPart','']])
                tube_to_end = np.array([[seg_name+' Tube','Instance',seg_name+' End','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasSegmentPartOutput','']])
                connection_2 = np.array([[seg_name+' Connection','Instance','GasPipeConnection','','']])
                start_connect_2 = np.array([[seg_name+' End','Instance',seg_name+' Connection','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#entersPipeConnection','']])
                diam_add_1 = np.array([['diameterOf'+seg_name+' Tube','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/Diameter','','']])
                diam_add_2 = np.array([['diameterOf'+seg_name+'Tube','Instance',seg_name+' Tube','http://www.ontology-of-units-of-measure.org/resource/om-2/hasPhenomenon','']])
                diam_add_3 = np.array([['_'+str(pipelines[i,5])+'Centimetres','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/Measure','','']])
                diam_add_4 = np.array([['diameterOf'+seg_name+' Tube','Instance','_'+str(pipelines[i,5])+'Centimetres','http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue','']])
                diam_add_5 = np.array([['_'+str(pipelines[i,5])+'Centimetres','Instance','http://www.ontology-of-units-of-measure.org/resource/om-2/centimetre','http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit','']])
                diam_add_6 = np.array([['http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue','Data Property','_'+str(pipelines[i,5])+'Centimetres','',pipelines[i,5]]])
                order_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasOrder','Data Property',seg_name+' Connection','',pipelines[i,10]+1]])
                abox_add = np.append(segment_add,subsys_add,axis=0)
                abox_add = np.append(abox_add,start_add,axis=0)
                abox_add = np.append(abox_add,end_add,axis=0)
                abox_add = np.append(abox_add,tube_add,axis=0)
                abox_add = np.append(abox_add,start_part,axis=0)
                abox_add = np.append(abox_add,end_part,axis=0)
                abox_add = np.append(abox_add,tube_part,axis=0)
                abox_add = np.append(abox_add,connection,axis=0)
                abox_add = np.append(abox_add,start_connect,axis=0)
                abox_add = np.append(abox_add,start_to_tube,axis=0)
                abox_add = np.append(abox_add,tube_to_end,axis=0)
                loc_connect = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',seg_name_prev+' Connection','',str(pipelines[i,1])+'#'+str(pipelines[i,0])]])
                abox_add = np.append(abox_add,loc_connect,axis=0)
                abox_add = np.append(abox_add,connection_2,axis=0)
                abox_add = np.append(abox_add,start_connect_2,axis=0)
                abox_add = np.append(abox_add,loc_connect_2,axis=0)
                abox_add = np.append(abox_add,diam_add_1,axis=0)
                abox_add = np.append(abox_add,diam_add_2,axis=0)
                abox_add = np.append(abox_add,diam_add_3,axis=0)
                abox_add = np.append(abox_add,diam_add_4,axis=0)
                abox_add = np.append(abox_add,diam_add_5,axis=0)
                abox_add = np.append(abox_add,diam_add_6,axis=0)
                abox_add = np.append(abox_add,order_add,axis=0)
                order_add_beginning = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#hasOrder','Data Property',seg_name_prev+' Connection','',pipelines[i,8]]])
                abox_add = np.append(abox_add,order_add_beginning,axis=0)
                abox_split = abox_add 
                i -= unique_index[j]
    abox = np.append(abox,abox_split,axis=0)
    abox_csv = pd.DataFrame(abox)
    if os.path.exists('pipeline_abox') != True:
        os.makedirs('pipeline_abox')
    abox_csv.to_csv('pipeline_abox/'+str(objectids[j])+'.csv',index=False,header=False)

pipelines = pd.read_csv('pipeline_split.csv').to_numpy()[:,:]

abox_header = np.array([['Source','Type','Target','Relation','Value']]) 
abox_full = np.array([['Source','Type','Target','Relation','Value']])
objectids_full = pipelines[:,2].astype(str)
objectids,unique_index = np.unique(objectids_full,return_index=True)
sort_index = np.argsort(unique_index)
unique_index = unique_index[sort_index]
objectids = objectids[sort_index]
add_1 = '<owl:Ontology>'
add_2 = '<owl:imports rdf:resource="http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl"/>'
add_3 = '</owl:Ontology>'

for i in tqdm(objectids):
    convert_into_rdf('pipeline_abox/'+str(i))
    owl_file = open('pipeline_abox/'+str(i)+'.owl','r')
    contents = owl_file.readlines() 
    owl_file.close()
    contents.insert(3,'xmlns:owl="http://www.w3.org/2002/07/owl#"'+'\n')
    contents.insert(10,'\n'+add_1)
    contents.insert(11,'\n'+add_2)
    contents.insert(12,'\n'+add_3+'\n')
    contents = "".join(contents)

    owl_file = open('pipeline_abox/'+str(i)+'.owl','w')
    owl_file.write(contents)
    owl_file.close() 



