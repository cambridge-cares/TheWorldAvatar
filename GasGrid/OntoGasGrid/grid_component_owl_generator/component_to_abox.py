
import os 
import pandas as pd 
import numpy as np 
from tqdm import tqdm
import uuid
import os.path 
import uuid
from EntityRDFizer_new import * 
import time
 

pipelines = pd.read_csv('OntoGasGrid/pipeline_owl_generator/pipeline_split.csv').to_numpy()
offtakes = pd.read_csv('OntoGasGrid/grid_component_owl_generator/grid_component_data.csv').to_numpy()

if len(offtakes[0,:]) < 8:

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
                    
    offtakes = np.concatenate((offtakes,closest_connection),axis=1)




abox = np.array([['Source','Type','Target','Relation','Value']]) 

for i in range(len(offtakes[:,0])):
    if offtakes[i,4][:3] == 'GDN' and offtakes[i,1] != '#VALUE!':
        name_add = np.array([[offtakes[i,0],'Instance','LocalDistribution','','']])
        zone_add = np.array([[offtakes[i,0],'Instance',offtakes[i,4][5:-1],'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasLocalDistributionZone','']])
        loc_add = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',offtakes[i,0],'',offtakes[i,1]+'#'+offtakes[i,2]]])
        exit_area_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitArea','Data Property',offtakes[i,0],'',offtakes[i,5]]])
        exit_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitZone','Data Property',offtakes[i,0],'',offtakes[i,6]]])
        linepack_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasLinepackZone','Data Property',offtakes[i,0],'',offtakes[i,7]]])
        abox = np.append(abox,name_add,axis=0)
        abox = np.append(abox,zone_add,axis=0)
        abox = np.append(abox,loc_add,axis=0)
        abox = np.append(abox,exit_area_add,axis=0)
        abox = np.append(abox,exit_zone_add,axis=0)
        abox = np.append(abox,linepack_zone_add,axis=0)
        nearest_connection = offtakes[i,8].upper().replace(' ','') 
        nearest_connection = nearest_connection[:-9] + nearest_connection[-9:].lower()
        closest_relation = np.array([[offtakes[i,0],'Instance','http://www.theworldavatar.com/kb/ontogasgrid/gas_network_system/'+nearest_connection,'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#isConnectedToPipeline','']])
        abox = np.append(abox,closest_relation,axis=0)
        gas_usage = np.array([[offtakes[i,0],'Instance','http://www.theworldavatar.com/kb/ontogasgrid/gas_network_system/'+offtakes[i,0]+'used','http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#isConnectedToPipeline','']])
    
    if offtakes[i,4] == 'DC':
        name_add = np.array([[offtakes[i,0],'Instance','PowerStation','','']])
        loc_add = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',offtakes[i,0],'',offtakes[i,1]+'#'+offtakes[i,2]]])
        exit_area_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitArea','Data Property',offtakes[i,0],'',offtakes[i,5]]])
        exit_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitZone','Data Property',offtakes[i,0],'',offtakes[i,6]]])
        linepack_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasLinepackZone','Data Property',offtakes[i,0],'',offtakes[i,7]]])
        abox = np.append(abox,name_add,axis=0)
        abox = np.append(abox,loc_add,axis=0)
        abox = np.append(abox,exit_area_add,axis=0)
        abox = np.append(abox,exit_zone_add,axis=0)
        abox = np.append(abox,linepack_zone_add,axis=0)
        nearest_connection = offtakes[i,8].upper().replace(' ','') 
        nearest_connection = nearest_connection[:-9] + nearest_connection[-9:].lower()
        closest_relation = np.array([[offtakes[i,0],'Instance','http://www.theworldavatar.com/kb/ontogasgrid/gas_network_system/'+nearest_connection,'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#isConnectedToPipeline','']])
        abox = np.append(abox,closest_relation,axis=0)
        
    if offtakes[i,4] == 'Industrial':
        name_add = np.array([[offtakes[i,0],'Instance','IndustrialUser','','']])
        loc_add = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',offtakes[i,0],'',offtakes[i,1]+'#'+offtakes[i,2]]])
        exit_area_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitArea','Data Property',offtakes[i,0],'',offtakes[i,5]]])
        exit_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitZone','Data Property',offtakes[i,0],'',offtakes[i,6]]])
        linepack_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasLinepackZone','Data Property',offtakes[i,0],'',offtakes[i,7]]])
        abox = np.append(abox,name_add,axis=0)
        abox = np.append(abox,loc_add,axis=0)
        abox = np.append(abox,exit_area_add,axis=0)
        abox = np.append(abox,exit_zone_add,axis=0)
        abox = np.append(abox,linepack_zone_add,axis=0)
        nearest_connection = offtakes[i,8].upper().replace(' ','') 
        nearest_connection = nearest_connection[:-9] + nearest_connection[-9:].lower()
        closest_relation = np.array([[offtakes[i,0],'Instance','http://www.theworldavatar.com/kb/ontogasgrid/gas_network_system/'+nearest_connection,'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#isConnectedToPipeline','']])
        abox = np.append(abox,closest_relation,axis=0)
    if offtakes[i,4] == 'STORAGE':
        name_add = np.array([[offtakes[i,0],'Instance','Storage','','']])
        loc_add = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',offtakes[i,0],'',offtakes[i,1]+'#'+offtakes[i,2]]])
        exit_area_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitArea','Data Property',offtakes[i,0],'',offtakes[i,5]]])
        exit_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasNTSExitZone','Data Property',offtakes[i,0],'',offtakes[i,6]]])
        linepack_zone_add = np.array([['http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#hasLinepackZone','Data Property',offtakes[i,0],'',offtakes[i,7]]])
        abox = np.append(abox,name_add,axis=0)
        abox = np.append(abox,loc_add,axis=0)
        abox = np.append(abox,exit_area_add,axis=0)
        abox = np.append(abox,exit_zone_add,axis=0)
        abox = np.append(abox,linepack_zone_add,axis=0)
        nearest_connection = offtakes[i,8].upper().replace(' ','') 
        nearest_connection = nearest_connection[:-9] + nearest_connection[-9:].lower()
        closest_relation = np.array([[offtakes[i,0],'Instance','http://www.theworldavatar.com/kb/ontogasgrid/gas_network_system/'+nearest_connection,'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#isConnectedToPipeline','']])
        abox = np.append(abox,closest_relation,axis=0)
    if offtakes[i,4] == 'Terminal':
        name_add = np.array([[offtakes[i,0],'Instance','GasTerminal','','']])
        loc_add = np.array([['http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon','Data Property',offtakes[i,0],'',offtakes[i,1]+'#'+offtakes[i,2]]])
        abox = np.append(abox,name_add,axis=0)
        abox = np.append(abox,loc_add,axis=0)
        nearest_connection = offtakes[i,8].upper().replace(' ','') 
        nearest_connection = nearest_connection[:-9] + nearest_connection[-9:].lower()
        closest_relation = np.array([[offtakes[i,0],'Instance','http://www.theworldavatar.com/kb/ontogasgrid/gas_network_system/'+nearest_connection,'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#isConnectedToPipeline','']])
        abox = np.append(abox,closest_relation,axis=0)
        
abox = pd.DataFrame(abox)


if os.path.exists('components_abox') != True:
    os.makedirs('components_abox')
csv_path = r'components_abox/components_abox.csv'
owl_path = r'components_abox/components_abox.owl'

abox.to_csv(csv_path,index=False,header=False)

convert_into_rdf(csv_path,owl_path)

add_1 = '<owl:Ontology>'
add_2 = '<owl:imports rdf:resource="http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl"/>'
add_3 = '</owl:Ontology>'

owl_file = open(owl_path,'r')
contents = owl_file.readlines() 
owl_file.close()
contents.insert(3,'xmlns:owl="http://www.w3.org/2002/07/owl#"'+'\n')
contents.insert(8,'\n'+add_1)
contents.insert(9,'\n'+add_2)
contents.insert(10,'\n'+add_3+'\n')
time.sleep(1)
owl_file = open(owl_path,'w')
contents = "".join(contents)
owl_file.write(contents)
owl_file.close() 