from pesfit.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pesfit.kgoperations.querykg import querykg
from pesfit.kgoperations.querytemplates import ontopesscan_data_query, \
                                               ontocompchem_data_query
import pesfit.unitconverter.unitconverter as uc
import os
import numpy as np
from scipy.signal import argrelmin

def get_kg_data(opesIRI):
    ScfE = []
    R = []
    pesdata = get_ontopesscan_data(opesIRI) 
    pesdata = sorted(pesdata, key=lambda item: item['scan_coord_value'])
    for x in range(len(pesdata)):
        data = pesdata[x]
        ocdata = get_ontocompchem_data(data['oc_IRIs'])
        ocdata = sorted(ocdata, key=lambda item: item['atom'])
        write_xyz_input(x, ocdata)
        ocdata = ocdata [0]
        ScfE.append(float(ocdata['ScfElecValue'])*uc.convertEnergyMoleUnitsToDLPunit(ocdata['ScfElecUnit'].partition('#')[2]))
        R.append(float(data['scan_coord_value'])*uc.convertLengthUnitsToDLPunit(data['scan_coord_unit'].partition('#')[2]))
    [ScfE, state1, state2] = get_en_states(ScfE)
    write_csv_input(ScfE, R)
    return state1, state2

def get_ontopesscan_data(opesIRI):
    pesscan_sparqltemplate = ontopesscan_data_query(opesIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontopesscan']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=pesscan_sparqltemplate)
    return data

def get_ontocompchem_data(ocIRI):
    compchem_sparqltemplate = ontocompchem_data_query(ocIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontocompchemtest']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=compchem_sparqltemplate)
    return data

def write_xyz_input(x, ocdata):
        with open('scan_' + str(x+1) + '.xyz', 'w') as f:
            f.write(str(len(ocdata)) + "\n")
            f.close()
        # convert data in xyz format (geometries)
        for i in range(len(ocdata)):
            data = ocdata[i]
            AtomElem = data['elem'].split('#', 1)
            AtomElem = AtomElem[1]
            with open('scan_' + str(x+1) + '.xyz', 'a') as f:
                f.write("\n" + AtomElem + "\t" + data['XCoordValue'] + "\t" + data['YCoordValue'] + "\t" + data['ZCoordValue'])
                f.close()

def write_csv_input(ScfE, R):
    with open('scan.csv', 'w') as f:
        f.write('R, Eg')
        for i in range(len(R)):
            f.write("\n" + str(R[i]) + "," + str(ScfE[i]) )
        f.close()

def get_en_states(ScfE):
    ScfE[:] = [ item - min(ScfE) for item in ScfE]
    E=np.array(ScfE)
    lmin=argrelmin(E)
    if len(lmin[0])==1:
        state1=len(E)
        state2=lmin[0][0]+1
    elif len(lmin[0])==2:
        state1=lmin[0][1]+1
        state2=lmin[0][0]+1
    else:
        state1=len(E)
        state2=1   
    with open('scan_data.txt', 'w') as f:
        f.write('state1 ' + str(state1) + " " + "\n")
        f.write('state2 ' + str(state2) + " " )
        f.close()
    return ScfE, state1, state2