from pesfit.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pesfit.kgoperations.querykg import querykg
from pesfit.kgoperations.querytemplates import ontopesscan_data_query, \
                                               ontocompchem_data_query
import pesfit.unitconverter.unitconverter as uc
import os

def get_kg_data(opesIRI):
    ScfE = []
    R = []
    pesdata = get_ontopesscan_data(opesIRI) 
    for x in range(len(pesdata)):
        data = pesdata[x]
        ocdata = get_ontocompchem_data(data['oc_IRIs'])
        write_xyz_input(x, ocdata)
        ocdata = ocdata [0]
        ScfE.append(float(ocdata['ScfElecValue'])*uc.convertEnergyMoleUnitsToDLPunit(ocdata['ScfElecUnit']))
        R.append(float(data['scan_coord_value'])*uc.convertLengthUnitsToDLPunit(data['scan_coord_unit']))
    ScfE = get_en_states(ScfE)
    write_csv_input(ScfE, R)

def get_ontopesscan_data(opesIRI):
    pesscan_sparqltemplate = ontopesscan_data_query(opesIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontopesscan']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=pesscan_sparqltemplate)
    return data

def get_ontocompchem_data(ocIRI):
    compchem_sparqltemplate = ontocompchem_data_query(ocIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontocompchem']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=compchem_sparqltemplate)
    return data

def write_xyz_input(x, ocdata):
        with open('scan' + str(x+1) + '.xyz', 'w') as f:
            f.write(str(len(ocdata)) + "\n")
            f.close()
        # convert data in xyz format (geometries)
        for i in range(len(ocdata)):
            data = ocdata[i]
            AtomElem = data['elem'].split('#', 1)
            AtomElem = AtomElem[1]
            with open('scan' + str(x+1) + '.xyz', 'a') as f:
                f.write("\n" + AtomElem + "\t" + data['XCoordValue'] + "\t" + data['YCoordValue'] + "\t" + data['ZCoordValue'])
                f.close()

def write_csv_input(ScfE, R):
    with open('scan.csv', 'w') as f:
        f.write('R, Eg')
        for i in range(len(R)):
            f.write("\n" + str(R[i]) + "," + str(ScfE[i]) )
        f.close()

def get_en_states(ScfE):
    state1 = ScfE.index(min(ScfE))
    ScfE[:] = [ item - min(ScfE) for item in ScfE]
    return ScfE