from unittest import installHandler
from pubchem.kgoperations.getkgdata import *
from pubchem.pug.pubchem_api import *

def query_with_inchi(inchi):
    IRI = get_iri_data(inchi)
    if IRI:
        # this needs to be implemented
        data = get_testontospecies_data(IRI)
        print(data)
        return (data, 'TWA')
    else:
        data = pubchem_api(inchi)
        print(data)
        return (data, 'PubChem')
        
if __name__== '__main__':

    for inchi in ['InChI=1S/C2H2/c1-2/h1-2H', 
                  'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                  'InChI=1S/C24H10/c1-5-15-13-16(6-2)20-11-12-22-18(8-4)14-17(7-3)21-10-9-19(15)23(20)24(21)22/h1-4,9-14H']:
        data, source = query_with_inchi(inchi)
        print(source)


