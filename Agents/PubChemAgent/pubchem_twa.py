from unittest import installHandler
from pubchem.kgoperations.getkgdata import *
from pubchem.kgoperations.insertkgdata import *
from pubchem.pug import pug_api

def query_with_inchi(inchi):
    pug_access = pug_api()
    IRI = get_iri_data(inchi)
    print("The IRI is:", IRI)
    if IRI:
        # main get data
        data = get_ontospecies_data(IRI)
        
        # test get data
        # data = get_test_data()
        
        # test insert data
        # data = insert_test_data()

        print(data)
        return (data, 'TWA')
    else:
        data = pug_access.pug_request('InChI', inchi)
        print(pug_access.get_props(data))
        return (data, 'PubChem')
        
if __name__== '__main__':

    for inchi in ['InChI=1/C10H15N/c1-8-5-6-10(11(3)4)9(2)7-8/h5-7H,1-4H3', 
                  'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                  'InChI=1S/C24H10/c1-5-15-13-16(6-2)20-11-12-22-18(8-4)14-17(7-3)21-10-9-19(15)23(20)24(21)22/h1-4,9-14H']:
        data, source = query_with_inchi(inchi)
        print(source)


