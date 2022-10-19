from unittest import installHandler
from pubchem.kgoperations.getkgdata import *
from pubchem.kgoperations.addkgdata import * 
from pubchem.pug import pug_api

def query_with_inchi(inchi):
    pug_access = pug_api()
    IRI = get_iri_data(inchi)
    if IRI:
        # main get data
        data = get_ontospecies_data(IRI)
        return (data, data['CID'],'TWA')
    else:

        data = pug_access.pug_request('InChI', inchi)
        props = pug_access.get_props(data)
        CID = pug_access.get_cid(data)
        cid = CID['cid']
        insert_ontospecies_data(str(cid), props)      
        return (props, cid, 'PubChem')

        
if __name__== '__main__':

    for inchi in ['InChI=1S/C6H6/c1-2-4-6-5-3-1/h1-6H', 
                  'InChI=1S/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                  'InChI=1S/C10H10/c1-2-8-5-6-9-4-3-7(1)10(8)9/h1-10H',
                  'InChI=1S/C6H8O6/c7-1-2(8)5-3(9)4(10)6(11)12-5/h2,5,7-10H,1H2/t2-,5+/m0/s1',
                  'InChI=1S/C9H10O3/c1-6-3-4-8(10)7(5-6)9(11)12-2/h3-5,10H,1-2H3']:
        data, CID,source = query_with_inchi(inchi)
        print(source, CID,'\n')
        print(data)
    

   # A test INSERT function
   # insert_with_inchi('inchi')


