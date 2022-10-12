from pubchem_twa import *
from pubchem.pug import pug_api
import pytest
import os
import json



def test_pug_request():
    pug_access = pug_api()
    data = pug_access.pug_request('InChI','InChI=1S/C24H10/c1-5-15-13-16(6-2)20-11-12-22-18(8-4)14-17(7-3)21-10-9-19(15)23(20)24(21)22/h1-4,9-14H')
    assert pug_access.get_cid(data)['cid'] == 11630783
    assert pug_access.get_props(data)['Molecular Formula'] == 'C24H10'
    # Test the functionality of the request with SMILES
    smiles =  pug_access.get_props(data)['Canonical SMILES']
    print('Before the call ', smiles)
    data = pug_access.pug_request('SMILES',smiles)
    print('After the call ', pug_access.get_props(data)['Canonical SMILES'])
    assert pug_access.get_cid(data)['cid'] == 11630783
    assert pug_access.get_props(data)['Molecular Formula'] == 'C24H10'

    



def test_query():
    sources = []
    for inchi in ['InChI=1S/C2H2/c1-2/h1-2H', 
                  'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                  'InChI=1S/C24H10/c1-5-15-13-16(6-2)20-11-12-22-18(8-4)14-17(7-3)21-10-9-19(15)23(20)24(21)22/h1-4,9-14H']:
        _, source = query_with_inchi(inchi)
        sources.append(source)
    assert sources[0]=='TWA'
    assert sources[1]=='TWA'
    assert sources[2]=='PubChem'

