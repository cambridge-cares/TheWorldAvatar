from pubchemagent.app import *
from pubchemagent.pug import pug_api
from pubchemagent.flaskapp import create_app
import pytest
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_DIR = os.path.join(THIS_DIR,'refData')

@pytest.fixture
def client():
    app = create_app({'TESTING': True})
    with app.test_client() as client:
        yield client

@pytest.mark.parametrize("inchi, cid_r, MF_r, MW_r", \
[
(   'InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3', \
    702 , \
    'C2H6O', \
    46.07
),
(
    'InChI=1S/C6H6/c1-2-4-6-5-3-1/h1-6H', \
    241 , \
    'C6H6', \
    78.11
)
]
)
def test_pug_request(inchi, cid_r, MF_r, MW_r):
    print('========================================================')
    pug_access = pug_api()
    data = pug_access.pug_request('InChI', inchi)
    cid, comp_props, identifiers = pug_access.get_props(data)
    assert cid == cid_r
    for i in identifiers:
        if identifiers[i]['key'] == 'MolecularFormula':
            MF = identifiers[i]['value'] 
    for i in comp_props:
        if comp_props[i]['key'] == 'MolecularWeight':
            MW = comp_props[i]['value']['value']
    assert MF == MF_r
    assert MW == MW_r

def test_from_textfile():
    print('========================================================')
    inchi = 'inchi string'
    a = 0
    inp_file = os.path.join(TEST_DIR,'inchi_list.txt')
    with open(inp_file, 'r') as f:
        inchi = f.readlines()
        for i in inchi:
            i = i.replace("\"","").replace("\n", "")
            a = a+1
            print('Species ' + str(a) + ': ' + i)
            start_time = time.time()
            try: 
                species_instantiation(i)
                print("--- Total: %s seconds ---" % (time.time() - start_time))
            except:
                continue   

@pytest.mark.parametrize("inchi", \
[
    'InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3', \
    'InChI=1S/C6H6/c1-2-4-6-5-3-1/h1-6H'
]
)
def test_webapp(inchi, client):
    print('========================================================')

    route = f"/query/species?inchi={inchi}"
    response = client.get(route)

    print(response)