from pubchemagent.app import *
from pubchemagent.pug import pug_api
import pytest
import os
import json


def test_pug_request():
    pug_access = pug_api()
    data = pug_access.pug_request('InChI','InChI=1S/C24H10/c1-5-15-13-16(6-2)20-11-12-22-18(8-4)14-17(7-3)21-10-9-19(15)23(20)24(21)22/h1-4,9-14H')
    cid, comp_props, identifiers = pug_access.get_props(data)
    assert cid == 11630783
    for i in identifiers:
        if identifiers[i]['key'] == 'MolecularFormula':
            MF = identifiers[i]['value'] 
    assert MF == 'C24H10'
    for i in comp_props:
        if comp_props[i]['key'] == 'MolecularWeight':
            MF = comp_props[i]['value'] 
            MW = MF['value']
    assert MW == 298.3

def test_from_textfile():
    inchi = 'inchi string'
    a = 0
    a_stop = 1
    with open('inchi_list.txt') as f:
        while inchi:
            inchi = f.readline().replace("\"","").replace("\n", "")
            a = a+1
            if a>=a_stop:
                print('Species ' + str(a) + ': ' + inchi)
                start_time = time.time()
                try: 
                    species_instantiation(inchi)
                    print("--- Total: %s seconds ---" % (time.time() - start_time))
                except:
                    continue   

