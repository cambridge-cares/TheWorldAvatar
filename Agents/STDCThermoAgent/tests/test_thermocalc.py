from stdc.app import runThermoCalculator
from stdc.flaskapp import create_app
import pytest
import os
import json

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_DIR = os.path.join(THIS_DIR,'refData')

@pytest.fixture
def client():
    app = create_app({'TESTING': True})
    with app.test_client() as client:
        yield client

@pytest.mark.parametrize("inp_file, regenerateResult", \
[
('Si_inp.json', False),
('CO2_inp.json', False),
('SiH4_inp.json', False)
]
)
def test_thermocalc(inp_file,regenerateResult,regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file)
    print()
    print()

    inp_file = os.path.join(TEST_DIR,inp_file)
    out_file = inp_file.replace('inp.json', 'out.json')

    # read the inputs from a file
    with open(inp_file, 'r') as f:
        inputs = json.load(f)

    thermoData = runThermoCalculator(inputs)

    # dump the output to a file. This makes it easier
    # to compare the results in case the test fails
    with open(out_file, 'w') as f:
        json.dump(obj=thermoData, fp=f)

    # if regenerate the test results if requested
    if regenerateResult or regenerateAllResults:
        with open(out_file+'_ref', 'w') as f:
            json.dump(thermoData, f)

    # read the ref results
    with open(out_file+'_ref', 'r') as f:
        ref_ThermoData = json.load(f)

    assert thermoData == ref_ThermoData

@pytest.mark.parametrize("ref_file, ocIRI, osIRI, regenerateResult", \
[
( 'CO2_out_web.json_ref', \
  'http://www.theworldavatar.com/kb/ontocompchem/G09_testID-111-111-111', \
  'http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000', \
   False
),
(
    'C9H20_out_web.json_ref', \
    'http://www.theworldavatar.com/kb/ontocompchem/G09_34486bee-f786-4bd2-ba1b-f7d82cadb88a', \
    'http://www.theworldavatar.com/kb/ontospecies/s00008729.owl/Species_7258559357316100', \
    False
)
]
)
def test_webapp(ref_file, ocIRI, osIRI, regenerateResult, client, regenerateAllResults=False):
    print('========================================================')

    ref_file = os.path.join(TEST_DIR,ref_file)
    out_file = ref_file.replace('_ref', '')
    route = f"/api/thermoagent/calculate?ontocompchem_IRI={ocIRI}&ontospecies_IRI={osIRI}"
    response = client.get(route)

    thermoData = response.json['result']

    # dump the output to a file. This makes it easier
    # to compare the results in case the test fails
    with open(out_file, 'w') as f:
        json.dump(obj=thermoData, fp=f)

    # if regenerate the test results if requested
    if regenerateResult or regenerateAllResults:
        with open(out_file+'_ref', 'w') as f:
            json.dump(thermoData, f)

    # read the ref results
    with open(out_file+'_ref', 'r') as f:
        ref_ThermoData = json.load(f)

    assert thermoData == ref_ThermoData