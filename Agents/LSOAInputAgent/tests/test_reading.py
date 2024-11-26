################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

import pytest
import os

from agent.datainstantiation.readings import *
from tests.conftest import *


# ------------- Test Exceptions -------------------- #

@pytest.mark.parametrize("expectedfilepath", \
[
('./data/shapes_array')
]
)
def test_shape_file_location(expectedfilepath):
   assert os.path.isfile(expectedfilepath) 

@pytest.mark.skip(reason="Only works as integration test with Blazegraph running at endpoint specified in `stack_configs_mock.py` file.\
                          Default settings in `stack_configs_mock.py` match provided `docker-compose.test.yml`")

def test_upload_elec_data_to_KG(clear_triple_store, mocker):
    # Mock call to uuid function
    mocker.patch('uuid.uuid4', side_effect=[str(1), str(2), str(3)])

    # Retrieve SPARQL endpoint from `stack_configs_mock.py` file and create namespace (if not exists)
    endpoint = QUERY_ENDPOINT
    create_blazegraph_namespace(endpoint)

    # Instantiate electricity data
    route = '/api/lsoainputagent/instantiate/electricity'
    response = create_testing_agent.get(route)
    new_stations = response.json['stations']
    assert new_stations == 3
