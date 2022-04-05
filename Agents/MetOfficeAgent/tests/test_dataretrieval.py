###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 04 Apr 2022                           #
###############################################

import time
import pytest
from testcontainers.core.container import DockerContainer

from metoffice.datainstantiation.stations import *
from metoffice.errorhandling.exceptions import InvalidInput
from tests.utils import *

# Import module under test from gasgridagent
from metoffice.dataretrieval.stations import *


@pytest.fixture()
def initialise_triple_store():
    # Define temporary Docker container based on empty Blazegraph image from CMCL registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999)
    yield blazegraph


@pytest.mark.parametrize("center, radius, expected_msg", \
[
('12.34', None, "Circle center or radius is missing for geo:search."),
(None, 50, "Circle center or radius is missing for geo:search."),
('123,456', 50, "Circle center coordinates shall be provided as \"latitude#longitude\" in WGS84 coordinates."),
]
)
def test_get_all_metoffice_stations_exceptions(center, radius, expected_msg):

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        get_all_metoffice_stations(circle_center=center, circle_radius=radius)
        pytest.fail
    # Check correct exception message
    assert expected_msg in str(excinfo.value)


def test_get_all_metoffice_stations(initialise_triple_store):

    # Read test station data
    station_data = read_station_data()
    to_instantiate = [station_data[i] for i in station_data]

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint & create new namespace with geospatial capabilities
        endpoint = get_sparql_endpoint(container)
        create_blazegraph_namespace(endpoint)

        # Verify that knowledge base is empty
        res = get_all_metoffice_stations(query_endpoint=endpoint)
        assert len(res) == 0

        # Instantiate stations
        instantiate_stations(to_instantiate, query_endpoint=endpoint, update_endpoint=endpoint)    

        # Verify that all stations are retrieved
        res = get_all_metoffice_stations(query_endpoint=endpoint)
        assert len(res) == 3

        # Verify that only one stations is retrieved
        res = get_all_metoffice_stations(query_endpoint=endpoint,
                                          circle_center='57.5#-3.5',
                                          circle_radius='100')
        assert len(res) == 1
        assert list(res.keys())[0] == station_data['station1']['id']
