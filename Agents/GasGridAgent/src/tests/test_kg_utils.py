import os
import time
import pytest
from pathlib import Path
from configobj import ConfigObj
from testcontainers.core.container import DockerContainer

from py4jps.resources import JpsBaseLib

# Import module under test from ukgasflows
import gasgridagent.kg_utils as utils
import gasgridagent.input_flow_data as term_in

# Define expected gas terminals and respective IRIs (as defined in components_abox)
expected_terminals = \
{'Bacton IPs Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/BactonIPsTerminal',
 'Bacton UKCS Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/BactonUKCSTerminal',
 'Barrow Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/BarrowTerminal',
 'Easington Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/EasingtonTerminal',
 'Isle of Grain Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/IsleofGrainTerminal',
 'Milford Haven Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/MilfordHavenTerminal',
 'St Fergus Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/StFergusTerminal',
 'Teesside Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/TeessideTerminal',
 'Theddlethorpe Terminal': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/TheddlethorpeTerminal'}


@pytest.fixture()
def initialise_triple_store():
    # Define temporary Docker container based on empty Blazegraph image from CMCL registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999)
    yield blazegraph


def test_read_properties_file(tmp_path):
    # Create empty test properties file
    p = os.path.join(tmp_path, "test_gasgridagent.properties")

    # Test for 'output.directory'
    # 1) Exception for missing key
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'Key "output.directory" is missing in properties file: '
    assert expected in str(excinfo.value)
    # 2) Exception for available key, but missing value
    with open(p, 'w') as f:
        f.write("output.directory = ")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'No "output.directory" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for 'sparql.query.endpoint'
    # 1) Exception for missing key
    with open(p, 'w') as f:
        f.write("output.directory = test_outdir\n")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'Key "sparql.query.endpoint" is missing in properties file: '
    assert expected in str(excinfo.value)
    # 2) Exception for available key, but missing value
    with open(p, 'a') as f:
        f.write("sparql.query.endpoint = ")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'No "sparql.query.endpoint" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for 'sparql.update.endpoint'
    # 1) Exception for missing key
    with open(p, 'w') as f:
        f.write("output.directory = test_outdir\n\
                sparql.query.endpoint = test_query_endpoint\n")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'Key "sparql.update.endpoint" is missing in properties file: '
    assert expected in str(excinfo.value)
    # 2) Exception for available key, but missing value
    with open(p, 'a') as f:
        f.write("sparql.update.endpoint = ")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'No "sparql.update.endpoint" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test correct reading of timeseries.properties file
    with open(p, 'w') as f:
        f.write("output.directory = test_outdir\n\
                sparql.query.endpoint = test_query_endpoint\n\
                sparql.update.endpoint = test_update_endpoint")
    utils.read_properties_file(p)
    assert utils.OUTPUT_DIR == 'test_outdir'
    assert utils.QUERY_ENDPOINT == 'test_query_endpoint'
    assert utils.UPDATE_ENDPOINT == 'test_update_endpoint'


def test_create_sparql_prefix():
    # Check for proper exception for not defined prefixes
    test_abbreviation = 'test'
    with pytest.raises(KeyError) as exc_info:
        # Check correct exception type
        utils.create_sparql_prefix(test_abbreviation)
    # Check correct exception message
    assert 'Prefix: "test" has not been specified' in str(exc_info.value)

    # Check for correct creation of defined prefixes
    test_abbreviation = 'comp'
    test_prefix = utils.create_sparql_prefix(test_abbreviation)
    assert test_prefix == \
           'PREFIX comp: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#> '


def test_get_instantiated_terminals(initialise_triple_store):

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint (replace potentially returned erroneous 'localnpipe' with 'localhost')
        endpoint = get_endpoint(container)

        # Verify that knowledge base is empty
        res = utils.get_instantiated_terminals(endpoint)
        assert len(res) == 0

        # Populate triple store with ABox RDF data
        load_abox_data(endpoint)

        # Verify that instantiated terminals are retrieved correctly
        res = utils.get_instantiated_terminals(endpoint)
        assert len(res) == len(expected_terminals)

    # Verify that all retrieved terminals are expected
    for k, v in res.items():
        assert k in expected_terminals.keys()
        assert v in expected_terminals.values()


def test_get_instantiated_gas_amounts(initialise_triple_store):

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint (replace potentially returned erroneous 'localnpipe' with 'localhost')
        endpoint = get_endpoint(container)

        # Verify that knowledge base is empty
        res = utils.get_instantiated_gas_amounts(endpoint)
        assert len(res) == 0

        # Populate triple store with ABox RDF data
        load_abox_data(endpoint)

        # Verify that knowledge base is still empty
        res = utils.get_instantiated_gas_amounts(endpoint)
        assert len(res) == 0

        # Instantiate time series associations in knowledge base (neglect corresponding postgres instantiation)
        for iri in expected_terminals.values():
            try:
                term_in.instantiate_timeseries(endpoint, endpoint, iri)
            except Exception:
                # Just catch and suppress exception due to unreachable postgres database
                pass

        # Verify that instantiated terminals are retrieved correctly
        res = utils.get_instantiated_gas_amounts(endpoint)
        assert len(res) == len(expected_terminals)


def test_get_instantiated_quantities(initialise_triple_store):

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint (replace potentially returned erroneous 'localnpipe' with 'localhost')
        endpoint = get_endpoint(container)

        # Verify that knowledge base is empty
        res = utils.get_instantiated_quantities(endpoint)
        assert len(res) == 0

        # Populate triple store with ABox RDF data
        load_abox_data(endpoint)

        # Verify that knowledge base is still empty
        res = utils.get_instantiated_quantities(endpoint)
        assert len(res) == 0

        # Instantiate time series associations in knowledge base (neglect corresponding postgres instantiation)
        for iri in expected_terminals.values():
            try:
                term_in.instantiate_timeseries(endpoint, endpoint, iri)
            except Exception:
                # Just catch and suppress exception due to unreachable postgres database
                pass

        # Verify that instantiated terminals are retrieved correctly
        res = utils.get_instantiated_quantities(endpoint)
        assert len(res) == len(expected_terminals)


def test_get_instantiated_measurements(initialise_triple_store):

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint (replace potentially returned erroneous 'localnpipe' with 'localhost')
        endpoint = get_endpoint(container)

        # Verify that knowledge base is empty
        res = utils.get_instantiated_measurements(endpoint)
        assert len(res) == 0

        # Populate triple store with ABox RDF data
        load_abox_data(endpoint)

        # Verify that knowledge base is still empty
        res = utils.get_instantiated_measurements(endpoint)
        assert len(res) == 0

        # Instantiate time series associations in knowledge base (neglect corresponding postgres instantiation)
        for iri in expected_terminals.values():
            try:
                term_in.instantiate_timeseries(endpoint, endpoint, iri)
            except Exception:
                # Just catch and suppress exception due to unreachable postgres database
                pass

        # Verify that instantiated terminals are retrieved correctly
        res = utils.get_instantiated_measurements(endpoint)
        assert len(res) == len(expected_terminals)


def test_get_time_format(initialise_triple_store):

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint (replace potentially returned erroneous 'localnpipe' with 'localhost')
        endpoint = get_endpoint(container)

        # Populate triple store with ABox RDF data
        load_abox_data(endpoint)

        # Path to timeseries.properties file
        p = os.path.abspath(os.path.join(Path(__file__).parent, "..", "resources", "timeseries.properties"))
        props = ConfigObj(p)
        # Retrieve original settings
        orig_query = props['sparql.query.endpoint']
        orig_update = props['sparql.update.endpoint']
        # "Override" properties normally read from timeseries.properties for testing
        utils.QUERY_ENDPOINT = endpoint
        props['sparql.query.endpoint'] = endpoint
        props['sparql.update.endpoint'] = endpoint
        props.write()

        for iri in expected_terminals.values():
            # Verify that no time format is yet instantiated
            assert utils.get_time_format(endpoint, iri) is None
            try:
                # Instantiate time series associations in knowledge base (neglect corresponding postgres instantiation)
                term_in.instantiate_timeseries(endpoint, endpoint, iri)
            except Exception:
                # Just catch and suppress exception due to unreachable postgres database
                pass
            # Verify the correct time format
            assert utils.get_time_format(endpoint, iri) == utils.FORMAT

        # Restore original properties file
        props = ConfigObj(p)
        props['sparql.query.endpoint'] = orig_query
        props['sparql.update.endpoint'] = orig_update
        props.write()


def test_get_measurementIRI(initialise_triple_store):

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint (replace potentially returned erroneous 'localnpipe' with 'localhost')
        endpoint = get_endpoint(container)

        # Populate triple store with ABox RDF data
        load_abox_data(endpoint)

        # Path to gasgridagent.properties file
        p = os.path.abspath(os.path.join(Path(__file__).parent, "..", "resources", "gasgridagent.properties"))
        props = ConfigObj(p)
        # Retrieve original settings
        orig_query = props['sparql.query.endpoint']
        orig_update = props['sparql.update.endpoint']
        # "Override" properties normally read from gasgridagent.properties for testing
        utils.QUERY_ENDPOINT = endpoint
        props['sparql.query.endpoint'] = endpoint
        props['sparql.update.endpoint'] = endpoint
        props.write()

        for iri in expected_terminals.values():
            # Verify that no measurement is yet instantiated
            assert utils.get_measurementIRI(endpoint, iri) is None
            try:
                # Instantiate time series associations in knowledge base (neglect corresponding postgres instantiation)
                term_in.instantiate_timeseries(endpoint, endpoint, iri)
            except Exception:
                # Just catch and suppress exception due to unreachable postgres database
                pass
            # Verify that measurement is now instantiated
            assert isinstance(utils.get_measurementIRI(endpoint, iri), str)

            # Verify exception for ambiguous instantiations
            with pytest.raises(ValueError) as excinfo:
                # Check correct exception type
                try:
                    # Instantiate 2nd time series associations in knowledge base
                    term_in.instantiate_timeseries(endpoint, endpoint, iri)
                except Exception:
                    # Just catch and suppress exception due to unreachable postgres database
                    pass
                # Verify that measurement is now instantiated
                utils.get_measurementIRI(endpoint, iri)
            # Check correct exception message
            expected = 'AMBIGUITY ERROR: Terminal connected to several gas flow time series!'
            assert expected in str(excinfo.value)

        # Restore original properties file
        props = ConfigObj(p)
        props['sparql.query.endpoint'] = orig_query
        props['sparql.update.endpoint'] = orig_update
        props.write()


def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    return endpoint


def load_abox_data(endpoint):
    # Load gas_network_components ABox into (temporary) triple store
    # Requires reachable SPARQL endpoint 'endpoint'

    # Create jpsBaseLibGateWay instance
    jpsBaseLibGW = JpsBaseLib()
    jpsBaseLibGW.launchGateway()
    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")
    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint, endpoint)

    # Read RDF/XML ABox file
    rdf_file = os.path.abspath(os.path.join(Path(__file__).parent, "components_abox.owl"))
    with open(rdf_file, 'r') as f:
        content = f.read()

    # Insert all triples from RDF/XML file to triple store
    KGClient.insert(None, content, None)
