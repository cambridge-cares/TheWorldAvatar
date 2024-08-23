import os

import pytest

from PVLibAgent.error_handling.exceptions import KGException
from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from PVLibAgent.kg_utils.utils import create_sparql_prefix
from PVLibAgent.data_retrieval.query_data import QueryData


class TestQueryData:
    def test_query_air_temperature_fails(self):
        # test non existing sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_air_temperature('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any air temperature data IRI!' in str(excinfo.value)

        # test valid and empty sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_air_temperature('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any air temperature data IRI!' in str(excinfo.value)

    def test_query_air_temperature_successful(self):
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''INSERT DATA { <http://test_weatherStation> rdf:type ontoems:ReportingStation .
                                 <http://test_weatherStation> ontoems:reports <http://test_parameter> .
                                 <http://test_parameter> rdf:type ontoems:AirTemperature .
                                 <http://test_parameter> om:hasValue <http://test_airTemp> }'''
        kg_client.performUpdate(query)

        value = QueryData.query_air_temperature('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                                'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # clear blazegraph namespace after test
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''DELETE DATA { <http://test_weatherStation> rdf:type ontoems:ReportingStation .
                                 <http://test_weatherStation> ontoems:reports <http://test_parameter> .
                                 <http://test_parameter> rdf:type ontoems:AirTemperature .
                                 <http://test_parameter> om:hasValue <http://test_airTemp> }'''
        kg_client.performUpdate(query)
        # Check correct exception message
        assert 'http://test_airTemp' in str(value)

    def test_query_wind_speed_fails(self):
        # test non existing sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_wind_speed('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                            'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any wind speed data IRI!' in str(excinfo.value)

        # test valid and empty sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_wind_speed('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                            'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any wind speed data IRI!' in str(excinfo.value)

    def test_query_wind_speed_successful(self):
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''INSERT DATA { <http://test_weatherStation> rdf:type ontoems:ReportingStation .
                                 <http://test_weatherStation> ontoems:reports <http://test_parameter> .
                                 <http://test_parameter> rdf:type ontoems:WindSpeed .
                                 <http://test_parameter> om:hasValue <http://test_data_iri> }'''
        kg_client.performUpdate(query)

        value = QueryData.query_wind_speed('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                           'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # clear blazegraph namespace after test
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''DELETE DATA { <http://test_weatherStation> rdf:type ontoems:ReportingStation .
                                 <http://test_weatherStation> ontoems:reports <http://test_parameter> .
                                 <http://test_parameter> rdf:type ontoems:WindSpeed .
                                 <http://test_parameter> om:hasValue <http://test_data_iri> }'''
        kg_client.performUpdate(query)
        # Check correct exception message
        assert 'http://test_data_iri' in str(value)

    def test_query_irradiance_fails(self):
        # test non existing sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_irradiance('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                            'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any irradiance data IRI!' in str(excinfo.value)

        # test valid and empty sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_irradiance('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                            'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any irradiance data IRI!' in str(excinfo.value)

    def test_query_irradiance_successful(self):
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                create_sparql_prefix('s3n') + \
                '''INSERT DATA { <http://test_device> rdf:type saref:Device .
                                 <http://test_device> rdf:type s3n:SmartSensor .
                                 <http://test_device> saref:measuresProperty <http://test_property> .
                                 <http://test_property> rdf:type om:Irradiance .
                                 <http://test_property> om:hasValue <http://test_data_iri> }'''
        kg_client.performUpdate(query)

        value = QueryData.query_irradiance('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                           'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # clear blazegraph namespace after test
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                create_sparql_prefix('s3n') + \
                '''DELETE DATA { <http://test_device> rdf:type saref:Device .
                                 <http://test_device> rdf:type s3n:SmartSensor .
                                 <http://test_device> saref:measuresProperty <http://test_property> .
                                 <http://test_property> rdf:type om:Irradiance .
                                 <http://test_property> om:hasValue <http://test_data_iri> }'''
        kg_client.performUpdate(query)
        # Check correct exception message
        assert 'http://test_data_iri' in str(value)

    def test_query_global_horizontal_irradiance_fails(self):
        # test non existing sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_global_horizontal_irradiance('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                                         'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any global horizontal irradiance data IRI!' in str(excinfo.value)

        # test valid and empty sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_global_horizontal_irradiance('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                                         'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any global horizontal irradiance data IRI!' in str(excinfo.value)

    def test_query_global_horizontal_irradiance_successful(self):
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''INSERT DATA { <http://test_weatherStation> rdf:type ontoems:ReportingStation .
                                 <http://test_weatherStation> ontoems:reports <http://test_parameter> .
                                 <http://test_parameter> rdf:type ontoems:GlobalHorizontalIrradiance .
                                 <http://test_parameter> om:hasValue <http://test_data_iri> }'''
        kg_client.performUpdate(query)

        value = QueryData.query_global_horizontal_irradiance('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                                             'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # clear blazegraph namespace after test
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''DELETE DATA { <http://test_weatherStation> rdf:type ontoems:ReportingStation .
                                 <http://test_weatherStation> ontoems:reports <http://test_parameter> .
                                 <http://test_parameter> rdf:type ontoems:GlobalHorizontalIrradiance .
                                 <http://test_parameter> om:hasValue <http://test_data_iri> }'''
        kg_client.performUpdate(query)
        # Check correct exception message
        assert 'http://test_data_iri' in str(value)

    def test_query_latitude_fails(self):
        # test non existing sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_latitude('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                                         'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any latitude value!' in str(excinfo.value)

        # test valid and empty sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_latitude('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                                         'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any latitude value!' in str(excinfo.value)

    def test_query_latitude_successful(self):
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''INSERT DATA { <http://test_iri> geo:location <http://test_location> .
                                 <http://test_location> geo:lat <http://test_latValue> .
                                 <http://test_latValue> om:hasValue <http://test_Measure> .
                                 <http://test_Measure> om:hasNumericalValue '10' }'''
        kg_client.performUpdate(query)

        value = QueryData.query_latitude('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                         'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # clear blazegraph namespace after test
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''DELETE DATA { <http://test_iri> geo:location <http://test_location> .
                                 <http://test_location> geo:lat <http://test_latValue> .
                                 <http://test_latValue> om:hasValue <http://test_Measure> .
                                 <http://test_Measure> om:hasNumericalValue '10' }'''
        kg_client.performUpdate(query)
        # Check correct exception message
        assert '10' in str(value)

    def test_query_longitude_fails(self):
        # test non existing sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_longitude('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                                         'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any longitude value!' in str(excinfo.value)

        # test valid and empty sparql endpoint
        with pytest.raises(KGException) as excinfo:
            QueryData.query_longitude('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                      'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # Check correct exception message
        assert 'Unable to query for any longitude value!' in str(excinfo.value)

    def test_query_longitude_successful(self):
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''INSERT DATA { <http://test_iri> geo:location <http://test_location> .
                                 <http://test_location> geo:long <http://test_longValue> .
                                 <http://test_longValue> om:hasValue <http://test_Measure> .
                                 <http://test_Measure> om:hasNumericalValue '10' }'''
        kg_client.performUpdate(query)

        value = QueryData.query_longitude('', 'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql',
                                          'http://host.docker.internal:27149/blazegraph/namespace/kb/sparql')
        # clear blazegraph namespace after test
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ontoems') + \
                create_sparql_prefix('om') + \
                create_sparql_prefix('saref') + \
                create_sparql_prefix('geo') + \
                '''DELETE DATA { <http://test_iri> geo:location <http://test_location> .
                                 <http://test_location> geo:long <http://test_longValue> .
                                 <http://test_longValue> om:hasValue <http://test_Measure> .
                                 <http://test_Measure> om:hasNumericalValue '10' }'''
        kg_client.performUpdate(query)
        # Check correct exception message
        assert '10' in str(value)


