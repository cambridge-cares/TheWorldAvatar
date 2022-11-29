from PVLibAgent.error_handling.exceptions import KGException
from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from PVLibAgent.kg_utils.utils import create_sparql_prefix


class QueryData:

    def query_air_temperature(iri, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            '''
            Returns air temperature data IRI
            '''
            if iri == '':
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        '''SELECT ?airTemp WHERE { ?weatherStation rdf:type ontoems:ReportingStation .
                                                      ?weatherStation ontoems:reports ?parameter .
                                                      ?parameter rdf:type ontoems:AirTemperature .
                                                      ?parameter om:hasValue ?airTemp }'''

                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any air temperature data IRI!")

                # check whether temperature is outside or average temperature iri
                if len(response) > 1:
                    for d in response:
                        val = d["airTemp"]
                        if val.__contains__("out") | val.__contains__("avg") | val.__contains__("Avg") | val.__contains__(
                                "Average") | val.__contains__("average"):
                            print(val)
                            return val

                else:
                    for d in response:
                        val = d["airTemp"]
                        print(response)
                        return val
            else:
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        'SELECT ?airTemp WHERE {' + iri + '''ontoems:reports ?parameter .
                                                      ?parameter rdf:type ontoems:AirTemperature .
                                                      ?parameter om:hasValue ?airTemp }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any air temperature data IRI!")

                # check whether temperature is outside or average temperature iri
                if len(response) > 1:
                    for d in response:
                        val = d["airTemp"]
                        if val.__contains__("out") | val.__contains__("avg") | val.__contains__("Avg") | val.__contains__(
                                "Average") | val.__contains__("average"):
                            print(val)
                            return val
                else:
                    for d in response:
                        val = d["airTemp"]
                        print(response)
                        return val
        except Exception as ex:
            raise KGException("Unable to query for any air temperature data IRI!") from ex

    def query_wind_speed(iri, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            '''
               Returns air temperature data IRI
            '''
            if iri == '':
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        '''SELECT ?windSpeed WHERE { ?weatherStation rdf:type ontoems:ReportingStation .
                                                      ?weatherStation ontoems:reports ?parameter .
                                                      ?parameter rdf:type ontoems:WindSpeed .
                                                      ?parameter om:hasValue ?windSpeed }'''

                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any wind speed data IRI!")

                # check whether temperature is outside or average temperature iri
                if len(response) > 1:
                    for d in response:
                        val = d["windSpeed"]
                        if val.__contains__("out") | val.__contains__("avg") | val.__contains__("Avg") | val.__contains__(
                                "Average") | val.__contains__("average"):
                            print(val)
                            return val
                else:
                    for d in response:
                        val = d["windSpeed"]
                        print(response)
                        return val

            else:
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        'SELECT ?windSpeed WHERE {' + iri + '''ontoems:reports ?parameter .
                                                      ?parameter rdf:type ontoems:WindSpeed .
                                                      ?parameter om:hasValue ?windSpeed }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any wind speed data IRI!")

                # check whether temperature is outside or average temperature iri
                if len(response) > 1:
                    for d in response:
                        val = d["windSpeed"]
                        if val.__contains__("out") | val.__contains__("avg") | val.__contains__("Avg") | val.__contains__(
                                "Average") | val.__contains__("average"):
                            print(val)
                            return val
                else:
                    for d in response:
                        val = d["windSpeed"]
                        print(response)
                        return val
        except Exception as ex:
            raise KGException("Unable to query for any wind speed data IRI!") from ex

    def query_irradiance(iri, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            if iri == '':
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        create_sparql_prefix('saref') + \
                        create_sparql_prefix('s3n') + \
                        '''SELECT ?ghi WHERE { ?device rdf:type saref:Device .
                                              ?device rdf:type s3n:SmartSensor .
                                              ?device saref:measuresProperty ?property .
                                              ?property rdf:type om:Irradiance .
                                              ?property om:hasValue ?ghi }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any irradiance data IRI!")
                for d in response:
                    val = d["ghi"]
                    print(val)
                    return val

            else:
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        create_sparql_prefix('saref') + \
                        create_sparql_prefix('s3n') + \
                        'SELECT ?ghi WHERE {' + iri + '''saref:measuresProperty ?property .
                                                      ?property rdf:type om:Irradiance .
                                                      ?property om:hasValue ?ghi }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any irradiance data IRI!")
                for d in response:
                    val = d["ghi"]
                    print(val)
                    return val
        except Exception as ex:
            raise KGException("Unable to query for any irradiance data IRI!") from ex

    def query_global_horizontal_irradiance(iri, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            '''
               Returns data IRI
            '''
            if iri == '':
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        '''SELECT ?ghi WHERE { ?weatherStation rdf:type ontoems:ReportingStation .
                                                      ?weatherStation ontoems:reports ?parameter .
                                                      ?parameter rdf:type ontoems:GlobalHorizontalIrradiance .
                                                      ?parameter om:hasValue ?ghi }'''

                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any global horizontal irradiance data IRI!")
                for d in response:
                    val = d["ghi"]
                    print(val)
                    return val

            else:
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        'SELECT ?ghi WHERE {' + iri + '''ontoems:reports ?parameter .
                                                      ?parameter rdf:type ontoems:GlobalHorizontalIrradiance .
                                                      ?parameter om:hasValue ?ghi }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any global horizontal irradiance data IRI!")
                for d in response:
                    val = d["ghi"]
                    print(val)
                    return val
        except Exception as ex:
            raise KGException("Unable to query for any global horizontal irradiance data IRI!")

    def query_latitude(iri, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            if iri == '':
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        create_sparql_prefix('saref') + \
                        create_sparql_prefix('geo') + \
                        '''SELECT ?value WHERE {?entity geo:location ?location .
                                                      ?location geo:lat ?latValue .
                                                      ?latValue om:hasValue ?Measure .
                                                      ?Measure om:hasNumericalValue ?value }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any latitude value!")
                for d in response:
                    val = d["value"]
                    print(val)
                    return val

            else:
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        create_sparql_prefix('saref') + \
                        create_sparql_prefix('geo') + \
                        'SELECT ?value WHERE { ' + iri + ''' geo:location ?location .
                                                      ?location geo:lat ?latValue .
                                                      ?latValue om:hasValue ?Measure .
                                                      ?Measure om:hasNumericalValue ?value }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any latitude value!")
                for d in response:
                    val = d["value"]
                    print(val)
                    return val
        except Exception as ex:
            raise KGException("Unable to query for any latitude value!") from ex

    def query_longitude(iri, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            if iri == '':
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        create_sparql_prefix('saref') + \
                        create_sparql_prefix('geo') + \
                        '''SELECT ?value WHERE { ?entity geo:location ?location .
                                                      ?location geo:long ?longValue .
                                                      ?longValue om:hasValue ?Measure .
                                                      ?Measure om:hasNumericalValue ?value }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any longitude value!")
                for d in response:
                    val = d["value"]
                    print(val)
                    return val
            else:
                query = create_sparql_prefix('rdf') + \
                        create_sparql_prefix('ontoems') + \
                        create_sparql_prefix('om') + \
                        create_sparql_prefix('saref') + \
                        create_sparql_prefix('geo') + \
                        'SELECT ?value WHERE { ' + iri + ''' geo:location ?location .
                                                       ?location geo:long ?longValue .
                                                       ?longValue om:hasValue ?Measure .
                                                       ?Measure om:hasNumericalValue ?value }'''
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any longitude value!")
                for d in response:
                    val = d["value"]
                    print(val)
                    return val

        except Exception as ex:
            raise KGException("Unable to query for any longitude value!") from ex





