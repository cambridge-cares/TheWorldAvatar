from PVLibAgent.error_handling.exceptions import KGException
from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import create_sparql_prefix
import logging



class QueryData:

    def query_air_temperature(iri, query_endpoint: str, update_endpoint: str):
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

    def query_wind_speed(iri, query_endpoint: str, update_endpoint: str):
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

    def query_irradiance(iri, query_endpoint: str, update_endpoint: str):
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

    def query_global_horizontal_irradiance(iri, query_endpoint: str, update_endpoint: str):
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

    def query_latitude(iri, query_endpoint: str, update_endpoint: str):
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
                logging.info(query)
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
                logging.info(query)
                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any latitude value!")
                for d in response:
                    val = d["value"]
                    print(val)
                    return val
        except Exception as ex:
            raise KGException("Unable to query for any latitude value!") from ex

    def query_longitude(iri, query_endpoint: str, update_endpoint: str):
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
                logging.info(query)
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
                logging.info(query)
                if len(response) == 0:
                    raise KGException("Unable to query for any longitude value!")
                for d in response:
                    val = d["value"]
                    print(val)
                    return val

        except Exception as ex:
            raise KGException("Unable to query for any longitude value!") from ex


    def query_direct_normal_irradiance(iri, query_endpoint: str, update_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)

            query = create_sparql_prefix('rdf') + \
                    create_sparql_prefix('ontoems') + \
                    create_sparql_prefix('om') + \
                    '''SELECT ?dni WHERE { ?weatherStation rdf:type ontoems:ReportingStation .
                                                  ?weatherStation ontoems:reports ?parameter .
                                                  ?parameter rdf:type ontoems:DirectNormalIrradiance .
                                                  ?parameter om:hasValue ?dni }'''

            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any global horizontal irradiance data IRI!")
            for d in response:
                val = d["dni"]
                print(val)
                return val

        except Exception as ex:
            raise KGException("Unable to query for any irradiance data IRI!") from ex


    def query_busnode_iri(query_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint)
            query = create_sparql_prefix('powreal') + \
                    '''SELECT ?busNode WHERE { ?busNode rdf:type powreal:BusNode }'''

            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any bus node IRI!")
            return response

        except Exception as ex:
            raise KGException("Unable to query for bus node IRI!") from ex

    def query_PV_Panels(update_endpoint: str):
        try:
            kg_client = KGClient(update_endpoint, update_endpoint)

            query = create_sparql_prefix('powreal') + \
                    create_sparql_prefix('powsys') + \
                    create_sparql_prefix('rdf') + \
                    create_sparql_prefix('rdfs') + \
                    create_sparql_prefix('ontocape') + \
                    '''SELECT ?name ?PV  WHERE { ?entity  rdf:type  powreal:BusNode .
                                           ?building powsys:hasBusNode ?entity .
                                           ?building rdfs:label ?name .
                                           ?building ontocape:contains ?PV. }'''

            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any PV panels!")
            for building in response:
                print(building["name"])
                print(building["PV"])
            return response

        except Exception as ex:
            raise KGException("Unable to query for any NTU PV IRI!") from ex
    
    def query_PV_data_iri(pv_iri, query_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, query_endpoint)

            query = create_sparql_prefix('powreal') + \
                    create_sparql_prefix('powsys') + \
                    create_sparql_prefix('rdf') + \
                    create_sparql_prefix('rdfs') + \
                    create_sparql_prefix('om') + \
                    '''SELECT ?data_iri WHERE { <''' + pv_iri + '''>  powsys:hasGeneratedPower ?gen_pow_iri .
                                           ?gen_pow_iri om:hasValue  ?data_iri .
                                           }'''

            response = kg_client.performQuery(query)
           
            return response

        except Exception as ex:
            raise KGException("Unable to query for NTU PV Area!") from ex


    def query_PV_panel_for_bus(bus_number, query_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, query_endpoint)

            query = create_sparql_prefix('powreal') + \
                    create_sparql_prefix('rdf') + \
					create_sparql_prefix('xsd') + \
                    create_sparql_prefix('powsys') + \
					create_sparql_prefix('ontocape') + \
                    '''SELECT ?PV WHERE { ?busNode rdf:type powreal:BusNode .
												?busNode ontocape:isModeledBy ?busModel.
												?busModel <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>  ?var.
												?var rdf:type <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>	 .
												?var ontocape:hasValue ?varValue .
												?varValue ontocape:numericalValue ?val .                  
                                                FILTER (!contains(str(?busNode), "GENERATOR")).
												FILTER(strdt(?val, xsd:float) = ''' + str(bus_number) + ''').
                                                ?building powsys:hasBusNode ?busNode .
                                                ?building ontocape:contains ?PV.
                                                ?PV rdf:type powreal:PhotovoltaicPanel .
                                                }'''

            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for PV panel!")
            return response[0]["PV"]

        except Exception as ex:
            raise KGException("Unable to query for any NTU PV IRI!") from ex

    def query_PV_area(pv_iri, query_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, query_endpoint)

            query = create_sparql_prefix('powreal') + \
                    create_sparql_prefix('powsys') + \
                    create_sparql_prefix('rdf') + \
                    create_sparql_prefix('rdfs') + \
                    create_sparql_prefix('ontocape') + \
                    '''SELECT ?val WHERE { <''' + pv_iri + '''>  powreal:hasPanelArea  ?area .
                                           ?area ontocape:hasValue  ?areaval .
                                           ?areaval ontocape:numericalValue ?val .}'''

            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any PV panel area!")
            
            return response[0]["val"]

        except Exception as ex:
            raise KGException("Unable to query for NTU PV Area!") from ex