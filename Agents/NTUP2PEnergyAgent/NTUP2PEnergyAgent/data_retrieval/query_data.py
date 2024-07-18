from NTUP2PEnergyAgent.error_handling.exceptions import KGException
from NTUP2PEnergyAgent.kg_utils.kgClient import KGClient
from NTUP2PEnergyAgent.kg_utils.utils import create_sparql_prefix
import logging

class QueryData:

    def query_P_iri(busNode_iri: str, query_endpoint: str, update_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)

            query = create_sparql_prefix('rdf') + \
                    create_sparql_prefix('powbehave') + \
                    create_sparql_prefix('om') + \
                    '''SELECT ?P WHERE { <''' + busNode_iri + '''> powbehave:hasActivePowerAbsorbed ?ActivePower.
                                                ?ActivePower om:hasValue ?P. }'''

            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any Active Power IRI!")
            for d in response:
                val = d["P"]
                print(val)
                return val

        except Exception as ex:
            raise KGException("Unable to query for any Active Power IRI!") from ex

    def query_Q_iri(busNode_iri: str, query_endpoint: str, update_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)

            query = create_sparql_prefix('rdf') + \
                    create_sparql_prefix('powbehave') + \
                    create_sparql_prefix('om') + \
                    '''SELECT ?Q WHERE {  <''' + busNode_iri + '''> powbehave:hasReactivePowerAbsorbed ?ReactivePower.
                                                ?ReactivePower om:hasValue ?Q. }'''

            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any Reactive Power IRI!")
            for d in response:
                val = d["Q"]
                print(val)
                return val

        except Exception as ex:
            raise KGException("Unable to query for any Reactive Power IRI!") from ex

    def query_Vm_iri(busNode_iri: str, query_endpoint: str, update_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            query = create_sparql_prefix('rdf') + \
                    create_sparql_prefix('powbehave') + \
                    create_sparql_prefix('om') + \
                    '''SELECT ?Vm WHERE {  <''' + busNode_iri + '''> powbehave:hasVoltageMagnitude ?VoltageMagnitude.
                                                ?VoltageMagnitude om:hasValue ?Vm. }'''
            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any Voltage Magnitude IRI!")
            for d in response:
                val = d["Vm"]
                print(val)
                return val

        except Exception as ex:
            raise KGException("Unable to query for any Voltage Magnitude IRI!") from ex

    def query_Va_iri(busNode_iri: str, query_endpoint: str, update_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            query = create_sparql_prefix('rdf') + \
                    create_sparql_prefix('powbehave') + \
                    create_sparql_prefix('om') + \
                    '''SELECT ?Va WHERE {  <''' + busNode_iri + '''> powbehave:hasVoltageAngle ?VoltageAngle.
                                                ?VoltageAngle om:hasValue ?Va. }'''
            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any Voltage Magnitude IRI!")
            for d in response:
                val = d["Va"]
                print(val)
                return val

        except Exception as ex:
            raise KGException("Unable to query for any Voltage Magnitude IRI!") from ex

    def query_busnode_iris(query_endpoint: str, update_endpoint: str, bus_number: float):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            			
            query = create_sparql_prefix('powreal') + \
                    create_sparql_prefix('rdf') + \
					create_sparql_prefix('xsd') + \
					create_sparql_prefix('ontocape') + \
                    '''SELECT ?busNode WHERE { ?busNode rdf:type powreal:BusNode .
												?busNode <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy> ?busModel.
												?busModel <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>  ?var.
												?var rdf:type <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>	 .
												?var ontocape:hasValue ?varValue .
												?varValue ontocape:numericalValue ?val .                  
                                                FILTER (!contains(str(?busNode), "GENERATOR")).
												FILTER(strdt(?val, xsd:float) = ''' + str(bus_number) + ''')}'''
            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any bus node IRI!")
            
            return response

        except Exception as ex:
            raise KGException("Unable to query for bus node IRI!") from ex

    def query_PV_generated_power_for_bus(bus_iri, query_endpoint: str):
            try:
                kg_client = KGClient(query_endpoint, query_endpoint)

                query = create_sparql_prefix('powreal') + \
                        create_sparql_prefix('rdf') + \
                        create_sparql_prefix('xsd') + \
                        create_sparql_prefix('powsys') + \
                        create_sparql_prefix('om') + \
                        create_sparql_prefix('ontocape') + \
                        '''SELECT ?datairi WHERE {   ?building powsys:hasBusNode <''' + bus_iri + '''> .
                                                ?building ontocape:contains ?PV.
                                                ?PV rdf:type powreal:PhotovoltaicPanel .
                                                ?PV powsys:hasGeneratedPower ?genpow .
                                                ?genpow rdf:type powsys:GeneratedPower .
                                                ?genpow om:hasValue ?datairi .
                                                ?datairi rdf:type om:Measure .
                                                }'''

                response = kg_client.performQuery(query)
                if len(response) == 0:
                    raise KGException("Unable to query for PV panel!")
                return response[0]["datairi"]

            except Exception as ex:
                raise KGException("Unable to query for any NTU PV panel!") from ex
