from NTUEnergyClusterAgent.error_handling.exceptions import KGException
from NTUEnergyClusterAgent.kg_utils.kgClient import KGClient
from NTUEnergyClusterAgent.kg_utils.utils import create_sparql_prefix
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

    def query_busnode_iris(query_endpoint: str, update_endpoint: str):
        try:
            kg_client = KGClient(query_endpoint, update_endpoint)
            query = create_sparql_prefix('powreal') + \
                    create_sparql_prefix('rdf') + \
                    '''SELECT ?busNode WHERE { ?busNode rdf:type powreal:BusNode .
                                                FILTER (!contains(str(?busNode), "GENERATOR"))}'''
            response = kg_client.performQuery(query)
            if len(response) == 0:
                raise KGException("Unable to query for any bus node IRI!")
            # sort the response based on the bus node number
            #response = sorted(response, key=lambda url: int(url.rstrip('>').split('_')[-1]))
            response = sorted(response, key=lambda d: int(d['busNode'].rstrip('>').split('_')[-1]))
            return response

        except Exception as ex:
            raise KGException("Unable to query for bus node IRI!") from ex

