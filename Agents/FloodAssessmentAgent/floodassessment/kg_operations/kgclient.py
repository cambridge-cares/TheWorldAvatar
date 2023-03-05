################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 29 Nov 2022                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid
import pandas as pd
from rdflib import URIRef, Literal, Graph

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from floodassessment.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    def get_flood_alert_warning_severity(self, alert_warning_iri:str) -> str:
        # Retrieve severity IRI of flood alert/warning
        # Returns string of severity IRI (instantiated as ABox)

        query = f"""
            SELECT ?severity
            WHERE {{   
            <{alert_warning_iri}> <{RDF_TYPE}> <{FLOOD_ALERT_WARNING}> ; 
                                  <{FLOOD_HAS_SEVERITY}> ?severity .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        if len(res) == 1:
            return res[0].get('severity')
        else:
            logger.error(f"Ambiguous severity instantiated for Flood Alert/Warning '{alert_warning_iri}'.")
            raise Exception(f"Ambiguous severity instantiated for Flood Alert/Warning '{alert_warning_iri}'.")


    def get_associated_flood_event(self, alert_warning_iri:str) -> str:
        # Retrieve flood event IRI associated with flood alert/warning
        # Returns string of flood event IRI or None

        query = f"""
            SELECT ?flood
            WHERE {{   
            <{alert_warning_iri}> <{RDF_TYPE}> <{FLOOD_ALERT_WARNING}> ; 
                                  <{FLOOD_WARNS_ABOUT}> ?flood . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        if len(res) == 1:
            return res[0].get('flood')
        else:
            return None


    def summarise_affected_property_values(self, property_value_iris:list) -> int:
        # Retrieve property market values from list of property value IRIs and summarise them
        # Returns sum of property value estimations as float

        # Create VALUES expression for property value IRIs
        iris = ' '.join(['<'+iri+'>' for iri in property_value_iris])
        
        query = f"""
            SELECT ?value ?unit
            WHERE {{  
            VALUES ?market_value {{ {iris} }} 
            ?market_value <{RDF_TYPE}> <{OM_AMOUNT_MONEY}> ; 
                          <{OM_HAS_VALUE}> ?measure . 
            ?measure <{OM_NUM_VALUE}> ?value ; 
                     <{OM_HAS_UNIT}>/<{OM_SYMBOL}> ?unit . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        if not res:
            # In case no market values can be retrieved (i.e. empty SPARQL result), return None
            res = None
        else:
            # Summarise market values
            df = pd.DataFrame(columns=['value', 'unit'], data=res)
            # NOTE: Fix encoding issue with pound sterling
            df['unit'] = df['unit'].apply(lambda x: x.encode('ISO-8859-1').decode('utf-8'))
            # NOTE: Only consider property values in GBP (and exclude NaNs, etc.)
            df = df[df['unit'] == GBP_SYMBOL]
            df['value'] = df['value'].astype(float)
            res = df['value'].sum()
        return res


    #
    # SPARQL UPDATES
    #
    def instantiate_flood_impact(self, graph: Graph, flood_alert_warning_iri,
                                 affected_buildings_count,
                                 affected_buildings_value=None, affected_population=None,
                                 impact_description=None, flood_iri=None) -> Graph:
        """
        Returns rdflib Graph with triples to instantiate/update flood impact assessment
        
        Arguments:
            graph {rdflib.Graph} -- Graph to which triples will be added
            flood_alert_warning_iri {str} -- IRI of flood alert/warning
            affected_buildings_count {int} -- Number of affected buildings , i.e. 
                                              length of list of provided building IRIs
            affected_buildings_value {float} -- Sum of affected building values, i.e.
                                                as derived by `summarise_affected_property_values`
            affected_population {int} -- Number of affected population (if available)
            impact_description {str} -- Description of flood impact (if available)
            flood_iri {str} -- IRI of associated flood event, if already instantiated
        """


        def _add_amount_of_money_triples(graph: Graph, amount_of_money_iri, measure_iri, value, unit):
            graph.add((URIRef(amount_of_money_iri), URIRef(RDF_TYPE), URIRef(OM_AMOUNT_MONEY)))
            graph.add((URIRef(amount_of_money_iri), URIRef(OM_HAS_VALUE), URIRef(measure_iri)))
            graph.add((URIRef(measure_iri), URIRef(RDF_TYPE), URIRef(OM_MEASURE)))
            graph.add((URIRef(measure_iri), URIRef(OM_NUM_VALUE), Literal(float(value), datatype=XSD_FLOAT)))
            graph.add((URIRef(measure_iri), URIRef(OM_HAS_UNIT), URIRef(unit)))
            #NOTE: PoundSterling symbol '£' excluded from recurring updates 
            #      There have been encoding issues within the KG with recurring instantiations; hence,
            #      the agent requires the symbol to be instantiated with the KG beforehand, i.e.
            #      when uploading the ontology initially (using the EnergyPerformanceCertificate agent)
            return graph

        if not flood_iri:
            flood_iri = KB + 'Flood_' + str(uuid.uuid4())
            # Instantiate "general" flood type if no more specific flood type is instantiated/given
            graph.add((URIRef(flood_iri), URIRef(RDF_TYPE), URIRef(FLOOD_FLOOD)))

        # Create unique new IRIs for new/updated output instances of derivation
        # (replacement of old IRIs taken care of by Derivation Framework)
        impact_iri = KB + 'Impact_' + str(uuid.uuid4())
        impact_money_iri = KB + 'AmountOfMoney_' + str(uuid.uuid4())
        impact_measure_iri = KB + 'Measure_' + str(uuid.uuid4())
        population_iri = KB + 'Population_' + str(uuid.uuid4())
        buildings_iri = KB + 'Buildings_' + str(uuid.uuid4())
        bldgs_money_iri = KB + 'AmountOfMoney_' + str(uuid.uuid4())
        bldgs_measure_iri = KB + 'Measure_' + str(uuid.uuid4())

        # Add triples to graph (RDF Triples to be provided as Python Tuples --> double brackets)
        graph.add((URIRef(flood_alert_warning_iri), URIRef(FLOOD_WARNS_ABOUT), URIRef(flood_iri)))
        # Affected population (only instantiate if assessed)
        if affected_population is not None:
            graph.add((URIRef(flood_iri), URIRef(FLOOD_AFFECTS), URIRef(population_iri)))
            graph.add((URIRef(population_iri), URIRef(RDF_TYPE), URIRef(FLOOD_POPULATION)))
            graph.add((URIRef(population_iri), URIRef(FLOOD_HAS_TOTAL_COUNT), Literal(affected_population, datatype=XSD_INTEGER)))

        # Affected buildings
        graph.add((URIRef(flood_iri), URIRef(FLOOD_AFFECTS), URIRef(buildings_iri)))
        graph.add((URIRef(buildings_iri), URIRef(RDF_TYPE), URIRef(FLOOD_BUILDING)))
        graph.add((URIRef(buildings_iri), URIRef(FLOOD_HAS_TOTAL_COUNT), Literal(affected_buildings_count, datatype=XSD_INTEGER))) 
        if affected_buildings_value is not None:
            graph.add((URIRef(buildings_iri), URIRef(FLOOD_HAS_TOTAL_MONETARY_VALUE), URIRef(bldgs_money_iri)))
            # Add OM markup for amount of money
            graph = _add_amount_of_money_triples(graph, bldgs_money_iri, bldgs_measure_iri, affected_buildings_value, OM_GBP)

        # Total impact (so far only includes value of affected buildings; to be extended potentially)
        if impact_description or (affected_buildings_value is not None):
            graph.add((URIRef(flood_iri), URIRef(FLOOD_RESULTS_IN), URIRef(impact_iri)))
            graph.add((URIRef(impact_iri), URIRef(RDF_TYPE), URIRef(FLOOD_IMPACT)))
            if impact_description:
                graph.add((URIRef(impact_iri), URIRef(FLOOD_HAS_CLASSIFICATION), Literal(impact_description, datatype=XSD_STRING)))
            if affected_buildings_value is not None:
                graph.add((URIRef(impact_iri), URIRef(FLOOD_HAS_MONETARY_VALUE), URIRef(impact_money_iri)))
                # Add OM markup for amount of money
                graph = _add_amount_of_money_triples(graph, impact_money_iri, impact_measure_iri, affected_buildings_value, OM_GBP)

        return graph

    #
    # Helper functions
    #
    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query
