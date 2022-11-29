################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 29 Nov 2022                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid
import datetime as dt
import pandas as pd
from rdflib import URIRef, Literal

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


    def summarise_affected_property_values(self, property_value_iris:list) -> int:
        # Retrieve property market value estimations and summarise them
        # Returns sum of property value estimations as int

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


    # #
    # # SPARQL UPDATES
    # #
    # def instantiate_property_value(self, graph, property_iri, property_value_iri, property_value) -> str:
    #     # Returns rdflib Graph with triples to instantiate/update property market value estimation
    #     # Create unique IRIs for new instances
    #     measure_iri = KB + 'Measure_' + str(uuid.uuid4())
        
    #     # Add triples to graph (RDF Triples to be provided as Python Tuples --> double brackets)
    #     graph.add((URIRef(property_iri), URIRef(OBE_HAS_MARKET_VALUE), URIRef(property_value_iri)))
    #     graph.add((URIRef(property_value_iri), URIRef(RDF_TYPE), URIRef(OM_AMOUNT_MONEY)))
    #     graph.add((URIRef(property_value_iri), URIRef(OM_HAS_VALUE), URIRef(measure_iri)))
    #     graph.add((URIRef(measure_iri), URIRef(RDF_TYPE), URIRef(OM_MEASURE)))
    #     graph.add((URIRef(measure_iri), URIRef(OM_NUM_VALUE), Literal(property_value, datatype=XSD_INTEGER)))
    #     graph.add((URIRef(measure_iri), URIRef(OM_HAS_UNIT), URIRef(OM_GBP)))
    #     #TODO: Triple with symbol potentially to be removed once OntoUOM contains
    #     #      all relevant units/symbols and is uploaded to the KB
    #     graph.add((URIRef(OM_GBP), URIRef(OM_SYMBOL), Literal(GBP_SYMBOL, datatype=XSD_STRING)))

    #     return graph


    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query
