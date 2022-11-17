################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid
import datetime as dt
from rdflib import URIRef, Literal

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from propertyvalueestimation.datamodel.iris import *
from propertyvalueestimation.datamodel.data import GBP_SYMBOL, TIME_FORMAT_LONG, TIME_FORMAT_SHORT

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    def get_transaction_details(self, tx_iri:str) -> dict:
        # Retrieve transaction details for given transaction IRI
        # Returns dictionary with keys: price, date, property_iri

        query = f"""
            SELECT ?price ?date ?property_iri
            WHERE {{   
            <{tx_iri}> <{LRPPI_DATE}> ?date ; 
                       <{LRPPI_PRICE}> ?price ; 
                       ^<{OBE_HAS_LATEST_TRANSACTION}> ?property_iri . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        if not res:
            # In case date or price (or both) are missing (i.e. empty SPARQL result), return Nones
            res = dict(zip(['price', 'date', 'property_iri'], (None,)*3))
        else:
            res = res[0]
            # Cast price to float
            try:
                res['price'] = float(res['price'])
            except:
                res['price'] = None
            # Extract relevant date (YYYY-MM) from string
            try:
                res['date'] = dt.datetime.strptime(res['date'], TIME_FORMAT_LONG).strftime(TIME_FORMAT_SHORT)
            except:
                res['date'] = None
        return res


    def get_floor_area_and_avg_price(self, floor_area_iri:str) -> dict:
        # Retrieve total floor area and property price index (representative for 
        # associated property) of given floor area IRI
        # Returns dictionary with keys: floor_area, avg_price, property_iri

        query = f"""
            SELECT ?floor_area ?avg_price ?property_iri
            WHERE {{   
            <{floor_area_iri}> <{OM_HAS_VALUE}>/<{OM_NUM_VALUE}> ?floor_area ; 
                               ^<{OBE_HAS_TOTAL_FLOOR_AREA}> ?property_iri . 
            ?property_iri <{OBE_HAS_ADDRESS}>/<{OBE_HAS_POSTALCODE}> ?postcode_iri . 
            ?avg_price_iri <{OBE_REPRESENTATIVE_FOR}> ?postcode_iri ; 
                           <{OM_HAS_VALUE}>/<{OM_NUM_VALUE}> ?avg_price . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        if not res:
            # In case floor area or average price (or both) are missing (i.e. empty SPARQL result), return Nones
            res = dict(zip(['floor_area', 'avg_price', 'property_iri'], (None,)*3))
        else:
            res = res[0]
            # Cast floor area and average price to float
            for key in ['floor_area', 'avg_price']:
                try:
                    res[key] = float(res[key])
                except:
                    res[key] = None
        return res


    def instantiate_property_value(self, graph, property_iri, property_value_iri, property_value) -> str:
        # Returns rdflib Graph with triples to instantiate/update property market value estimation
        # Create unique IRIs for new instances
        measure_iri = KB + 'Measure_' + str(uuid.uuid4())
        
        # Add triples to graph (RDF Triples to be provided as Python Tuples --> double brackets)
        graph.add((URIRef(property_iri), URIRef(OBE_HAS_MARKET_VALUE), URIRef(property_value_iri)))
        graph.add((URIRef(property_value_iri), URIRef(RDF_TYPE), URIRef(OM_AMOUNT_MONEY)))
        graph.add((URIRef(property_value_iri), URIRef(OM_HAS_VALUE), URIRef(measure_iri)))
        graph.add((URIRef(measure_iri), URIRef(RDF_TYPE), URIRef(OM_MEASURE)))
        graph.add((URIRef(measure_iri), URIRef(OM_NUM_VALUE), Literal(property_value, datatype=XSD_INTEGER)))
        graph.add((URIRef(measure_iri), URIRef(OM_HAS_UNIT), URIRef(OM_GBP)))
        #TODO: Triple with symbol potentially to be removed once OntoUOM contains
        #      all relevant units/symbols and is uploaded to the KB
        graph.add((URIRef(OM_GBP), URIRef(OM_SYMBOL), Literal(GBP_SYMBOL, datatype=XSD_STRING)))

        return graph


    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query
