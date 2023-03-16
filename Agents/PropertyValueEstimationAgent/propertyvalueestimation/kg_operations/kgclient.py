################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid
import datetime as dt
from rdflib import URIRef, Literal, Graph

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
    def get_property_iri(self, tx_iri: str = None, floor_area_iri: str = None) -> dict:
        # Retrieve total floor area and property price index (representative for 
        # associated property) of given floor area IRI
        # Returns dictionary with keys: floor_area, avg_price, property_iri

        triple1 = "" if not tx_iri else f"?property_iri <{OBE_HAS_LATEST_TRANSACTION}> <{tx_iri}>"
        triple2 = "" if not floor_area_iri else f"?property_iri <{OBE_HAS_TOTAL_FLOOR_AREA}> <{floor_area_iri}>"
        query = f"""
            SELECT ?property_iri
            WHERE {{ 
            {{ {triple1} }}
            UNION
            {{ {triple2} }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        # Remove (potentially) empty result dicts from list
        res = [r for r in res if r]

        return res[0]
    

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


    def instantiate_property_value(self, graph, property_iri, property_value_iri, property_value) -> Graph:
        # Returns rdflib Graph with triples to instantiate/update property market value estimation
        # Create unique IRIs for new instances
        measure_iri = KB + 'Measure_' + str(uuid.uuid4())
        
        # Add triples to graph (RDF Triples to be provided as Python Tuples --> double brackets)
        graph.add((URIRef(property_iri), URIRef(OBE_HAS_MARKET_VALUE), URIRef(property_value_iri)))
        graph.add((URIRef(property_value_iri), URIRef(RDF_TYPE), URIRef(OM_AMOUNT_MONEY)))
        graph.add((URIRef(property_value_iri), URIRef(OM_HAS_VALUE), URIRef(measure_iri)))
        graph.add((URIRef(measure_iri), URIRef(RDF_TYPE), URIRef(OM_MEASURE)))
        graph.add((URIRef(measure_iri), URIRef(OM_NUM_VALUE), Literal(property_value, datatype=XSD_FLOAT)))
        graph.add((URIRef(measure_iri), URIRef(OM_HAS_UNIT), URIRef(OM_GBP)))
        #NOTE: PoundSterling symbol '£' excluded from recurring updates 
        #      There have been encoding issues within the KG with recurring instantiations; hence,
        #      the agent requires the symbol to be instantiated with the KG beforehand, i.e.
        #      when uploading the ontology initially (using the EnergyPerformanceCertificate agent)
        return graph
    

    def instantiate_unavailable_property_value(self, graph, property_iri, property_value_iri) -> Graph:
        # Returns rdflib Graph with triples to instantiate/update non-computable property market value estimation
        # e.g. for cases where no previous transactions are available for given property and also no average price
        #      for the associated postcode could be retrieved

        # Specify comment to instantiate
        comment = 'Property market value estimation not computable'

        # Create unique IRIs for new instances
        measure_iri = KB + 'Measure_' + str(uuid.uuid4())
        
        # Add triples to graph (RDF Triples to be provided as Python Tuples --> double brackets)
        graph.add((URIRef(property_iri), URIRef(OBE_HAS_MARKET_VALUE), URIRef(property_value_iri)))
        graph.add((URIRef(property_value_iri), URIRef(RDF_TYPE), URIRef(OM_AMOUNT_MONEY)))
        graph.add((URIRef(property_value_iri), URIRef(OM_HAS_VALUE), URIRef(measure_iri)))
        graph.add((URIRef(property_value_iri), URIRef(RDFS_COMMENT), Literal(comment, datatype=XSD_STRING)))
        graph.add((URIRef(measure_iri), URIRef(RDF_TYPE), URIRef(OM_MEASURE)))

        return graph


    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query
