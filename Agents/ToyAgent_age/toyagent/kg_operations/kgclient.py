################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid
import datetime as dt
from rdflib import URIRef, Literal
from SPARQLWrapper import SPARQLWrapper, POST

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from toyagent.datamodel.iris import *
from toyagent.datamodel.data import GBP_SYMBOL, TIME_FORMAT_LONG, TIME_FORMAT_SHORT

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    def get_birthday(self, birthday_iri):
        query = f"""
        SELECT ? personiri ?birth
        {{
        ?personiri a <{EX_PERSON}>;
               <{EX_BIRTHDAY} <{birthday_iri}>.
        <{birthday_iri}>  a   <{EX_BIRTHDAY}>;
            <{BIRTHDAY_HAVE_DATE} ?birth.
        }}
        """
        query = self.remove_unnecessary_whitespace(query)
        #res = self.performQuery(query)
        sparql = SPARQLWrapper('http://localhost:3846/ontop/ui/sparql')
        sparql.setMethod(POST)  # POST query, not GET
        sparql.setQuery(query)
        res = sparql.query()
        
        if not res:
            # In case date or price (or both) are missing (i.e. empty SPARQL result), return Nones
            res = dict(zip(['personiri', 'birth'], (None,)*2))
        else:
            res = res[0]
            try:
                res['personiri'] = str(res['personiri'])
            except:
                res['personiri'] = None

            try:
                res['birth'] = dt.datetime.strptime(res['birth'], TIME_FORMAT_LONG).strftime(TIME_FORMAT_SHORT)
            except:
                res['birth'] = None
        
        return res

    def instantiate_age(self, g, personiri, age_iri, age):
        g.add((URIRef(personiri),URIRef('a'),URIRef(EX_PERSON)))
        g.add((URIRef(personiri),URIRef(EX_HASAGE),URIRef(age_iri)))
        g.add((URIRef(age_iri),URIRef('a'),Literal(age, datatype=XSD_INTEGER)))
        return g

    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query
