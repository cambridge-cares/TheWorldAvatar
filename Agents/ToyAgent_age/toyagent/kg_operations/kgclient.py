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
from toyagent.utils.stack_configs import QUERY_ENDPOINT

from toyagent.datamodel.iris import *
from toyagent.datamodel.data import GBP_SYMBOL, TIME_FORMAT_LONG, TIME_FORMAT_SHORT

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    
    def get_birthday(self, birthday_iri):
        
        ontop_url = QUERY_ENDPOINT.split('/blazegraph')[0] + '/ontop/ui/sparql'
        service_expression = f'SERVICE <{ontop_url}> {{ '
        query_string = f"""
        SELECT ?personiri ?birth
        WHERE {{
        {service_expression}
        ?personiri a <{EX_PERSON}>;
               <{EX_HASBIRTHDAY}> <{birthday_iri}>.
        <{birthday_iri}>  a   <{EX_BIRTHDAY}>;
            <{BIRTHDAY_HAVE_DATE}> ?birth.
        }}}}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        # An alternative way to query without the use of kg_client
        '''
        # sparql = SPARQLWrapper(ontop_url)
        # sparql.setMethod(POST)  # POST query, not GET
        # sparql.setQuery(query_string)
        # sparql.setReturnFormat(JSON)
        # result = sparql.query().convert()
        # res = result["results"]["bindings"]
        # # Iterate over each dictionary in the list
        # for i, my_dict in enumerate(res):
        #     new_dict = {}
        #     # Iterate over each key-value pair in the dictionary
        #     for key, value in my_dict.items():
        #         # If the value is a dictionary with a 'value' key, replace the dictionary with its 'value' value
        #         if isinstance(value, dict) and 'value' in value:
        #             new_dict[key] = value['value']
        #         else:
        #             new_dict[key] = value
        #     # Replace the dictionary in the list with the new dictionary
        #     res[i] = new_dict
        '''
        
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

# ontop_url = QUERY_ENDPOINT.split('/blazegraph')[0] + '/ontop/ui/sparql'
# a = KGClient(ontop_url,ontop_url)
# res = a.get_birthday('http://example.org/birthday#Birthday_robot_1')
    
