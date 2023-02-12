################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

from py4jps import agentlogging
from agent.datamodel import *

# Initialise logger
logger = agentlogging.get_logger("prod")


#
# EXTERNAL SPARQL QUERIES
#
def ons_county(county_name: str) -> str:
    # Retrieve county IRI from ONS API
    query = f"""
        SELECT DISTINCT *
        WHERE {{
        ?district_iri <{RDF_TYPE}> <{ONS_GEOGRAPGY}> ;
                      <{ONS_MEMBER_OF}> <{ONS_COUNTIES}> ; 
                      <{ONS_NAME}> "{county_name}" .   
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query

#
# INTERNAL SPARQL QUERIES
#