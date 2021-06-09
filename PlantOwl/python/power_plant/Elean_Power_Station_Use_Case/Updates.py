#########################################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 30 Mar 2021                                     #
#########################################################

"""This module defines SPARQL update operations"""

def insert_power_density(subject, predicate, object, value, units):
    """Sets the current SPARQL query"""
    query = """
            Prefix ontocropenergy: <http://www.theworldavatar.com/ontology/ontocropenergy/OntoCropEnergy.owl#> 
            Prefix ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> 
            INSERT DATA {
                <%s> %s <%s> .
                <http://www.theworldavatar.com/kb/ontocropenergy/PowerDensity_1> ontospecies:value %s .
                <http://www.theworldavatar.com/kb/ontocropenergy/PowerDensity_1> ontospecies:units "%s" .
            }

            """ % (subject, predicate, object, value, units)
    return query

def delete_power_density(subject, predicate, object, value, units):
    """Sets the current SPARQL query"""
    query = """
            Prefix ontocropenergy: <http://www.theworldavatar.com/ontology/ontocropenergy/OntoCropEnergy.owl#> 
            Prefix ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> 
            DELETE DATA {
                <%s> %s <%s> .
                <http://www.theworldavatar.com/kb/ontocropenergy/PowerDensity_1> ontospecies:value %s .
                <http://www.theworldavatar.com/kb/ontocropenergy/PowerDensity_1> ontospecies:units "%s" .
            }

            """ % (subject, predicate, object, value, units)
    return query