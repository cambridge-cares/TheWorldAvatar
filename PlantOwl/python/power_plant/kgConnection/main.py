# The purpose of this module is to have a single entry point
# to your application
#============================================================
from kgConnection.app_module import doTask


def get_all_power_plant_iri():
    query = """
        PREFIX powerplant:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?powerPlantIRI
        WHERE
        {
        ?powerPlantIRI rdf:type powerplant:PowerPlant .
        } LIMIT 10
        """
    return query
	
def get_all_mechanism_iri():
    query = """
        PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>	SELECT ?mechanismIRI
        WHERE	
		{ ?mechanismIRI rdf:type ontokin:ReactionMechanism .
		} LIMIT 10
        """
    return query

if __name__== '__main__':
	response1 = doTask(get_all_power_plant_iri(), "ukpowerplant", True, True)
	print('Response:')
	print('Response 1', response1)
