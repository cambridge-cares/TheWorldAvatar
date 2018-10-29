# import rdflib
import json

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger
from sparql_wrapper import sparqlQueryRead

class WorldPowerPlantsSPARQL:

    def __init__(self):
        self.graph = "http://www.theworldavatar.com/kb/powerplants/WorldPowerPlants.owl#WorldPowerPlants"

    def __del__(self):
        pass

    def getPowerplants(self):
        queryStringWorld = """
            PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

            SELECT ?powerplantIRI
            WHERE
            {{
                GRAPH <{0}>
            {{
                <http://www.theworldavatar.com/kb/powerplants/WorldPowerPlants.owl#WorldPowerPlants> system:hasSubsystem ?powerplantIRI .
            }}}}
        """.format(self.graph)

        results = sparqlQueryRead(queryStringWorld)['results']['bindings']
        powerplants = []
        for result in results:
            powerplants.append(result['powerplantIRI']['value'])
        return powerplants

if __name__ == "__main__":
    pythonLogger = PythonLogger('world_powerplants_sparql.py')
    pythonLogger.postInfoToLogServer('start of world_powerplants_sparql.py')
    
    try:
        wPSPARQL = WorldPowerPlantsSPARQL()
        powerplants = wPSPARQL.getPowerplants()
        returnResultsToJava(json.dumps(powerplants))
        pythonLogger.postInfoToLogServer('end of world_powerplants_sparql.py')
    except Exception as e:
        returnExceptionToJava(e)
        pythonLogger.postInfoToLogServer('end of world_powerplants_sparql.py')