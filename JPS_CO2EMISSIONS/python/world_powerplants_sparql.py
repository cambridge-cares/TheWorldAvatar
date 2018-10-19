import rdflib
import json

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger

class WorldPowerPlantsSPARQL:

    def __init__(self):
        # self.rootNodeDir = "C:/Users/WE/Desktop/PlantTemplate/{}".format("WorldPowerPlants.owl")
        self.rootNodeDir = "http://www.theworldavatar.com/kb/powerplants/WorldPowerPlants.owl"
        self.graph = rdflib.Graph()
        self.rootNode = "http://www.theworldavatar.com/kb/powerplants/WorldPowerPlants.owl#WorldPowerPlants"
        self.graph.parse(self.rootNodeDir)

    def __del__(self):
        pass

    def getPowerplants(self):
        queryString = """
            PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

            SELECT ?powerplantIRI
            WHERE
            {{
                <{0}> system:hasSubsystem ?powerplantIRI .
            }}
        """.format(self.rootNode)

        powerplants = []
        queryResults = self.graph.query(queryString).bindings
        for result in queryResults:
            powerplants.append(result['powerplantIRI'])

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