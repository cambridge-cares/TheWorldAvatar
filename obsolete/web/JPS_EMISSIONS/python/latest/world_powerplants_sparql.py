import rdflib

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