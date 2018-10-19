import rdflib
import re
import sys
import json
from rdflib.plugins.sparql.processor import processUpdate

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger

class PowerplantSPARQLSync:

    def __init__(self, powerplant):

        self.powerplantIRI = powerplant
        self.powerplantName = powerplant[powerplant.rfind('#') + 1:]
        self.graph = rdflib.Graph()
        self.fileDestination = "C:/TOMCAT/webapps/ROOT/kb/powerplants/{}.owl".format(self.powerplantName)
        self.graph.parse(self.fileDestination)

    def __del__(self):
        pass

    def updatePowerplantEmission(self, latestEmission):
        queryString = """
            PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX j6: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
            PREFIX j8: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
            PREFIX j5: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
            PREFIX j7: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>

            DELETE
            {{
                ?emissionRateValIRI j1:numericalValue ?emissionRateValue.
            }}
            INSERT 
            {{
                ?emissionRateValIRI j1:numericalValue {1}.
            }}
            WHERE
            {{
                <{0}> j5:realizes ?generation.
                    ?generation j7:hasEmission ?emissionRateIRI.
                        ?emissionRateIRI j1:hasValue ?emissionRateValIRI.
                            ?emissionRateValIRI j1:numericalValue ?emissionRateValue.
            }}
        """.format(self.powerplantIRI, latestEmission)

        processUpdate(self.graph, queryString)
        self.graph.serialize(destination=self.fileDestination, format='xml')

    
if __name__ == "__main__":
    pythonLogger = PythonLogger('powerplant_sparql_update.py')
    pythonLogger.postInfoToLogServer('start of powerplant_sparql_update.py')
    
    try:
        plantIRI = sys.argv[1]
        latestEmission = sys.argv[2]
        pSPARQL = PowerplantSPARQLSync(plantIRI)
        pSPARQL.updatePowerplantEmission(latestEmission)
        returnResultsToJava(json.dumps("COMPLETE"))
        pythonLogger.postInfoToLogServer('end of powerplant_sparql_update.py')
    except Exception as e:
        returnExceptionToJava(e)
        pythonLogger.postInfoToLogServer('end of powerplant_sparql_update.py')