import random

from pyasyncagent import AsyncAgent
from tests.sparql_client_for_test import PySparqlClientForTest

class RandomAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> list:
        sparql_client = PySparqlClientForTest(self.kgUrl, self.kgUrl)

        # Get the number of points to be generated
        numofpoints_iri = sparql_client.getNumOfPoints()
        numofpoints = sparql_client.getValue(numofpoints_iri)

        # Get the upper limit
        upperlimit_iri = sparql_client.getUpperLimit()
        upperlimit = sparql_client.getValue(upperlimit_iri)

        # Get the lower limit
        lowerlimit_iri = sparql_client.getLowerLimit()
        lowerlimit = sparql_client.getValue(lowerlimit_iri)

        new_points = []
        for i in range(numofpoints):
            new_points.append(random.randint(lowerlimit, upperlimit))

        new_created_iri = sparql_client.createListOfPoints(new_points)
        return [new_created_iri]
