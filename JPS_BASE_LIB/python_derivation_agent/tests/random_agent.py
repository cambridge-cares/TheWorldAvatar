import random

from pyderivationagent import AsyncAgent
from tests.sparql_client_for_test import PySparqlClientForTest, RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_UPPERLIMIT, RANDOM_EXAMPLE_LOWERLIMIT

class RandomAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> list:
        sparql_client = PySparqlClientForTest(self.kgUrl, self.kgUrl)

        # Get the number of points to be generated
        numofpoints_iri = agentInputs[RANDOM_EXAMPLE_NUMOFPOINTS]
        numofpoints = sparql_client.getValue(numofpoints_iri)

        # Get the upper limit
        upperlimit_iri = agentInputs[RANDOM_EXAMPLE_UPPERLIMIT]
        upperlimit = sparql_client.getValue(upperlimit_iri)

        # Get the lower limit
        lowerlimit_iri = agentInputs[RANDOM_EXAMPLE_LOWERLIMIT]
        lowerlimit = sparql_client.getValue(lowerlimit_iri)

        new_points = []
        for i in range(numofpoints):
            new_points.append(random.randint(lowerlimit, upperlimit))

        new_created_iri = sparql_client.createListOfPoints(new_points)

        return [new_created_iri]
