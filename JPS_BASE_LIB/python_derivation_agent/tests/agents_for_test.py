import random
import uuid

from pyderivationagent import DerivationAgent
from tests.sparql_client_for_test import PySparqlClientForTest, RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_UPPERLIMIT, RANDOM_EXAMPLE_LOWERLIMIT, RANDOM_EXAMPLE_POINT,RANDOM_EXAMPLE_LISTOFPOINTS, RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_HASPOINT, RANDOM_EXAMPLE_BASE_URL

class DifferenceAgent(DerivationAgent):
    pass

class MaxValueAgent(DerivationAgent):
    pass

class MinValueAgent(DerivationAgent):
    pass

class RNGAgent(DerivationAgent):
    def process_request_parameters(self, derivation_inputs, derivation_outputs):
        sparql_client = PySparqlClientForTest(self.kgUrl, self.kgUrl)

        # Get the number of points to be generated
        numofpoints_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_NUMOFPOINTS)[0]
        numofpoints = sparql_client.getValue(numofpoints_iri)

        # Get the upper limit
        upperlimit_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_UPPERLIMIT)[0]
        upperlimit = sparql_client.getValue(upperlimit_iri)

        # Get the lower limit
        lowerlimit_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_LOWERLIMIT)[0]
        lowerlimit = sparql_client.getValue(lowerlimit_iri)

        new_points = []
        for i in range(numofpoints):
            new_points.append(random.randint(lowerlimit, upperlimit))
        listofpoints_iri = RANDOM_EXAMPLE_BASE_URL + 'ListOfPoints_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(listofpoints_iri, RANDOM_EXAMPLE_LISTOFPOINTS)

        for pt in new_points:
            pt_iri = RANDOM_EXAMPLE_BASE_URL + 'Point_' + str(uuid.uuid4())
            derivation_outputs.createNewEntity(pt_iri, RANDOM_EXAMPLE_POINT)
            derivation_outputs.addTriple(listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, pt_iri)
            derivation_outputs.addTriple(pt_iri, RANDOM_EXAMPLE_HASVALUE, pt)

    # def setupJob(self, agentInputs) -> list:
    #     sparql_client = PySparqlClientForTest(self.kgUrl, self.kgUrl)

    #     # Get the number of points to be generated
    #     numofpoints_iri = agentInputs[RANDOM_EXAMPLE_NUMOFPOINTS]
    #     numofpoints = sparql_client.getValue(numofpoints_iri)

    #     # Get the upper limit
    #     upperlimit_iri = agentInputs[RANDOM_EXAMPLE_UPPERLIMIT]
    #     upperlimit = sparql_client.getValue(upperlimit_iri)

    #     # Get the lower limit
    #     lowerlimit_iri = agentInputs[RANDOM_EXAMPLE_LOWERLIMIT]
    #     lowerlimit = sparql_client.getValue(lowerlimit_iri)

    #     new_points = []
    #     for i in range(numofpoints):
    #         new_points.append(random.randint(lowerlimit, upperlimit))

    #     new_created_iri = sparql_client.createListOfPoints(new_points)

    #     return [new_created_iri]
