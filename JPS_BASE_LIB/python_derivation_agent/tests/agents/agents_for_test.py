import random
import uuid

from pyderivationagent import DerivationAgent, DerivationInputs, DerivationOutputs
from .sparql_client_for_test import PySparqlClientForTest
from .sparql_client_for_test import RANDOM_EXAMPLE_NUMOFPOINTS
from .sparql_client_for_test import RANDOM_EXAMPLE_UPPERLIMIT
from .sparql_client_for_test import RANDOM_EXAMPLE_LOWERLIMIT
from .sparql_client_for_test import RANDOM_EXAMPLE_POINT
from .sparql_client_for_test import RANDOM_EXAMPLE_LISTOFPOINTS
from .sparql_client_for_test import RANDOM_EXAMPLE_MAXVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_MINVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCE
from .sparql_client_for_test import RANDOM_EXAMPLE_HASVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_HASPOINT
from .sparql_client_for_test import RANDOM_EXAMPLE_BASE_URL

class UpdateEndpoint(DerivationAgent):
    def update_derivations(self):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        diff_iri = sparql_client.getDifferenceIRI()
        diff_derivation = self.derivationClient.getDerivationsOf([diff_iri])[diff_iri]
        self.derivationClient.unifiedUpdateDerivation(diff_derivation)
        return {"status": "successfully requested update derivation <" + diff_derivation + ">, will be done in due course"}


class DifferenceAgent(DerivationAgent):
    def agent_input_concepts(self, *args) -> list:
        return super().agent_input_concepts(*[RANDOM_EXAMPLE_MAXVALUE, RANDOM_EXAMPLE_MINVALUE])

    def agent_output_concepts(self, *args) -> list:
        return super().agent_output_concepts(*[RANDOM_EXAMPLE_DIFFERENCE])

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get min and max value
        max_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_MAXVALUE)[0]
        max = sparql_client.getValue(max_iri)
        min_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_MINVALUE)[0]
        min = sparql_client.getValue(min_iri)

        # Compute difference, write to derivation_outputs
        diff = max - min
        diff_iri = RANDOM_EXAMPLE_BASE_URL + 'Difference_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(diff_iri, RANDOM_EXAMPLE_DIFFERENCE)
        derivation_outputs.addTriple(diff_iri, RANDOM_EXAMPLE_HASVALUE, diff)


class MaxValueAgent(DerivationAgent):
    def agent_input_concepts(self, *args) -> list:
        return super().agent_input_concepts(*[RANDOM_EXAMPLE_LISTOFPOINTS])

    def agent_output_concepts(self, *args) -> list:
        return super().agent_output_concepts(*[RANDOM_EXAMPLE_MAXVALUE])

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        listofpoints_iri = derivation_inputs.getIris(
            RANDOM_EXAMPLE_LISTOFPOINTS)[0]
        max = sparql_client.getExtremeValueInList(listofpoints_iri, True)

        # Compute max value, write to derivation_outputs
        max_iri = RANDOM_EXAMPLE_BASE_URL + 'MaxValue_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(max_iri, RANDOM_EXAMPLE_MAXVALUE)
        derivation_outputs.addTriple(max_iri, RANDOM_EXAMPLE_HASVALUE, max)


class MinValueAgent(DerivationAgent):
    def agent_input_concepts(self, *args) -> list:
        return super().agent_input_concepts(*[RANDOM_EXAMPLE_LISTOFPOINTS])

    def agent_output_concepts(self, *args) -> list:
        return super().agent_output_concepts(*[RANDOM_EXAMPLE_MINVALUE])

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        listofpoints_iri = derivation_inputs.getIris(
            RANDOM_EXAMPLE_LISTOFPOINTS)[0]
        min = sparql_client.getExtremeValueInList(listofpoints_iri, False)

        # Compute min value, write to derivation_outputs
        min_iri = RANDOM_EXAMPLE_BASE_URL + 'MinValue_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(min_iri, RANDOM_EXAMPLE_MINVALUE)
        derivation_outputs.addTriple(min_iri, RANDOM_EXAMPLE_HASVALUE, min)


class RNGAgent(DerivationAgent):
    def agent_input_concepts(self, *args) -> list:
        return super().agent_input_concepts(*[RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_UPPERLIMIT, RANDOM_EXAMPLE_LOWERLIMIT])

    def agent_output_concepts(self, *args) -> list:
        return super().agent_output_concepts(*[RANDOM_EXAMPLE_LISTOFPOINTS, RANDOM_EXAMPLE_POINT])

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the number of points to be generated
        numofpoints_iri = derivation_inputs.getIris(
            RANDOM_EXAMPLE_NUMOFPOINTS)[0]
        numofpoints = sparql_client.getValue(numofpoints_iri)

        # Get the upper limit
        upperlimit_iri = derivation_inputs.getIris(
            RANDOM_EXAMPLE_UPPERLIMIT)[0]
        upperlimit = sparql_client.getValue(upperlimit_iri)

        # Get the lower limit
        lowerlimit_iri = derivation_inputs.getIris(
            RANDOM_EXAMPLE_LOWERLIMIT)[0]
        lowerlimit = sparql_client.getValue(lowerlimit_iri)

        # Generate a list of random points, write to derivation_outputs
        listofpoints_iri = RANDOM_EXAMPLE_BASE_URL + \
            'ListOfPoints_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(
            listofpoints_iri, RANDOM_EXAMPLE_LISTOFPOINTS)

        for i in range(numofpoints):
            new_point = random.randint(lowerlimit, upperlimit)
            pt_iri = RANDOM_EXAMPLE_BASE_URL + 'Point_' + str(uuid.uuid4())
            derivation_outputs.createNewEntity(pt_iri, RANDOM_EXAMPLE_POINT)
            derivation_outputs.addTriple(
                listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, pt_iri)
            derivation_outputs.addTriple(
                pt_iri, RANDOM_EXAMPLE_HASVALUE, new_point)
