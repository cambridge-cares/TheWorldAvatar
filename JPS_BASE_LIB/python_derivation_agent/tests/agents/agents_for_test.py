import random
import uuid
import time
from rdflib import RDFS
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
from .sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCEREVERSE
from .sparql_client_for_test import RANDOM_EXAMPLE_HASVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_HASPOINT
from .sparql_client_for_test import RANDOM_EXAMPLE_BASE_URL

class UpdateEndpoint(DerivationAgent):
    # NOTE Placeholder to enable instantiating UpdateEndpoint instance
    def agent_input_concepts(self) -> list:
        return []

    # NOTE Placeholder to enable instantiating UpdateEndpoint instance
    def agent_output_concepts(self) -> list:
        return []

    # NOTE Placeholder to enable instantiating UpdateEndpoint instance
    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    # NOTE Placeholder to enable instantiating UpdateEndpoint instance
    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        pass

    def update_derivations(self):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        try:
            diff_iri = sparql_client.getDifferenceIRI()
            diff_reverse_iri_list = sparql_client.getDiffReverseIRI()
            self.logger.info("Difference IRI: %s", diff_iri)
            self.logger.info("Difference Reverse IRI List: %s", diff_reverse_iri_list)
            derivations = list(self.derivationClient.getDerivationsOf([diff_iri] + diff_reverse_iri_list).values())
            self.logger.info("Derivations: %s", derivations)
            for derivation in derivations:
                self.derivationClient.unifiedUpdateDerivation(derivation)
            return {"status": f"successfully requested update derivation {derivations}, will be done in due course"}
        except Exception as e:
            raise f"Difference IRI: {diff_iri}; DifferenceReverse IRI list: {diff_reverse_iri_list}; Requested Derivations: {derivations}; Error in update_derivations: {str(e)}"

class DifferenceAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_MAXVALUE, RANDOM_EXAMPLE_MINVALUE]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_DIFFERENCE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

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
        derivation_outputs.addLiteral(diff_iri, RANDOM_EXAMPLE_HASVALUE, diff)


class DiffReverseAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_MAXVALUE, RANDOM_EXAMPLE_MINVALUE]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_DIFFERENCEREVERSE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # DiffReverseAgent will sleep for a few seconds before it runs
        # This is to simulate an agent running long jobs
        time.sleep(self.time_interval)

        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get min and max value
        max_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_MAXVALUE)[0]
        max = sparql_client.getValue(max_iri)
        min_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_MINVALUE)[0]
        min = sparql_client.getValue(min_iri)

        # Compute difference, write to derivation_outputs
        diff_reverse = min - max
        diff_reverse_iri = RANDOM_EXAMPLE_BASE_URL + 'DifferenceReverse_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(diff_reverse_iri, RANDOM_EXAMPLE_DIFFERENCEREVERSE)
        derivation_outputs.addLiteral(diff_reverse_iri, RANDOM_EXAMPLE_HASVALUE, diff_reverse)


class MaxValueAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_LISTOFPOINTS]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_MAXVALUE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        listofpoints_iri = derivation_inputs.getIris(
            RANDOM_EXAMPLE_LISTOFPOINTS)[0]
        max = sparql_client.getExtremeValueInList(listofpoints_iri, True)

        # Compute max value, write to derivation_outputs
        max_iri = RANDOM_EXAMPLE_BASE_URL + 'MaxValue_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(max_iri, RANDOM_EXAMPLE_MAXVALUE)
        derivation_outputs.addLiteral(max_iri, RANDOM_EXAMPLE_HASVALUE, max)


class MinValueAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_LISTOFPOINTS]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_MINVALUE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        listofpoints_iri = derivation_inputs.getIris(
            RANDOM_EXAMPLE_LISTOFPOINTS)[0]
        min = sparql_client.getExtremeValueInList(listofpoints_iri, False)

        # Compute min value, write to derivation_outputs
        min_iri = RANDOM_EXAMPLE_BASE_URL + 'MinValue_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(min_iri, RANDOM_EXAMPLE_MINVALUE)
        derivation_outputs.addLiteral(min_iri, RANDOM_EXAMPLE_HASVALUE, min)


class RNGAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_UPPERLIMIT, RANDOM_EXAMPLE_LOWERLIMIT]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_LISTOFPOINTS, RANDOM_EXAMPLE_POINT]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

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
            derivation_outputs.addLiteral(
                pt_iri, RANDOM_EXAMPLE_HASVALUE, new_point)
            derivation_outputs.addLiteral(pt_iri, RDFS.comment.toPython(), "this is a random comment")
