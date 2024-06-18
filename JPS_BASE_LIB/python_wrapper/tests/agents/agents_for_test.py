import random
import uuid
import time
from rdflib import Literal
from rdflib import URIRef
from rdflib import Graph
from rdflib import RDFS
from rdflib import XSD
from twa.agent import DerivationAgent
from twa.data_model.derivation import DerivationInputs, DerivationOutputs
from .sparql_client_for_test import PySparqlClientForTest
from .sparql_client_for_test import RANDOM_EXAMPLE_NUMOFPOINTS
from .sparql_client_for_test import RANDOM_EXAMPLE_UPPERLIMIT
from .sparql_client_for_test import RANDOM_EXAMPLE_LOWERLIMIT
from .sparql_client_for_test import RANDOM_EXAMPLE_POINT
from .sparql_client_for_test import RANDOM_EXAMPLE_MAXVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_MINVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCE
from .sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCEREVERSE
from .sparql_client_for_test import RANDOM_EXAMPLE_HASVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_BASE_URL
from .sparql_client_for_test import RANDOM_STRING_WITH_SPACES
from .sparql_client_for_test import RANDOM_EXAMPLE_SPECIALVALUE
from .sparql_client_for_test import RANDOM_EXAMPLE_INPUTPLACEHOLDEREXCEPTIONTHROW
from .sparql_client_for_test import RANDOM_EXAMPLE_OUTPUTPLACEHOLDEREXCEPTIONTHROW
from .sparql_client_for_test import RANDOM_EXAMPLE_EXCEPTION_THROW_MSG

class UpdateEndpoint(DerivationAgent):
    def set_diff_agent_service(self, agent_iri: str):
        self.DIFFAGENT_SERVICE = agent_iri

    def set_diff_reverse_agent_service(self, agent_iri: str):
        self.DIFFREVERSEAGENT_SERVICE = agent_iri

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
        try:
            diff_derivation_iri = self.derivation_client.getDerivations(self.DIFFAGENT_SERVICE)
            diff_reverse_derivation_iri_list = self.derivation_client.getDerivations(self.DIFFREVERSEAGENT_SERVICE)
            self.logger.info("Difference Derivation IRI: %s", diff_derivation_iri)
            self.logger.info("Difference Reverse Derivation IRI List: %s", diff_reverse_derivation_iri_list)
            derivations = diff_derivation_iri
            derivations.extend(diff_reverse_derivation_iri_list)
            self.logger.info("Derivations: %s", derivations)
            for derivation in derivations:
                self.derivation_client.unifiedUpdateDerivation(derivation)
            return {"status": f"successfully requested update derivation {derivations}, will be done in due course"}
        except Exception as e:
            raise f"Difference IRI: {diff_derivation_iri}; DifferenceReverse IRI list: {diff_reverse_derivation_iri_list}; Requested Derivations: {derivations}; Error in update_derivations: {str(e)}"

class ExceptionThrowAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_INPUTPLACEHOLDEREXCEPTIONTHROW]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_OUTPUTPLACEHOLDEREXCEPTIONTHROW]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing ExceptionThrow Derivation: {derivation_inputs.getDerivationIRI()}")
        raise Exception(RANDOM_EXAMPLE_EXCEPTION_THROW_MSG)

class DifferenceAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_MAXVALUE, RANDOM_EXAMPLE_MINVALUE]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_DIFFERENCE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing Difference Derivation: {derivation_inputs.getDerivationIRI()}")
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get min and max value
        max_iris = derivation_inputs.getIris(RANDOM_EXAMPLE_MAXVALUE)
        min_iris = derivation_inputs.getIris(RANDOM_EXAMPLE_MINVALUE)
        # If either max or min is empty, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(max_iris) or not bool(min_iris):
            return
        max_val = sparql_client.getValue(max_iris[0])
        min_val = sparql_client.getValue(min_iris[0])

        # Compute difference, write to derivation_outputs
        diff = max_val - min_val
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
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing DiffReverse Derivation: {derivation_inputs.getDerivationIRI()}")

        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get min and max value
        max_iris = derivation_inputs.getIris(RANDOM_EXAMPLE_MAXVALUE)
        min_iris = derivation_inputs.getIris(RANDOM_EXAMPLE_MINVALUE)
        # If either max or min is empty, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(max_iris) or not bool(min_iris):
            return
        max_val = sparql_client.getValue(max_iris[0])
        min_val = sparql_client.getValue(min_iris[0])

        # Compute difference, write to derivation_outputs
        diff_reverse = min_val - max_val
        diff_reverse_iri = RANDOM_EXAMPLE_BASE_URL + 'DifferenceReverse_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(diff_reverse_iri, RANDOM_EXAMPLE_DIFFERENCEREVERSE)
        derivation_outputs.addLiteral(diff_reverse_iri, RANDOM_EXAMPLE_HASVALUE, diff_reverse)


class MaxValueAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_POINT]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_MAXVALUE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing MaxValue Derivation: {derivation_inputs.getDerivationIRI()}")

        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        list_points_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_POINT)
        # If no points, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(list_points_iri):
            return
        # Otherwise, compute min value
        max_val = sparql_client.getExtremeValueInList(list_points_iri, True)

        # Compute max value, write to derivation_outputs
        max_iri = RANDOM_EXAMPLE_BASE_URL + 'MaxValue_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(max_iri, RANDOM_EXAMPLE_MAXVALUE)
        derivation_outputs.addLiteral(max_iri, RANDOM_EXAMPLE_HASVALUE, max_val)


class MinValueAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_POINT]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_MINVALUE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing MinValue Derivation: {derivation_inputs.getDerivationIRI()}")

        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        list_points_iri = derivation_inputs.getIris(RANDOM_EXAMPLE_POINT)
        # If no points, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(list_points_iri):
            return
        # Otherwise, compute min value
        min_val = sparql_client.getExtremeValueInList(list_points_iri, False)

        # Compute min value, write to derivation_outputs
        min_iri = RANDOM_EXAMPLE_BASE_URL + 'MinValue_' + str(uuid.uuid4())
        derivation_outputs.createNewEntity(min_iri, RANDOM_EXAMPLE_MINVALUE)
        derivation_outputs.addLiteral(min_iri, RANDOM_EXAMPLE_HASVALUE, min_val)


class RNGAgent(DerivationAgent):
    def agent_input_concepts(self) -> list:
        return [RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_UPPERLIMIT, RANDOM_EXAMPLE_LOWERLIMIT]

    def agent_output_concepts(self) -> list:
        return [RANDOM_EXAMPLE_POINT]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing RNG Derivation: {derivation_inputs.getDerivationIRI()}")

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

        # If the number of points is less than 1, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if numofpoints < 1:
            return

        # Generate random points triples in a Graph and add to derivation_outputs
        g = Graph()
        for i in range(numofpoints):
            new_point = random.randint(lowerlimit, upperlimit)
            pt_iri = RANDOM_EXAMPLE_BASE_URL + 'Point_' + str(uuid.uuid4())
            derivation_outputs.createNewEntity(pt_iri, RANDOM_EXAMPLE_POINT)
            g.add((URIRef(pt_iri), URIRef(RANDOM_EXAMPLE_HASVALUE), Literal(new_point)))
            # this RANDOM_STRING_WITH_SPACES is to test that the agent can add a string with spaces as data property
            g.add((URIRef(pt_iri), RDFS.comment, Literal(RANDOM_STRING_WITH_SPACES)))
            # below special numbers are added to test the python side can be correctly parsed by the java side
            g.add((URIRef(pt_iri), URIRef(RANDOM_EXAMPLE_SPECIALVALUE), Literal(float("inf"))))
            g.add((URIRef(pt_iri), URIRef(RANDOM_EXAMPLE_SPECIALVALUE), Literal(float("-inf"))))
            g.add((URIRef(pt_iri), URIRef(RANDOM_EXAMPLE_SPECIALVALUE), Literal(float("nan"))))

        # addTriple and addLiteral will be called by addGraph, therefore tested behind the scenes
        derivation_outputs.addGraph(g)
