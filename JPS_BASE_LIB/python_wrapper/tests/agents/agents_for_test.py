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
from .sparql_client_for_test import RANDOM_STRING_WITH_SPACES
from .sparql_client_for_test import RANDOM_EXAMPLE_INPUTPLACEHOLDEREXCEPTIONTHROW
from .sparql_client_for_test import RANDOM_EXAMPLE_OUTPUTPLACEHOLDEREXCEPTIONTHROW
from .sparql_client_for_test import RANDOM_EXAMPLE_EXCEPTION_THROW_MSG

from .data_model_for_test import NumOfPoints
from .data_model_for_test import UpperLimit
from .data_model_for_test import LowerLimit
from .data_model_for_test import Point
from .data_model_for_test import MaxValue
from .data_model_for_test import MinValue
from .data_model_for_test import Difference
from .data_model_for_test import DifferenceReverse


class UpdateEndpoint(DerivationAgent):
    def set_diff_agent_service(self, agent_iri: str):
        self.DIFFAGENT_SERVICE = agent_iri

    def set_diff_reverse_agent_service(self, agent_iri: str):
        self.DIFFREVERSEAGENT_SERVICE = agent_iri

    # NOTE Placeholder to enable instantiating UpdateEndpoint instance
    agent_input_concepts = []

    # NOTE Placeholder to enable instantiating UpdateEndpoint instance
    agent_output_concepts = []

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
    agent_input_concepts = [RANDOM_EXAMPLE_INPUTPLACEHOLDEREXCEPTIONTHROW]

    agent_output_concepts = [RANDOM_EXAMPLE_OUTPUTPLACEHOLDEREXCEPTIONTHROW]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing ExceptionThrow Derivation: {derivation_inputs.getDerivationIRI()}")
        raise Exception(RANDOM_EXAMPLE_EXCEPTION_THROW_MSG)

class DifferenceAgent(DerivationAgent):
    agent_input_concepts = [MaxValue.get_rdf_type(), MinValue.get_rdf_type()]

    agent_output_concepts = [Difference.get_rdf_type()]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing Difference Derivation: {derivation_inputs.getDerivationIRI()}")
        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get min and max value
        max_iris = derivation_inputs.getIris(MaxValue.get_rdf_type())
        min_iris = derivation_inputs.getIris(MinValue.get_rdf_type())
        # If either max or min is empty, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(max_iris) or not bool(min_iris):
            return
        max_val = sparql_client.getValue(max_iris[0])
        min_val = sparql_client.getValue(min_iris[0])

        # Compute difference, write to derivation_outputs
        diff = max_val - min_val
        diff_instance = Difference(hasValue=diff)
        derivation_outputs.add_outputs_ogm(diff_instance)


class DiffReverseAgent(DerivationAgent):
    agent_input_concepts = [MaxValue, MinValue]

    agent_output_concepts = [DifferenceReverse.get_rdf_type()]

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
        max_iris = derivation_inputs.getIris(MaxValue.get_rdf_type())
        min_iris = derivation_inputs.getIris(MinValue.get_rdf_type())
        # If either max or min is empty, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(max_iris) or not bool(min_iris):
            return
        max_val = sparql_client.getValue(max_iris[0])
        min_val = sparql_client.getValue(min_iris[0])

        # Compute difference, write to derivation_outputs
        diff_reverse_instance = DifferenceReverse(hasValue=min_val - max_val)
        derivation_outputs.add_outputs_ogm(diff_reverse_instance)


class MaxValueAgent(DerivationAgent):
    agent_input_concepts = [Point.get_rdf_type()]

    agent_output_concepts = [MaxValue]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing MaxValue Derivation: {derivation_inputs.getDerivationIRI()}")

        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        list_points_iri = derivation_inputs.getIris(Point.get_rdf_type())
        # If no points, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(list_points_iri):
            return
        # Otherwise, compute min value
        max_val = sparql_client.getExtremeValueInList(list_points_iri, True)

        # Compute max value, write to derivation_outputs
        max_instance = MaxValue(hasValue=max_val)
        derivation_outputs.add_outputs_ogm(max_instance)


class MinValueAgent(DerivationAgent):
    agent_input_concepts = [Point]

    agent_output_concepts = [MinValue]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing MinValue Derivation: {derivation_inputs.getDerivationIRI()}")

        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the list of random points
        list_points_iri = derivation_inputs.getIris(Point.get_rdf_type())
        # If no points, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if not bool(list_points_iri):
            return
        # Otherwise, compute min value
        min_val = sparql_client.getExtremeValueInList(list_points_iri, False)

        # Compute min value, write to derivation_outputs
        min_instance = MinValue(hasValue=min_val)
        derivation_outputs.add_outputs_ogm(min_instance)


class RNGAgent(DerivationAgent):
    agent_input_concepts = [NumOfPoints, UpperLimit.get_rdf_type(), LowerLimit]

    agent_output_concepts = [Point]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Log IRI of current derivation been handled, implying the function is working as expected as long as the test still passes
        self.logger.info(f"Processing RNG Derivation: {derivation_inputs.getDerivationIRI()}")

        sparql_client = self.get_sparql_client(PySparqlClientForTest)

        # Get the number of points to be generated
        numofpoints_instance = derivation_inputs.get_inputs_ogm_assume_one(
            clz=NumOfPoints,
            sparql_client=sparql_client,
            recursive_depth=-1,
        )
        numofpoints = numofpoints_instance.hasValue.get_range_assume_one()

        # Get the upper limit
        upperlimit_instance = derivation_inputs.get_inputs_ogm_assume_one(
            clz=UpperLimit,
            sparql_client=sparql_client,
            recursive_depth=-1,
        )
        upperlimit = upperlimit_instance.hasValue.get_range_assume_one()

        # Get the lower limit
        lowerlimit_instance = derivation_inputs.get_inputs_ogm_assume_one(
            clz=LowerLimit,
            sparql_client=sparql_client,
            recursive_depth=-1,
        )
        lowerlimit = lowerlimit_instance.hasValue.get_range_assume_one()

        # If the number of points is less than 1, skip the rest of the function
        # This is to test that the agent can handle the case where no output is generated
        if numofpoints < 1:
            return

        # Generate random points triples in a Graph and add to derivation_outputs
        for i in range(numofpoints):
            new_point = Point(
                hasValue=random.randint(lowerlimit, upperlimit),
                # this RANDOM_STRING_WITH_SPACES is to test that the agent can add a string with spaces as data property
                rdfs_comment=RANDOM_STRING_WITH_SPACES,
                # below special numbers are added to test the python side can be correctly parsed by the java side
                specialValue=[float("inf"), float("-inf"), float("nan")]
            )
            # addTriple and addLiteral will be called by addGraph which is called by add_outputs_ogm
            # therefore all tested behind the scenes
            derivation_outputs.add_outputs_ogm(new_point)
