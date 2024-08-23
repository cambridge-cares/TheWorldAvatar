# Import necessary classes from pyderivationagent
from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

# Import your sparql client class and necessary data model classes
from derivationagentpythonexample.kg_operations import ExampleSparqlClient
import derivationagentpythonexample.data_model as dm

# Other third party libraries you need
from rdflib import Graph

# The purpose of this module is to provide the agent class based on the pyderivationagent.DerivationAgent class
# Please read in-line comments for more information

class ExampleAgent(DerivationAgent):
    def __init__(
        self,
        example_conf_param: str,
        **kwargs
    ):
        # NOTE MUST call the super class's __init__ method with **kwargs
        super().__init__(**kwargs)

        # NOTE It is recommended to override the __init__ method and call the self.get_sparql_client method to get the sparql client
        # The method should be called after the super().__init__ method with input parameter as the class of your sparql client
        # which itself should be a subclass of PySparqlClient
        self.sparql_client = self.get_sparql_client(ExampleSparqlClient)

        # You can also read your own config parameters added to the __init__ method
        self.example_conf_param = example_conf_param
        # You can log information using the self.logger object
        self.logger.info(f"Example config parameter: {self.example_conf_param}")

    def agent_input_concepts(self) -> list:
        """You can override this method to define the input concepts (full IRIs) of your agent.

        Returns:
            list: A list of input concepts
        """
        # NOTE Declared inputs/outputs need proper instantiation incl.
        # rdf:type declarations in the KG for the derivation to work
        return [dm.DERIVATION_AGENT_PYTHON_EXAMPLE_MAXVALUE, dm.DERIVATION_AGENT_PYTHON_EXAMPLE_MINVALUE]

    def agent_output_concepts(self) -> list:
        """You can override this method to define the output concepts (IRIs) of your agent.

        Returns:
            list: A list of output concepts
        """
        # Output concept (i.e. result) of the Derivation
        return [dm.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE]

    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request,
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)

    def process_request_parameters(
        self,
        derivation_inputs: DerivationInputs,
        derivation_outputs: DerivationOutputs
    ):
        #################################################################
        ## Firstly, retrieve the input data from the derivation_inputs ##
        #################################################################
        try:
            max_value_iri = derivation_inputs.getIris(dm.DERIVATION_AGENT_PYTHON_EXAMPLE_MAXVALUE)[0]
            min_value_iri = derivation_inputs.getIris(dm.DERIVATION_AGENT_PYTHON_EXAMPLE_MINVALUE)[0]
        except Exception as e:
            self.logger.error(e)
            raise Exception(f"Input data is not correctly marked up, derivation inputs: {derivation_inputs.getInputs()}") from e

        self.logger.info("Derivation inputs are valid.")

        ##############################################################################################
        ## Secondly, query the knowledge graph for the input data and convert it to your data model ##
        ##############################################################################################
        max_value = self.sparql_client.get_max_value(max_value_iri)
        min_value = self.sparql_client.get_min_value(min_value_iri)

        ##############################################################################
        ## Thirdly, perform the actual computation and return the resulting triples ##
        ##############################################################################
        # NOTE It is recommended to construct the output triples using the rdflib.Graph object
        # then all the output triples can be added to the derivation_outputs in one-go using addGraph method
        g = self.perform_computation(max_value, min_value)

        ###############################################################
        ## Finally, add the output triples to the derivation_outputs ##
        ###############################################################
        derivation_outputs.addGraph(g)

    def perform_computation(self, max_value: dm.MaxValue, min_value: dm.MinValue) -> Graph:
        diff_num_val = max_value.hasValue.numVal - min_value.hasValue.numVal
        return self.sparql_client.create_difference(diff_num_val=diff_num_val)


# Show an instructional message at the agent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is a derivation agent that serve as an example of pyderivationagent package.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DerivationAgentPythonExample#readme<BR>"
    return msg
