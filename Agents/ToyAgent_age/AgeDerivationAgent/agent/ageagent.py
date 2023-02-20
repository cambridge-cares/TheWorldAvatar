
import uuid
from rdflib import Graph
import datetime

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from AgeDerivationAgent.datamodel.iris import *
from AgeDerivationAgent.kg_operations.kgclient import KGClient


class AgeAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)

    def agent_input_concepts(self) -> list:
        return [EX_BIRTHDAY]
    
    def agent_output_concepts(self) -> list:
        return [XSD_INTEGER]
    
    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
    
    def validate_input_values(self, inputs, derivationIRI=None):
        
        # Create dict between input concepts and return values
        input_dict = {EX_BIRTHDAY:None}

        # Verify that max. one instance per concept is provided
        for i in input_dict:
            # Check whether input is available
            if inputs.get(i):
                inp = inputs.get(i)
                # Check whether only one input has been provided
                if len(inp) == 1:
                    input_dict[i] = inp[0]
                else:
                    inp_name = i[i.rfind('/')+1:]
                    self.logger.error(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")
                    raise Exception(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")
            else: 
                self.logger.info(f"Derivation {derivationIRI}: Insufficient set of inputs provided.")
            return input_dict[EX_BIRTHDAY]
        
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        birthday_iri = self.validate_input_values(inputs=inputs,
                                                    derivationIRI=derivIRI)
        
        g = self.getage(birthday_iri)
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)

    def getage(self, birthday_iri):
        age = None
        g = Graph()
        res = self.sparql_client.get_birthday(birthday_iri)
        
        current_time = datetime.datetime.now()
        date = datetime.datetime.strptime(res['birth'], '%Y-%m')
        age = current_time.year - date.year
        if (current_time.month, current_time.day) < (date.month, date.day):
            age -= 1
        if age:
            age_iri = AGE_ID + 'Age_' + str(uuid.uuid4())

        g = self.sparql_client.instantiate_age(g, res['personiri'], age_iri, age)

        return g

def default():
    """
        Instructional message at the app root.
    """
    # TODO: Update path to main upon merging
    msg  = "This is an toy agent.<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PropertyValueEstimationAgent<BR>"
    return msg
