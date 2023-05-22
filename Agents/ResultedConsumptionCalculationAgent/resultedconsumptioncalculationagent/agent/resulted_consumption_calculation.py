################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################
import uuid
from rdflib import Graph
import datetime

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from resultedconsumptioncalculationagent.datamodel.iris import *
from resultedconsumptioncalculationagent.datamodel.data import date_dict
from resultedconsumptioncalculationagent.kg_operations.kgclient import KGClient

class ResultedConsumptionAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)

    def agent_input_concepts(self) -> list:
        return [OM_MEASURE, OM_MEASURE, OM_MEASURE, 
                REGION_PROPORTION_OF_HEATING, REGION_BOILER_EFFICIENCY, 
                REGION_UPTAKE, REGION_ENERGYCONSUMPTION_PROFILE]
    
    def agent_output_concepts(self) -> list:
        return [REGION_RESULTED_CONSUMPTION]
    
    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
    
    def validate_input_values(self, inputs, derivationIRI=None):
        
        # Create dict between input concepts and return values
        input_dict = {OM_MEASURE:[None,None,None], 
                      REGION_PROPORTION_OF_HEATING:None, 
                      REGION_BOILER_EFFICIENCY:None, 
                      REGION_UPTAKE:None, 
                      REGION_ENERGYCONSUMPTION_PROFILE:None
                      }
        
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
            return input_dict[OM_MEASURE][0],input_dict[OM_MEASURE][1],input_dict[OM_MEASURE][2], 
        input_dict[REGION_PROPORTION_OF_HEATING],input_dict[REGION_BOILER_EFFICIENCY],
        input_dict[REGION_UPTAKE],input_dict[REGION_ENERGYCONSUMPTION_PROFILE]
        
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        # Update assumptions provided
        self.sparql_client.update_assumptions()
        # Update consumption profiles provided#
        self.sparql_client.update_consumption_profile()

        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        cop_iri, elec_consumption_iri, gas_consumption_iri, proportion_of_heating_iri, boiler_efficiency_iri, uptake_iri, consumption_profile_iri= self.validate_input_values(inputs=inputs,
                                                    derivationIRI=derivIRI)
        
        g = self.getconsumptionprofile_Graph(cop_iri, elec_consumption_iri, gas_consumption_iri, proportion_of_heating_iri, boiler_efficiency_iri, uptake_iri, consumption_profile_iri)
        
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)

    def getconsumptionprofile_Graph(self,
                                    cop_iri, 
                                    elec_consumption_iri, 
                                    gas_consumption_iri, 
                                    proportion_of_heating_iri, 
                                    boiler_efficiency_iri, 
                                    uptake_iri, 
                                    consumption_profile_iri):
        
        # Initialise COP and return triples
        resulted_gas_consumption = None
        resulted_elec_consumption = None
        g = Graph()

        res = self.sparql_client.get_cop(cop_iri)
        elec_consumption = self.sparql_client.get_consumption(elec_consumption_iri)
        gas_consumption = self.sparql_client.get_consumption(gas_consumption_iri)
        proportion_of_heating = self.sparql_client.get_proportion_of_heating(proportion_of_heating_iri)
        boiler_efficiency = self.sparql_client.get_boiler_efficiency(boiler_efficiency_iri)
        uptake = self.sparql_client.get_uptake(uptake_iri)
        elec_profile, gas_profile = self.sparql_client.get_consumption_profile(consumption_profile_iri)
        
        # Calculate original consumption of the current month
        monthly_gas_consumption = gas_consumption * gas_profile[date_dict(res['start'])] / sum(gas_profile)
        monthly_elec_consumption = elec_consumption * elec_profile[date_dict(res['start'])] / sum(elec_profile)
        
        # Calculate change of consumptions
        delta_gas = self.delta_gas(uptake, monthly_gas_consumption,proportion_of_heating)
        delta_elec = self.delta_elec(delta_gas, res['value'],boiler_efficiency)

        # Calculate resulted consumptions
        resulted_gas_consumption = monthly_gas_consumption - delta_gas
        resulted_elec_consumption = monthly_elec_consumption + delta_elec
        
        # Verify iri
        consumption_iri, elec_consumption_iri, gas_consumption_iri = self.sparql_client.generate_resulted_consumption_iri(res['region'],
                                                                                                                          res['start'],
                                                                                                                          res['end'],
                                                                                                                          resulted_gas_consumption,
                                                                                                                          resulted_elec_consumption)
        # Instantisation
        g = self.sparql_client.instantiate_resulted_consumptions(g, 
                                                                consumption_iri, 
                                                                elec_consumption_iri,
                                                                gas_consumption_iri,
                                                                res['region'],
                                                                res['start'],
                                                                res['end'],
                                                                resulted_gas_consumption,
                                                                resulted_elec_consumption)
        
        print('Resulted Consumption has been updated!')

        return g
    
    def delta_gas(self, uptake: float, total_gas_consumption, propotion_heating: float = 0.9):
        '''
        Based on a given uptake, and gas consumption, calculate how many gas will be converted to electricity, which is delta_gas
        Note: the definition of uptake is (delta_gas / gas_for_heating) = (delta_gas / (Total_gas_consumption * propotion_heating))
            Therefore, delta_gas = uptake * Total_gas_consumption * propotion_heating
            propotion_heating is hypothesd as constant, which have default value as 0.9 I suggest to check if that default value is 
            up to date or if that hypothesis is valid in your case
        '''
        delta_gas = uptake * total_gas_consumption * propotion_heating

        return delta_gas

    def delta_elec(self, delta_gas, COP, boiler_efficiency: float = 0.8):
        '''
        Based on given COP, delta_gas to calculate how much electricity has been converted based on delta_gas
        Note: delta_elec = boiler_efficiency * delta_gas / COP. where boiler_efficiency is hypothesd as constant, 
        which have default value as 0.8. I suggest to check if that default value 
        is up to date or if that hypothesis is valid in your case
        '''
        delta_elec = boiler_efficiency * delta_gas / COP

        return delta_elec

def default():
    """
        Instructional message at the app root.
    """
    # TODO: Update path to main upon merging
    msg  = "This is an resulted energy consumption calculation agent.<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ResultedConsumptionCalculationAgent<BR>"
    return msg

agent_config = config_derivation_agent(env_file='./agent.env.example')

agent = ResultedConsumptionAgent(
    # Settings read from environment variables (.env file, docker-compose)
    register_agent=agent_config.REGISTER_AGENT,
    agent_iri=agent_config.ONTOAGENT_SERVICE_IRI, 
    time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
    derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
    agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
    # Settings read from Stack Clients
    kg_url=SPARQL_QUERY_ENDPOINT,
    kg_update_url=SPARQL_QUERY_ENDPOINT,      
    # Miscellaneous settings
    logger_name='dev',
    max_thread_monitor_async_derivations=1
)
