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
from resultedconsumptioncalculationagent.kg_operations.kgclient import KGClient, COP_VAR
from resultedconsumptioncalculationagent.kg_operations.tsclient import TSClient

class ResultedConsumptionAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)

    def agent_input_concepts(self) -> list:
        return [REGION_COP, OM_MEASURE, 
                REGION_PROPORTION_OF_HEATING, REGION_BOILER_EFFICIENCY, 
                REGION_UPTAKE, REGION_ENERGYCONSUMPTION_PROFILE]
    
    def agent_output_concepts(self) -> list:
        return [REGION_RESULTED_ENERGYCONSUMPTION]
    
    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
    
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        # Update assumptions provided
        try: 
            with open('state.txt', "r") as file:
                has_function_run = file.read().strip() == "True"
        except:
            has_function_run = False
            with open('state.txt', "w") as file:
                file.write("False")  # Initialize the file with "False"
                print("Assumptions/Indecies has been updated!")

        if not has_function_run:
            # Update assumptions provided
            self.sparql_client.update_assumptions()
            # Update consumption profiles provided#
            self.sparql_client.update_consumption_profile()
            with open('state.txt', "w") as file:
                file.write("True")

        # ---------------- Get Input IRIs --------------- #
        inputs = derivation_inputs.getInputs()
        cop_iri_list = inputs[REGION_COP]
        consumption_iri_list = inputs[OM_MEASURE]
        proportion_of_heating_iri = inputs[REGION_PROPORTION_OF_HEATING][0]
        boiler_efficiency_iri = inputs[REGION_BOILER_EFFICIENCY][0]
        uptake_iri = inputs[REGION_UPTAKE][0]
        consumption_profile_iri = inputs[REGION_ENERGYCONSUMPTION_PROFILE][0]
        for a in consumption_iri_list:
            if "Electricity" in a:
                elec_consumption_iri = a
            if "Gas" in a:
                gas_consumption_iri = a
            else:
                self.logger.error(f"Specified {COP_VAR} COP IRI can not be found")
                raise Exception(f"Specified {COP_VAR} COP IRI can not be found")
            
        for a in cop_iri_list:
            if COP_VAR in a:
                cop_iri = a
            else:
                self.logger.error(f"Specified {COP_VAR} COP IRI can not be found")
                raise Exception(f"Specified {COP_VAR} COP IRI can not be found")

        # ---------------- Populate Triples for Non-Timeseries in the KG --------------- #
        g, resulted_consumption_iri, resulted_elec_consumption_iri, resulted_gas_consumption_iri = self.getconsumptionprofile_Graph(cop_iri)

        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)
        
        # ---------------- Perform Calculation and populate Timeseries data ------------- #
        # Initialise variables to be added to the TS client
        dates = []
        resulted_gas_consumption_list = []
        resulted_elec_consumption_list = []
        # Initialise TS client
        ts_client = TSClient(kg_client=self.sparql_client)

        # retrieve input values
        elec_consumption = self.sparql_client.get_consumption(elec_consumption_iri)
        gas_consumption = self.sparql_client.get_consumption(gas_consumption_iri)
        proportion_of_heating = self.sparql_client.get_proportion_of_heating(proportion_of_heating_iri)
        boiler_efficiency = self.sparql_client.get_boiler_efficiency(boiler_efficiency_iri)
        uptake = self.sparql_client.get_uptake(uptake_iri)
        elec_profile, gas_profile = self.sparql_client.get_consumption_profile(consumption_profile_iri)
        cop_dict = ts_client.retrieve_data(cop_iri)

        for key, value in cop_dict.items():
            # Calculate original consumption of the current month
            monthly_gas_consumption = gas_consumption * gas_profile[date_dict(key)] / sum(gas_profile)
            monthly_elec_consumption = elec_consumption * elec_profile[date_dict(key)] / sum(elec_profile)
            
            # Calculate change of consumptions
            delta_gas = self.delta_gas(uptake, monthly_gas_consumption,proportion_of_heating)
            delta_elec = self.delta_elec(delta_gas, value, boiler_efficiency)

            # Calculate resulted consumptions
            resulted_gas_consumption = monthly_gas_consumption - delta_gas
            resulted_elec_consumption = monthly_elec_consumption + delta_elec
            dates.append(key)
            resulted_gas_consumption_list.append(resulted_gas_consumption)
            resulted_elec_consumption_list.append(resulted_elec_consumption)

        dataIRIs = [resulted_elec_consumption_iri, resulted_gas_consumption_iri]
        values = [resulted_gas_consumption_list, resulted_elec_consumption_list]
        
        # Create Time Series
        ts_client.add_timeseries(dates, dataIRIs, values)
        
        print('Resulted Consumption has been updated!')

    def getconsumptionprofile_Graph(self, cop_iri):
        
        # Initialise results and return triples
        g = Graph()
        
        # retrieve input values
        region = self.sparql_client.get_region(cop_iri)

        # Verify iri
        resulted_consumption_iri, resulted_elec_consumption_iri, resulted_gas_consumption_iri, deletion = self.sparql_client.verify_resulted_consumption_iri(region)
        
        if deletion:
            # Initialise TS client
            ts_client = TSClient(kg_client=self.sparql_client)
            # Remove TimeSeries
            ts_client.delete_timeseries(resulted_elec_consumption_iri)
            ts_client.delete_timeseries(resulted_gas_consumption_iri)
            self.sparql_client.remove_triples_for_iri(resulted_consumption_iri, resulted_elec_consumption_iri, resulted_gas_consumption_iri)
        
        # Instantisation
        g = self.sparql_client.instantiate_resulted_consumptions(g, 
                                                                resulted_consumption_iri, 
                                                                resulted_elec_consumption_iri,
                                                                resulted_gas_consumption_iri,
                                                                region)

        return g, resulted_consumption_iri, resulted_elec_consumption_iri, resulted_gas_consumption_iri
    
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

