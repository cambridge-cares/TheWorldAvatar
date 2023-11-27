################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2023                            #
################################################

# The purpose of this module is to provide the EmissionAgent class based on 
# the pyderivationagent.DerivationAgent class, i.e., implementing the emission
# agent as derivation agent using synchronous derivation

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from emissionagent.datamodel.iris import *
from emissionagent.kgutils.kgclient import KGClient
from emissionagent.kgutils.tsclient import TSClient
from emissionagent.agent.emission_estimation import *
from emissionagent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD


class EmissionAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Specify emission types to be considered
        # NOTE: NO2 is currently not supported by Aermod; hence, NOx is used instead
        self.POLLUTANTS = [OD_NOX, OD_PM2_5, OD_PM10]
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [OD_SIMULATION_TIME, OD_STATIC_POINT_SOURCE,
                OHN_PROVIDED_HEAT_AMOUNT, OHN_CONSUMED_GAS_AMOUNT]


    def agent_output_concepts(self) -> list:
        # Output concepts (i.e., results) of the Derivation
        return [OD_EMISSION]


    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)


    def validate_input_values(self, inputs, derivationIRI=None):
        """
        Check whether received input instances are suitable to derive emissions.
        Throw exception if data is not suitable.

        Arguments:
            inputs {dict} -- Dictionary of inputs with input concepts as keys and values as list
            derivationIRI {str} -- IRI of the derivation instance (optional)

        Returns:
            dictionary of validated input IRIs as key-value pairs with full concept IRIs
            as keys and values as lists of (single) values
        """
        
        print(f'Validating inputs for derivation {derivationIRI} ...')
        
        # Create dict between input concepts and return values
        input_iris = {
            OD_SIMULATION_TIME: None,
            OD_STATIC_POINT_SOURCE: None
        }

        # 1) Verify that exactly one instance per required concept is provided
        for i in [OD_SIMULATION_TIME, OD_STATIC_POINT_SOURCE]:
            inp_name = i[i.rfind('/')+1:]
            inp_name = inp_name[inp_name.rfind('#')+1:]
            # Check whether input is available
            if not inputs.get(i):
                msg = f"Derivation {derivationIRI}: No '{inp_name}' IRI provided."
                self.logger.error(msg)
                raise TypeError(msg)
            else:
                inp = inputs.get(i)
                # Check whether only one input has been provided
                if len(inp) == 1:
                    input_iris[i] = inp
                else:
                    self.logger.error(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")
                    raise TypeError(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")

        # 2) Verify that either 1 ProvidedHeat (heat sourced from energy from waste plant) 
        #    or at least 1 ConsumedGas instance (heat generated from gas combustion) is provided
        # NOTE: a list of consumed gas instances can be provided to account for multiple 
        #       gas boilers and gas turbine emitting through the same chimney
            
        # Extract lists of consumed gas and provided heat instances
        provided_heat = inputs.get(OHN_PROVIDED_HEAT_AMOUNT)
        consumed_gas = inputs.get(OHN_CONSUMED_GAS_AMOUNT)
        # Create empty lists in case no instances have been marked up
        provided_heat = [] if provided_heat is None else provided_heat
        consumed_gas = [] if consumed_gas is None else consumed_gas

        if provided_heat and consumed_gas:
            msg = f"Derivation {derivationIRI}: Both 'ProvidedHeatAmount' and 'ConsumedGasAmount' instances provided."
            self.logger.error(msg)
            raise TypeError(msg)
        if not provided_heat and not consumed_gas:
            msg = f"Derivation {derivationIRI}: Neither 'ProvidedHeatAmount' nor 'ConsumedGasAmount' instances provided."
            self.logger.error(msg)
            raise TypeError(msg)
        if provided_heat:                
            if len(provided_heat) > 1:
                msg = f"Derivation {derivationIRI}: More than one 'ProvidedHeatAmount' instance provided."
                self.logger.error(msg)
                raise TypeError(msg)
            else:
                input_iris[OHN_PROVIDED_HEAT_AMOUNT] = provided_heat

        if consumed_gas:
                input_iris[OHN_CONSUMED_GAS_AMOUNT] = consumed_gas

        print('Inputs successfully validated.')

        return input_iris


    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        """
        This method takes 
            1 IRI of OntoDisp:StaticPointSource (instance representing the emission
                    location, i.e., via attached OntoCityGML CityObject)
            1 IRI of OntoDisp:SimulationTime (time instant for which to estimate emissions)
            1 IRI of OntoHeatNet:ProvidedHeatAmount (instance associate with time series
                     of heat provided by energy from waste plant)
            1 List of OntoHeatNet:ConsumedGasAmount IRIs (instances associate with time
                      series of gas consumed by gas boiler or gas turbine)
            NOTE: either 1 ProvidedHeatAmount OR at least 1 ConsumedGasAmount instance
                  must be provided, not both 
        
        and generates
            Set of OntoDisp:Emission instances (incl. further relationships detailing
            emission values, i.e, temperature (K), density (kg/m3), mass flow rate (kg/s)
            NOTE: SI units are required by Aermod agent for dispersion modelling
        """

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        # Get validated inputs still as key-value pairs with lists
        input_iris = self.validate_input_values(inputs=inputs, derivationIRI=derivIRI)

        # Query all dataIRIs for emission estimation calculation (i.e., required
        # to retrieve time series data) -> create consolidated list as either
        # only ProvidedHeatAmount or ConsumedGasAmount instances will be provided
        dataIRIs = []
        iris = [input_iris.get(OHN_PROVIDED_HEAT_AMOUNT), input_iris.get(OHN_CONSUMED_GAS_AMOUNT)]
        iris = [iri for sublist in iris if sublist is not None for iri in sublist]
        for iri in iris:
            data_iri, _ = self.sparql_client.get_associated_dataIRI(instance_iri=iri, 
                                                                    unit=OM_MEGAWATTHOUR)
            dataIRIs.append(data_iri)

        # Retrieve and consolidate time series data for dataIRIs
        ts_client = TSClient(kg_client=self.sparql_client, rdb_url=DB_URL, 
                             rdb_user=DB_USER, rdb_password=DB_PASSWORD)
        amount = extract_relevant_gas_or_heat_amount(ts_client=ts_client, 
                        kg_client=self.sparql_client, derivationIRI=derivIRI, 
                        simulation_time_iri=input_iris[OD_SIMULATION_TIME][0],
                        dataIRIs=dataIRIs)

        # Estimate associated emissions for each pollutant of interest
        emissions = []
        for pollutant in self.POLLUTANTS:
            if input_iris.get(OHN_PROVIDED_HEAT_AMOUNT):
                print(f'Estimating {pollutant} emissions from EfW plant sourcing.')
                # Estimate emissions from EfW plant sourcing, i.e.,
                # amount refers to amount of sourced heat
                emissions.append(calculate_emissions_for_provided_heat(pollutant, 
                                                                       provided_heat=amount))
            else:
                print(f'Estimating {pollutant} emissions from natural gas burning.')
                # Estimate emissions from natural gas burning, i.e.,
                # amount refers to amount of consumed gas (wrt lower calorific value)
                emissions.append(calculate_emissions_for_consumed_gas(pollutant, 
                                                                      consumed_gas=amount))

        # Create graph of emission instance triples and add to derivation outputs
        g = self.sparql_client.instantiate_emissions(location_iri=input_iris[OD_STATIC_POINT_SOURCE][0],
                                                     emissions=emissions)
        derivation_outputs.addGraph(g)
        

def default():
    """
    Instructional message at the agent root.
    """

    msg = '<B>District Heating Emission Estimation Agent</B>:<BR><BR>'
    msg += 'This emission estimation agent "converts" instantiated (time series) data for '
    msg += '1) burned natural gas amounts by conventional gas boilers and a CHP gas turbine or '
    msg += '2) generated heat amounts by an energy from waste plant into corresponding emission '
    msg += 'values for certain emission types (i.e., PM2.5, PM10, NOx).<BR>'
    msg += "The agent is implemented as derivation agent using synchronous derivation."
    msg += "<BR><BR>"
    msg += 'For further details please see the <a href="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingEmissionEstimationAgent/">District Heating Emission Estimation Agent README</a>.'
    return msg
