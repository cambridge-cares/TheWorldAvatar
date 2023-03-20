################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Nov 2022                            #
################################################

# The purpose of this module is to instantiate/update the estimated market value of a property
# based on instantiated HM Land Registry's Price Paid Data transactions and/or the 
# average square metre price per postcode in the KG (using asynchronous derivation framework)

import uuid
import pandas as pd
import traceback
from rdflib import Graph

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from propertyvalueestimation.datamodel.iris import *
from propertyvalueestimation.datamodel.data import TIME_FORMAT_LONG, TIME_FORMAT_SHORT
from propertyvalueestimation.errorhandling.exceptions import TSException
from propertyvalueestimation.kg_operations.kgclient import KGClient
from propertyvalueestimation.kg_operations.tsclient import TSClient


class PropertyValueEstimationAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [LRPPI_TRANSACTION_RECORD, OBE_PROPERTY_PRICE_INDEX,
                OBE_AVERAGE_SM_PRICE, OM_AREA]


    def agent_output_concepts(self) -> list:
        # Output concept (i.e. result) of the Derivation
        return [OM_AMOUNT_MONEY]


    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)


    def validate_input_values(self, inputs, derivationIRI=None):
        """
        Check whether received input values are suitable to perform property value
        estimation. Throw exception if data is not suitable.
        -> relevant for asynchronous derivation

        Arguments:
            inputs {dict} -- Dictionary of inputs with input concepts as keys and values as list
            derivationIRI {str} -- IRI of the derivation instance (optional)

        Returns:
            transaction_iri {str}, prop_price_index_iri  {str}, 
            avgsqm_price_iri {str}, floor_area_iri {str}
        """

        # Create dict between input concepts and return values
        input_dict = {LRPPI_TRANSACTION_RECORD: None,
                      OBE_PROPERTY_PRICE_INDEX: None,
                      OBE_AVERAGE_SM_PRICE: None,
                      OM_AREA: None}

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

        # Verify that either
        # 1) TransactionRecord & PropertyPriceIndex or
        # 2) AveragePricePerSqm & TotalFloorArea are provided
        if not ((input_dict[LRPPI_TRANSACTION_RECORD] and input_dict[OBE_PROPERTY_PRICE_INDEX]) or \
                (input_dict[OBE_AVERAGE_SM_PRICE] and input_dict[OM_AREA])):
            self.logger.info(f"Derivation {derivationIRI}: Insufficient set of inputs provided.")

        return input_dict[LRPPI_TRANSACTION_RECORD], input_dict[OBE_PROPERTY_PRICE_INDEX], \
               input_dict[OBE_AVERAGE_SM_PRICE], input_dict[OM_AREA] 

    
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        """
            This method takes 
                1 IRI of LRPPI:TransactionRecord & 1 IRI of OntoBuiltEnv:PropertyPriceIndex
                or 
                1 IRI of OntoBuiltEnv:AveragePricePerSqm & 1 IRI of OM:Area
            to assess the estimated value of a property and generate
                1 IRI of OM:AmountOfMoney
                (actually, this includes an entire set of triples due to ontology
                 of units of measure representation of OM:AmountOfMoney)
        """

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        tx_iri, ppi_iri, avgsqm_iri, area_iri = self.validate_input_values(inputs=inputs,
                                                    derivationIRI=derivIRI)
        
        # Assess property value estimate in case all required inputs are available
        # (i.e. relevant inputs have been marked up successfully)
        g = self.estimate_property_market_value(transaction_iri=tx_iri,
                                                prop_price_index_iri=ppi_iri, 
                                                avgsqm_price_iri=avgsqm_iri, 
                                                floor_area_iri=area_iri)        

        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)


    def estimate_property_market_value(self, transaction_iri:str = None,
                                       prop_price_index_iri:str = None, 
                                       avgsqm_price_iri:str = None, 
                                       floor_area_iri:str = None):
        """
        Estimate market value of property (i.e. building or flat) based on given inputs.
        Prio1: LRPPI:TransactionRecord & OntoBuiltEnv:PropertyPriceIndex
        Prio2: OntoBuiltEnv:AveragePricePerSqm & OM:Area

        Arguments:
            transaction_iri {str} - IRI of LRPPI:TransactionRecord
            prop_price_index_iri {str} - IRI of OntoBuiltEnv:PropertyPriceIndex
            avgsqm_price_iri {str} - IRI of OntoBuiltEnv:AveragePricePerSqm
            floor_area_iri {str} - IRI of OM:Area
        Returns:
            Graph to instantiate/update property market value
        """

        # Initialise market value and return Graph
        market_value = None
        g = Graph()

        # Initialise property value estimate instance
        market_value_iri = KB + 'AmountOfMoney_' + str(uuid.uuid4())

        try:
            # Prio 1: Check if transaction record and property price index are provided
            # (i.e. market value assessment based on previous transaction)
            if transaction_iri and prop_price_index_iri:
                # Initialise TS client
                ts_client = TSClient(kg_client=self.sparql_client)
                # 1) Retrieve representative UK House Price Index and parse as Series (i.e. unwrap Java data types)
                # UKHPI was set at a base of 100 in January 2015, and reflects the change in value of residential property since then
                # (https://landregistry.data.gov.uk/app/ukhpi/doc)
                try:
                    # Retrieve time series in try-with-resources block to ensure closure of RDB connection
                    with ts_client.connect() as conn:
                        ts = ts_client.tsclient.getTimeSeries([prop_price_index_iri], conn)
                    dates = [d.toString() for d in ts.getTimes()]
                    values = [v for v in ts.getValues(prop_price_index_iri)]
                except Exception as ex:
                    self.logger.error('Error retrieving/unwrapping Property Price Index time series')
                    raise TSException('Error retrieving/unwrapping Property Price Index time series') from ex

                # Create UKHPI series with conditioned date index
                ukhpi = pd.Series(index=dates, data=values)
                ukhpi = ukhpi.astype(float)
                ukhpi.index = pd.to_datetime(ukhpi.index, format=TIME_FORMAT_LONG)
                ukhpi.sort_index(ascending=False, inplace=True)
                ukhpi.index = ukhpi.index.strftime(TIME_FORMAT_SHORT)

                # 2) Retrieve previous sales transaction details for previous transaction IRI
                #    and adjust to current market value
                res = self.sparql_client.get_transaction_details(transaction_iri)
                ukhpi_now = ukhpi.iloc[0]
                ukhpi_old = ukhpi[res['date']]
                market_value = res['price'] * ukhpi_now / ukhpi_old

            # Prio 2: Otherwise assess market value based on FloorArea and AveragePricePerSqm
            elif avgsqm_price_iri and floor_area_iri and not market_value:
                # NOTE: To ensure availability of AvgSqmPrice (i.e. derivation being computed by
                #       AvgSqmPrice Agent), AvgSqmPrice initially marked up as Synchronous Derivation
                res = self.sparql_client.get_floor_area_and_avg_price(floor_area_iri)
                # NOTE: This will cause an error if no AvgSqmPrice (i.e. actual numerical value)
                #       (or floor area) is available
                market_value = res['floor_area'] * res['avg_price']
        
        except Exception as ex:
            # Catch and log exception, but do not re-raise and instantiate non-computable
            # property estimate value instead
            self.logger.error('Error estimating property market value: {}'.format(ex))
            self.logger.error(traceback.format_exc())

        if market_value:
            # Round property market value to full kGBP
            market_value = round(market_value/1000)*1000
            # Create rdflib graph with update triples 
            g = self.sparql_client.instantiate_property_value(graph=g,
                                                property_iri=res['property_iri'],
                                                property_value_iri=market_value_iri, 
                                                property_value=market_value)
        else:
            if not res['property_iri']:
                res = self.sparql_client.get_property_iri(tx_iri=transaction_iri, 
                                                          floor_area_iri=floor_area_iri)
            # In case PropertyValueEstimate cannot be assessed, instantiate AmountOfMoney
            # (and attached Measure) instance with RDFS comment that value is not computable
            # NOTE: This design is intended to ensure that no errors are experienced
            # in case of non-computable values (i.e. when requested from downstream derivation)
            g = self.sparql_client.instantiate_unavailable_property_value(graph=g,
                                                property_iri=res['property_iri'],
                                                property_value_iri=market_value_iri)

        # Return graph with SPARQL update
        return g


def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent to estimate the market value of a particular property (i.e. building, flat).<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PropertyValueEstimationAgent<BR>"
    return msg
