################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to instantiate/update the average square metre 
# price of properties for a postcode based on instantiated HM Land Registry's Price
# Paid Data transactions in the KG (using asynchronous derivation framework)

import uuid
import pandas as pd

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs
from rdflib import Graph

from avgsqmpriceagent.datamodel.iris import *
from avgsqmpriceagent.datamodel.data import TIME_FORMAT_LONG, TIME_FORMAT_SHORT
from avgsqmpriceagent.errorhandling.exceptions import TSException
from avgsqmpriceagent.kg_operations.kgclient import KGClient
from avgsqmpriceagent.kg_operations.tsclient import TSClient


class AvgSqmPriceAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Set transaction threshold for average calculation
        self.threshold = kwargs.pop('threshold')
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [OBE_POSTALCODE, OBE_PROPERTY_PRICE_INDEX,
                LRPPI_TRANSACTION_RECORD]


    def agent_output_concepts(self) -> list:
        # Output concept (i.e. result) of the Derivation
        return [OBE_AVERAGE_SM_PRICE]


    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)


    def validate_input_values(self, inputs, derivationIRI=None):
        """
        Check whether received input values are suitable to perform average price
        estimation. Throw exception if data is not suitable.
        -> relevant for asynchronous derivation

        Arguments:
            inputs {dict} -- Dictionary of inputs with input concepts as keys and values as list
            derivationIRI {str} -- IRI of the derivation instance (optional)

        Returns:
            postcode_iri {str}, ppi_iri {str}, tx_records {list}
        """

        # Check whether postcode is available
        if inputs.get(OBE_POSTALCODE):
            pc = inputs.get(OBE_POSTALCODE)
            # Check whether only one postcode has been provided
            if len(pc) == 1:
                postcode_iri = pc[0]
            else:
                self.logger.error(f"Derivation {derivationIRI}: More than one Postcode IRI provided.")
                raise Exception(f"Derivation {derivationIRI}: More than one Postcode IRI provided.")                
        else:
            self.logger.error(f"Derivation {derivationIRI}: Postcode IRI is missing.")
            raise Exception(f"Derivation {derivationIRI}: Postcode IRI is missing.")

        # Check whether property price index is available
        if inputs.get(OBE_PROPERTY_PRICE_INDEX):
            ppi = inputs.get(OBE_PROPERTY_PRICE_INDEX)
            # Check whether only one property price index has been provided
            if len(ppi) == 1:
                ppi_iri = ppi[0]
            else:
                self.logger.error(f"Derivation {derivationIRI}: More than one Property Price Index IRI provided.")
                raise Exception(f"Derivation {derivationIRI}: More than one Property Price Index IRI provided.")
        else:
            self.logger.error(f"Derivation {derivationIRI}: Property Price Index IRI is missing.")
            raise Exception(f"Derivation {derivationIRI}: Property Price Index IRI is missing.")


        # Check whether previous transactions are available
        if inputs.get(LRPPI_TRANSACTION_RECORD):
            tx_iris = inputs.get(LRPPI_TRANSACTION_RECORD)
        else:
            # Return empty list (instead of None) for non available previous transactions
            tx_iris = []
            self.logger.info(f"Derivation {derivationIRI}: No previous property sales transactions available for postcode.")

        return postcode_iri, ppi_iri, tx_iris

    
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        """
            This method takes 
                1 IRI of OntoBuiltEnv:PostalCode
                1 IRI of OntoBuiltEnv:PropertyPriceIndex
                list of IRIs of LRPPI:TransactionRecord
            and generates
                1 IRI of OntoBuiltEnv:AveragePricePerSqm
                (actually, this includes an entire set of triples due to ontology
                 of units of measure representation of OBE:AveragePricePerSqm)
        """

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        postcode_iri, ppi_iri, tx_records = self.validate_input_values(inputs=inputs,
                                                                       derivationIRI=derivIRI)
        
        # Assess average price per sqm in case all required inputs are available
        # (i.e. all inputs have been marked up successfully)
        g = self.estimate_average_square_metre_price(postcode_iri=postcode_iri,
                                                     ppi_iri=ppi_iri,
                                                     tx_records=tx_records)        
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)


    def estimate_average_square_metre_price(self, postcode_iri: str, ppi_iri: str,
                                            tx_records: list = []):
        """
        Retrieves instantiated sales transaction data for provided transaction record
        IRIs and calculates the average square metre price for the postcode

        Arguments:
            postcode_iri - IRI of postcode for which to estimate average square metre price
            tx_records - list of sales transaction IRIs (within postcode) which to consider
                        when estimating average square metre price
            ppi_iri - Property Price Index IRI of local authority associated with postcode
        Returns:
            Current average square metre price for postcode    
        """

        
        # Initialise TS client
        ts_client = TSClient(kg_client=self.sparql_client)

        # Initialise average square metre price instance
        # NOTE Derivation Framework faces issues if instance IRI is already associated 
        # with another derivation --> create new derivation instance IRI for each update;
        # The framework ensures that the "outdated" IRI is replaced with this new one
        avgsm_price_iri = KB + 'AveragePricePerSqm_' + str(uuid.uuid4())

        # In case less than `threshold` transactions are provided/available 
        # for current postcode, include transactions from nearby postcodes
        if len(tx_records) < self.threshold:
            tx_records = self.get_transactions_from_nearest_postcodes(postcode_iri, self.threshold)

        if tx_records:
            # 1) Retrieve representative UK House Price Index and parse as Series (i.e. unwrap Java data types)
            # UKHPI was set at a base of 100 in January 2015, and reflects the change in value of residential property since then
            # (https://landregistry.data.gov.uk/app/ukhpi/doc)
            try:
                # Retrieve time series in try-with-resources block to ensure closure of RDB connection
                with ts_client.connect() as conn:
                    ts = ts_client.tsclient.getTimeSeries([ppi_iri], conn)
                dates = [d.toString() for d in ts.getTimes()]
                values = [v for v in ts.getValues(ppi_iri)]
            except Exception as ex:
                self.logger.error('Error retrieving/unwrapping Property Price Index time series')
                raise TSException('Error retrieving/unwrapping Property Price Index time series') from ex

            # Create UKHPI series with conditioned date index
            ukhpi = pd.Series(index=dates, data=values)
            ukhpi.index = pd.to_datetime(ukhpi.index, format=TIME_FORMAT_LONG)
            ukhpi.sort_index(ascending=False, inplace=True)
            ukhpi.index = ukhpi.index.strftime(TIME_FORMAT_SHORT)

            # 2) Retrieve sales transaction details for transaction IRIs
            res = self.sparql_client.get_tx_details_and_floor_areas(tx_records)
            cols = ['tx_iri', 'price', 'date', 'floor_area']
            df = pd.DataFrame(columns=cols, data=res)
            # Ensure date index and value columns are in correct format
            df['date'] = pd.to_datetime(df['date'], format=TIME_FORMAT_LONG)
            df['date'] = df['date'].dt.strftime(TIME_FORMAT_SHORT)
            df['price'] = df['price'].astype(float)
            df['floor_area'] = df['floor_area'].astype(float)

            # 3) Calculate current square metre price for all transactions
            # Assess original square metre price
            df['avg_orig'] = df['price'] / df['floor_area']
            # Assess current square metre price
            df['ukhpi_orig'] = df['date'].map(ukhpi)
            df['ukhpi_curr'] = ukhpi[0]
            df['avg_curr'] = df['avg_orig'] / df['ukhpi_orig'] * df['ukhpi_curr']

            # Assess average current square metre price
            avg = round(df['avg_curr'].mean())
    
            # Create instantiation/update triples
            triples = self.sparql_client.instantiate_average_price(avg_price_iri=avgsm_price_iri,
                                                postcode_iri=postcode_iri,
                                                avg_price=avg)
        
        else:
            # In case no transactions are available for current (and nearby) postcode,
            # instantiate AveragePricePerSqm instance (and attached Measure instance)
            # with RDFS comment that value is not computable
            # NOTE: This design is intended to ensure that no errors are experienced
            # in case of non-computable values (i.e. when requested from downstream derivation)
            triples = self.sparql_client.instantiate_unavailable_average_price(avg_price_iri=avgsm_price_iri,
                                                postcode_iri=postcode_iri)
        
        # Create rdflib graph with update triples
        g = Graph()
        update_query = f'INSERT DATA {{ {triples} }}'
        g.update(update_query)

        return g


    def get_transactions_from_nearest_postcodes(self, postcode_iri:str, threshold:int):
        """
            Retrieves postcodes in same Super Output Area from ONS API, queries the
            instantiated number of transactions for them from the KG and orders them
            by increasing distance from target pc - returns the closest postcodes
            required to reach the threshold of transactions 
            
            Arguments:
                postcode_iri (str): Postcode IRI instantiated per OntoBuiltEnv
                threshold (int): Minimum number of transactions wanted

            Returns:
                List of sales transaction IRIs
        """

        # Retrieve postcode string from KG
        postcode_str = self.sparql_client.get_postcode_strings([postcode_iri])[0]

        # Retrieve nearby postcodes with easting and northing from ONS endpoint
        data = self.sparql_client.get_nearby_postcodes(postcode_str)
        df = pd.DataFrame(data['results']['bindings'])
        for c in df.columns:
            df[c] = df[c].apply(lambda x: x['value'])
        # Set postcode as index and ensure float coordinate values
        df.set_index('pc', inplace=True)
        df = df.astype(float)

        # Set target postcode as reference point for distance calculation
        pref = df.loc[postcode_str]
        # Assess distance from target postcode
        df['dist'] = df.apply(lambda x: self.euclidean_distance(x, pref), axis=1)

        # Retrieve number of available transactions per postcode
        tx_map = self.sparql_client.get_tx_count_for_postcodes(df.index.to_list())
        df['tx_count'] = df.index.map(tx_map)
        df.dropna(inplace=True)
        df['tx_count'] = df['tx_count'].astype(int)    
        # Keep only nearest postcodes required to reach `threshold` transactions
        df.sort_values(by='dist', inplace=True)
        df['total_tx'] = df['tx_count'].cumsum()
        df['helper'] = df['total_tx'].shift(1)
        df.fillna(0, inplace=True)
        df = df.loc[(df['helper'] < threshold)]

        # Retrieve transaction IRIs for determined postcodes
        pc_iris = self.sparql_client.get_postcode_iris(df.index.tolist())
        txns = self.sparql_client.get_tx_iris_for_postcodes(pc_iris)

        return txns


    def euclidean_distance(self, pointA, pointB):
        return (
        ((pointA[0] - pointB[0]) ** 2) +
        ((pointA[1] - pointB[1]) ** 2)
    ) ** 0.5  # fast sqrt


def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent to calculate the average square metre price of properties per postcode.<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AverageSquareMetrePriceAgent<BR>"
    return msg
