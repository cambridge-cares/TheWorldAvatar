################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to instantiate/update the average square metre 
# price of properties for a postcode based on instantiated HM Land Registry's Price
# Paid Data transactions in the KG (using asynchronous derivation framework)

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs
from pyderivationagent import FlaskConfig
from rdflib import Graph

import uuid
import pandas as pd

from avgsqmpriceagent.datamodel.iris import *
from avgsqmpriceagent.datamodel.data import TIME_FORMAT, DATACLASS
from avgsqmpriceagent.errorhandling.exceptions import TSException, APIException
from avgsqmpriceagent.kg_operations.kgclient import KGClient
from avgsqmpriceagent.kg_operations.tsclient import TSClient
from avgsqmpriceagent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT


class AvgSqmPriceAgent(DerivationAgent):

    def __init__(self,
        **kwargs
    ):
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in `agent.env` file)
        self.sparql_client = self.get_sparql_client(KGClient)

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [OBE_POSTALCODE, OBE_PROPERTY_PRICE_INDEX,
                LRPPI_TRANSACTION_RECORD]

    def agent_output_concepts(self) -> list:
        return [OBE_AVERAGE_SM_PRICE]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

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
        #TODO: Check if inputs are valid  
        derivation_inputs.getInputs()
        postcode_iri = derivation_inputs.getIris(OBE_POSTALCODE)[0]
        ppi_iri = derivation_inputs.getIris(OBE_PROPERTY_PRICE_INDEX)[0]
        tx_records = derivation_inputs.getIris(LRPPI_TRANSACTION_RECORD)

        # Assess average square metre price
        g = self.estimate_average_square_metre_price(postcode_iri=postcode_iri,
                                                     ppi_iri=ppi_iri,
                                                     tx_records=tx_records)
        
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)


    def estimate_average_square_metre_price(self, postcode_iri:str = None, 
                                            tx_records:list = None,
                                            ppi_iri:str = None):
        """
        Retrieves instantiated sales transaction data for provided transaction record
        IRIs and calculates the average square metre price for the postcode

        Arguments:
            postcode_iri - IRI of postcode for which to estimate average square metre price
            tx_records - list of sales transaction IRIs (within postcode) which to consider
                        when estimating average square metre price
            ppi_iri - Property Price Index IRI of local authority associated with postcode
            query_endpoint/update_endpoint - SPARQL endpoint with instantiated buildings
        Returns:
            Current average square metre price for postcode    
        """

        # Initialise return triples
        triples = None

        if postcode_iri and ppi_iri:
        
            # Initialise TS client
            #TODO:uncomment
            ts_client = TSClient(kg_client=self.sparql_client)

            # In case less than `threshold` transactions provided/available for current postcode,
            # include transactions from nearby postcodes
            threshold = 50
            if len(tx_records) < threshold:
                tx_records = self.get_transactions_from_nearest_postcodes(postcode_iri, threshold)

            if tx_records:
                # 1) Retrieve representative UK House Price Index and parse as Series (i.e. unwrap Java data types)
                # UKHPI was set at a base of 100 in January 2015, and reflects the change in value of residential property since then
                # (https://landregistry.data.gov.uk/app/ukhpi/doc)
                try:
                    #TODO: Uncomment and remove temporary workaround
                    # ts = ts_client.tsclient.getTimeSeries([ppi_iri], ts_client.conn)
                    # dates = [d.toString() for d in ts.getTimes()]
                    # values = [v for v in ts.getValues(ppi_iri)]
                    # fix:
                    dates = pd.date_range(start='1990-01-01', freq='M', end='2022-10-01')
                    dates = dates.strftime('%Y-%m').tolist()
                    values = [100 for d in dates]
                except Exception as ex:
                    self.logger.error('Error retrieving/unwrapping Property Price Index time series')
                    raise TSException('Error retrieving/unwrapping Property Price Index time series') from ex

                # Create UKHPI series with conditioned date index
                ukhpi = pd.Series(index=dates, data=values)
                ukhpi.index = pd.to_datetime(ukhpi.index, format='%Y-%m-%d')
                ukhpi.sort_index(ascending=False, inplace=True)
                ukhpi.index = ukhpi.index.strftime('%Y-%m')

                # 2) Retrieve sales transaction details for transaction IRIs
                res = self.sparql_client.get_tx_details_and_floor_areas(tx_records)
                cols = ['tx_iri', 'price', 'date', 'floor_area']
                df = pd.DataFrame(columns=cols, data=res)
                # Ensure date index and value columns are in correct format
                df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%d')
                df['date'] = df['date'].dt.strftime('%Y-%m')
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

                # Instantiate/update current average square metre price in KG
                avgsm_price_iri = self.sparql_client.get_avgsm_price_iri(postcode_iri)
                if not avgsm_price_iri:
                    avgsm_price_iri = KB + 'AveragePricePerSqm_' + str(uuid.uuid4())
                
                # Create instantiation/update triples
                triples = self.sparql_client.instantiate_average_price(avg_price_iri=avgsm_price_iri,
                                                    postcode_iri=postcode_iri,
                                                    avg_price=avg)
                update_query = f'INSERT DATA {{ {triples} }}'

                # Create rdflib graph with update triples
                g = Graph()
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
    #TODO: update link
    msg  = "This is an asynchronous agent to calculate the average square metre price of properties per postcode.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCPostProAgent#readme<BR>"
    return msg


if __name__ == '__main__':

    # Initialise KG client
    avgagent = AvgSqmPriceAgent(agent_iri='http://agent_iri',
                                time_interval= 100,
                                derivation_instance_base_url='http://derivation_base',
                                kg_url=QUERY_ENDPOINT )

    kgclient = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)

    # Get IRI inputs for testing
    # pcs = ['PE30 5DH', 'PE30 4XH', 'PE30 3NS', 'PE31 6XU', 
    #        'PE30 4GG', 'PE34 3LS']
    pcs = ['PE30 5DH']

    # Start INSERT query
    insert_query = 'INSERT DATA {'

    for pc in pcs:
    
        # Postcode IRI
        postcode = kgclient.get_postcode_iris([pc])[0]

        # Transaction record IRI list
        tx_iris = kgclient.get_tx_iris_for_postcodes([postcode])

        # Property Price Index IRI
        ppi = kgclient.get_ppi_iri(postcode)

        # Estimate average square metre price
        insert_query += avgagent.estimate_average_square_metre_price(postcode_iri=postcode, 
                                        tx_records=tx_iris, ppi_iri=ppi)
    
    insert_query += '}'
    insert_query = kgclient.remove_unnecessary_whitespace(insert_query)
    kgclient.performUpdate(insert_query)
