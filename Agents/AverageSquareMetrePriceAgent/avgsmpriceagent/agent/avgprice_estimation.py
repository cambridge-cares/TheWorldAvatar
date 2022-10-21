################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to instantiate/update the average
# square metre price for a postcode based on instantiated Price Paid
# Data transactions in the KG (using asynchronous derivation framework)

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs
from pyderivationagent import FlaskConfig

import json
import uuid
import pandas as pd
import urllib.parse
import requests

import agentlogging
from avgsmpriceagent.datamodel.iris import *
from avgsmpriceagent.datamodel.data_mapping import TIME_FORMAT, DATACLASS
from avgsmpriceagent.errorhandling.exceptions import TSException, APIException
from avgsmpriceagent.kg_operations.kgclient import KGClient
from avgsmpriceagent.kg_operations.tsclient import TSClient
from avgsmpriceagent.utils.env_configs import ONS_ENDPOINT
from avgsmpriceagent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from avgsmpriceagent.kg_operations.querytemplates import *

# Initialise logger
#logger = agentlogging.get_logger("prod")

def estimate_average_square_metre_price(postcode_iri:str = None, 
                                        tx_records:list = None,
                                        ppi_iri:str = None,
                                        query_endpoint = QUERY_ENDPOINT,
                                        update_endpoint = UPDATE_ENDPOINT):
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
    
        # Initialise KG clients
        kgclient = KGClient(query_endpoint, update_endpoint)
        ts_client = TSClient(kg_client=kgclient)

        # In case less than `threshold` transactions provided/available for current postcode,
        # include transactions from nearby postcodes
        threshold = 5
        if len(tx_records) < threshold:
            tx_records = get_transactions_from_nearest_postcodes(postcode_iri, threshold)

        if tx_records:
            # 1) Retrieve representative UK House Price Index and parse as Series (i.e. unwrap Java data types)
            # UKHPI was set at a base of 100 in January 2015, and reflects the change in value of residential property since then
            # (https://landregistry.data.gov.uk/app/ukhpi/doc)
            try:
                ts = ts_client.tsclient.getTimeSeries([ppi_iri], ts_client.conn)
                dates = [d.toString() for d in ts.getTimes()]
                values = [v for v in ts.getValues(ppi_iri)]
            except Exception as ex:
                #logger.error('Error retrieving/unwrapping Property Price Index time series')
                raise TSException('Error retrieving/unwrapping Property Price Index time series') from ex

            # Create UKHPI series with conditioned date index
            ukhpi = pd.Series(index=dates, data=values)
            ukhpi.index = pd.to_datetime(ukhpi.index, format='%Y-%m-%d')
            ukhpi.sort_index(ascending=False, inplace=True)
            ukhpi.index = ukhpi.index.strftime('%Y-%m')

            # 2) Retrieve sales transaction details for transaction IRIs
            query = get_tx_details_and_floor_areas(tx_records)
            res = kgclient.performQuery(query)
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
            query = get_avgsm_price_iri(postcode_iri)
            res = kgclient.performQuery(query)
            if res:
                avgsm_price_iri = res[0]['avg_price_iri']
            else:
                avgsm_price_iri = KB + 'AveragePricePerSqm_' + str(uuid.uuid4())
            
            # Create instantiation/update triples
            triples = instantiate_average_price(avg_price_iri=avgsm_price_iri,
                                                postcode_iri=postcode_iri,
                                                avg_price=avg)

    return triples


def get_transactions_from_nearest_postcodes(postcode_iri:str, threshold:int):
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
    query = get_postcode_strings([postcode_iri])
    res = kgclient.performQuery(query)
    postcode_str = res[0]['pc']

    # Retrieve nearby postcodes with easting and northing from ONS endpoint
    query = get_nearby_postcodes(postcode_str)
    query = urllib.parse.quote(query)
    # Perform GET request
    url = ONS_ENDPOINT + '.json?query=' + query
    res = requests.get(url)
    if res.status_code != 200:
        #logger.error('Error retrieving data from ONS API.')
        raise APIException('Error retrieving data from ONS API.')

    # Extract and unwrap results
    data = json.loads(res.text)
    df = pd.DataFrame(data['results']['bindings'])
    for c in df.columns:
        df[c] = df[c].apply(lambda x: x['value'])
    # Set postcode as index and ensure float coordinate values
    df.set_index('pc', inplace=True)
    df = df.astype(float)

    # Set target postcode as reference point for distance calculation
    pref = df.loc[postcode_str]
    # Assess distance from target postcode
    df['dist'] = df.apply(lambda x: distance(x, pref), axis=1)

    # Retrieve number of available transactions per postcode
    query = get_tx_count_for_postcodes(df.index.to_list())
    res = kgclient.performQuery(query)
    tx_map = {r['pc']: r['txs'] for r in res}
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
    query = get_postcode_iris(df.index.tolist())
    res = kgclient.performQuery(query)
    pc_iris = [r['pc_iri'] for r in res]
    query = get_tx_iris_for_postcodes(pc_iris)
    txns = kgclient.performQuery(query)
    txns = [tx['tx_iri'] for tx in txns]

    return txns


def distance(pointA, pointB):
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
    kgclient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)

    # Get IRI inputs for testing
    pcs = ['PE30 5DH', 'PE30 4XH', 'PE30 3NS', 'PE31 6XU', 
           'PE30 4GG', 'PE34 3LS']

    # Start INSERT query
    insert_query = 'INSERT DATA {'

    for pc in pcs:
    
        # Postcode IRI
        pc_query = get_postcode_iris([pc])
        postcode = kgclient.performQuery(pc_query)
        postcode = postcode[0]['pc_iri']

        # Transaction record IRI list
        tx_query = get_tx_iris_for_postcodes([postcode])
        txns = kgclient.performQuery(tx_query)
        txns = [tx['tx_iri'] for tx in txns]

        # Property Price Index IRI
        ppi_query = get_ppi_iri(postcode)
        ppi = kgclient.performQuery(ppi_query)
        ppi = ppi[0]['ppi_iri']

        # Estimate average square metre price
        insert_query += estimate_average_square_metre_price(postcode_iri=postcode, 
                                                            tx_records=txns, 
                                                            ppi_iri=ppi)
    
    insert_query += '}'
    insert_query = remove_unnecessary_whitespace(insert_query)
    kgclient.performUpdate(insert_query)
