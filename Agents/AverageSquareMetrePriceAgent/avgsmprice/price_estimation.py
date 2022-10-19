################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 19 Oct 2022                            #
################################################

# The purpose of this module is to instantiate/update data retrieved from
# the HM Land Registry Open Data SPARQL endpoint according to OntoBuiltEnv

import re
import json
import uuid
import numpy as np
import pandas as pd


#import agentlogging

from avgsmprice.datamodel.iris import *
from avgsmprice.datamodel.data_mapping import *
from avgsmprice.datamodel.data_mapping import TIME_FORMAT, DATACLASS
from avgsmprice.errorhandling.exceptions import KGException
from avgsmprice.kgutils.kgclient import KGClient
from avgsmprice.kgutils.tsclient import TSClient
from avgsmprice.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from avgsmprice.kgutils.querytemplates import *

# Initialise logger
#logger = agentlogging.get_logger("prod")


def estimate_average_square_metre_price(postcode_iri:str = None, 
                                        tx_records:list = None,
                                        ppi_iri:str = None,
                                        query_endpoint = QUERY_ENDPOINT,
                                        update_endpoint = UPDATE_ENDPOINT):
    """
    Retrieves HM Land Registry's Price Paid data for provided properties from
    given endpoint and instantiates them in the KG according to OntoBuiltEnv

    Arguments:
        postcode_iri - IRI of postcode for which to estimate average square metre price
        tx_records - list of sales transaction IRIs (within postcode) which to consider
                     when estimating average square metre price
        ppi_iri - Property Price Index IRI of local authority associated with postcode
        query_endpoint/update_endpoint - SPARQL endpoint with instantiated buildings
    Returns:
        Current average square metre price for postcode    
    """

    if postcode_iri and ppi_iri:
    
        # Initialise KG clients
        kgclient = KGClient(query_endpoint, update_endpoint)
        ts_client = TSClient(kg_client=kgclient)

        # In case no transactions provided/available for current postcode, retrieve
        # transactions from nearby postcode
        if not tx_records:
            #TODO: query to ONS SPARQL endpoint to be implemented
            pass

        else:
            # 1) Retrieve representative UK House Price Index and parse as Series
            #    (i.e. unwrap Java data types)
            # UKHPI was set at a base of 100 in January 2015, and reflects the change in value of residential property since then
            # (https://landregistry.data.gov.uk/app/ukhpi/doc)
            ts = ts_client.tsclient.getTimeSeries([ppi_iri], ts_client.conn)
            dates = [d.toString() for d in ts.getTimes()]
            values = [v for v in ts.getValues(ppi_iri)]
            ukhpi = pd.Series(index=dates, data=values)
            ukhpi.index = pd.to_datetime(ukhpi.index, format='%Y-%m-%d')
            ukhpi.sort_index(ascending=False, inplace=True)
            ukhpi.index = ukhpi.index.strftime('%Y-%m')

            # Retrieve transaction details
            query = get_tx_details_and_floor_areas(tx_records)
            res = kgclient.performQuery(query)
            cols = ['tx_iri', 'price', 'date', 'floor_area']
            df = pd.DataFrame(columns=cols, data=res)
            df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%d')
            df['date'] = df['date'].dt.strftime('%Y-%m')
            df['price'] = df['price'].astype(float)
            df['floor_area'] = df['floor_area'].astype(float)

            # Assess original square metre price
            df['avg_orig'] = df['price'] / df['floor_area']
            # Assess current square metre price
            df['ukhpi_orig'] = df['date'].map(ukhpi)
            df['ukhpi_curr'] = ukhpi[0]
            df['avg_curr'] = df['avg_orig'] / df['ukhpi_orig'] * df['ukhpi_curr']

            avg = round(df['avg_curr'].mean())
   
    return avg


if __name__ == '__main__':

    # Initialise KG client
    kgclient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)

    # Get IRI inputs for testing , i.e. postcode 'PE30 5DH'
    pc = 'PE30 5DH'
    
    # Postcode IRI
    pc_query = get_postcode_iri(pc)
    postcode = kgclient.performQuery(pc_query)
    postcode = postcode[0]['pc_iri']

    # Transaction record IRI list
    tx_query = get_tx_iris_for_postcode(postcode)
    txns = kgclient.performQuery(tx_query)
    txns = [tx['tx_iri'] for tx in txns]

    # Property Price Index IRI
    ppi_query = get_ppi_iri(postcode)
    ppi = kgclient.performQuery(ppi_query)
    ppi = ppi[0]['ppi_iri']

    # Estimate average square metre price
    estimate_average_square_metre_price(postcode_iri=postcode, tx_records=txns, 
                                        ppi_iri=ppi)
