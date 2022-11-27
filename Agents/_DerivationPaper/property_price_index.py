# This script provides functionality to prepare and instantiate all data required
# for the minimum viable product of the King's Lynn use case for the derivation paper

import os
import requests
import pandas as pd
from pathlib import Path

from iris import *
from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT

from pyderivationagent.kg_operations import PySparqlClient

# REQUIRED FILES
# Specify name of csv file with all UK house price index data
ukhpi_file = 'ukhpi.csv'

# HM Land Registry SPARQL endpoint
UKHPI_ENDPOINT = 'http://landregistry.data.gov.uk/landregistry/query'


def retrieve_ukhpi_monthly_data() -> str:
    """
    Retrieve UKHPI data for King's Lynn and West Norfolk
    (returned date format is xsd:gYearMonth (YYYY-MM))
    """

    query = f"""
        SELECT ?month ?ukhpi_value 
        WHERE {{
        ?ukhpi <{UKHPI_REFREGION}> <http://landregistry.data.gov.uk/id/region/king's-lynn-and-west-norfolk> ; 
               <{UKHPI_INDEX}> ?ukhpi_value ; 
               <{UKHPI_REF_MONTH}> ?month 
        }}
        ORDER BY DESC(?month)
    """

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


def create_ukhpi_csv(kg_client, output_file):
    """
    Download UKHPI data for King's Lynn and West Norfolk and save to CSV
    """
    
    # Query UKHPI endpoint
    query = retrieve_ukhpi_monthly_data()
    res = kg_client.performQuery(query)

    # Condition data and save to csv
    df = pd.DataFrame(columns=['month', 'ukhpi_value'], data=res)
    df.to_csv(output_file, index=False)


def create_ukhpi_timeseries(local_authority_iri, ppi_iri, months=240,
                            api_endpoint=UKHPI_ENDPOINT, kgclient_hm=None):
    """
    Retrieve UKHPI data for a given local authority ONS IRI 

    Arguments:
        local_authority_iri {str} - IRI of the local authority as used by
                                    Office for National statistics
        ppi_iri {str} - IRI of the property price index (i.e. dataIRI of timeseries)
        months {int} - Number of months for which to retrieve date
                       (default: 240 (i.e. 20 years))
    
    """

    # Initialise KG client
    if not kgclient_hm:
        kgclient_hm = KGClient(api_endpoint, api_endpoint)

    query = get_ukhpi_monthly_data_for_district(ons_local_authority_iri=local_authority_iri, 
                                                months=months)
    res = kgclient_hm.performQuery(query)

    # Condition data
    cols = ['month', 'ukhpi_value']
    df = pd.DataFrame(columns=cols, data=res)
    df.dropna(inplace=True)

    # Assign correct data types
    df['month'] = pd.to_datetime(df['month'])
    df['month'] = df['month'].dt.strftime('%Y-%m-%d')
    df['ukhpi_value'] = df['ukhpi_value'].astype(float)

    # Create time series
    time_list = df['month'].tolist()
    value_list = df['ukhpi_value'].tolist()
    ts = TSClient.create_timeseries(time_list, [ppi_iri], [value_list])

    return ts


if __name__ == '__main__':

    # Create UKHPI csv file if not exists
    ukhpi_fp = os.path.join(Path(__file__).parent, 'data', ukhpi_file)
    if not Path(ukhpi_fp).exists():
        # Initialise KG client
        kg_client = PySparqlClient(query_endpoint=UKHPI_ENDPOINT,
                                   update_endpoint=UKHPI_ENDPOINT)
        create_ukhpi_csv(kg_client, ukhpi_fp)

    pass
