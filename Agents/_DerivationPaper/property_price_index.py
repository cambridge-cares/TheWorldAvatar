# This script provides functionality to update initially instantiated Property
# Price Index data both in the KG and RDB, i.e. adding new entry to RDB and
# updating time stamp of pure input in KG to trigger 2nd cascade of updates

import os
import pandas as pd
from pathlib import Path

from iris import *
from tsclient import TSClient, TIME_FORMAT_KG, TIME_FORMAT_RDB
from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT

from pyderivationagent import PyDerivationClient
from pyderivationagent.kg_operations import PySparqlClient

# REQUIRED FILES
# Specify name of csv file with all UK house price index data
ukhpi_file = 'ukhpi.csv'
ukhpi_fp = os.path.join(Path(__file__).parent, 'data', ukhpi_file)

# HM Land Registry SPARQL endpoint
UKHPI_ENDPOINT = 'http://landregistry.data.gov.uk/landregistry/query'


def retrieve_ukhpi_monthly_data() -> str:
    """
    Retrieve UKHPI data for King's Lynn and West Norfolk from HM Land Registry
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
    Download UKHPI data for King's Lynn and West Norfolk and save to .csv
    """
    
    # Query UKHPI endpoint
    query = retrieve_ukhpi_monthly_data()
    res = kg_client.performQuery(query)

    # Condition data and save to csv
    df = pd.DataFrame(columns=['month', 'ukhpi_value'], data=res)
    df.to_csv(output_file, index=False)


def load_ukhpi_timeseries_from_csv(input_file, ppi_iri=ppi_iri, skip_first_entries=0):
    """
    Create Java time series object from .csv file

    Arguments:
        ppi_iri - property price index IRI
        skip_first_entries - Number of entries to skip at the beginning of the file    
    """

    # Load data from csv
    df = pd.read_csv(input_file)

    # Assign correct data types
    df['month'] = pd.to_datetime(df['month'])
    df['month'] = df['month'].dt.strftime(TIME_FORMAT_RDB)
    df['ukhpi_value'] = df['ukhpi_value'].astype(float)

    # Skip first entries as specified
    # NOTE: This is used to mock the updating of instantiated data by input agents
    #       to trigger the derivation updating cascade
    df = df.iloc[skip_first_entries:]

    # Create time series
    time_list = df['month'].tolist()
    value_list = df['ukhpi_value'].tolist()
    ts = TSClient.create_timeseries(time_list, [ppi_iri], [value_list])

    return ts


def initialise_ukhpi(kg_client, district_iri=district_iri, ppi_iri=ppi_iri,
                     timeseries_file=ukhpi_fp):
    """
    Instantiate Property Price Index in KG and RDB and upload "outdated" PPI data
    (i.e. time series data excluding the latest entry)
    """

    # Instantiate static triples in KG
    query = instantiate_property_price_index(district_iri, ppi_iri)
    kg_client.performUpdate(query)

    # Initialise TimeSeriesClient with default settings and load time series from csv
    ts_client = TSClient(kg_client=kg_client)
    ts = load_ukhpi_timeseries_from_csv(timeseries_file, ppi_iri, skip_first_entries=1)
    
    with ts_client.connect() as conn:
        # Initialise TimeSeries
        ts_client.tsclient.initTimeSeries([ppi_iri], [ts_client.DATACLASS], TIME_FORMAT_KG, conn)
        # Add time series data
        ts_client.tsclient.addTimeSeriesData(ts, conn)


def update_ukhpi(kg_client, derivation_client, ppi_iri=ppi_iri, timeseries_file=ukhpi_fp):
    """
    Update Property Price Index with latest data and update pure input timestamp
    """
    
    # Initialise TimeSeriesClient with default settings and load ENTIRE time series from csv
    ts_client = TSClient(kg_client=kg_client)
    ts = load_ukhpi_timeseries_from_csv(timeseries_file, ppi_iri, skip_first_entries=0)
    
    with ts_client.connect() as conn:
        # Update time series data (overwrite existing timestamps and add new one)
        ts_client.tsclient.addTimeSeriesData(ts, conn)
    
    # Update time stamp attached to PropertyPriceIndex IRI 
    derivation_client.updateTimestamp(ppi_iri)



def instantiate_property_price_index(district_iri, ppi_iri):
    # Instantiate property price index for a given administrative district
    query = f"""
        INSERT DATA {{
            <{ppi_iri}> <{RDF_TYPE}> <{OBE_PROPERTY_PRICE_INDEX}> . 
            <{ppi_iri}> <{OBE_REPRESENTATIVE_FOR}> <{district_iri}> . 
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


if __name__ == '__main__':

    # Create UKHPI csv file if not exists
    if not Path(ukhpi_fp).exists():
        # Initialise KG client
        kg_client = PySparqlClient(query_endpoint=UKHPI_ENDPOINT,
                                   update_endpoint=UKHPI_ENDPOINT)
        create_ukhpi_csv(kg_client, ukhpi_fp)

    # Initialise KG client
    kg_client = PySparqlClient(query_endpoint=SPARQL_QUERY_ENDPOINT,
                               update_endpoint=SPARQL_UPDATE_ENDPOINT)
    # Create "dummy" derivation client to access time stamp manipulation methods
    deriv_client = PyDerivationClient(derivation_instance_base_url=None,
                                      query_endpoint=SPARQL_QUERY_ENDPOINT,
                                      update_endpoint=SPARQL_QUERY_ENDPOINT)

    # Initialise PropertyPriceIndex in KG and RDB and upload "outdated" data
    #initialise_ukhpi(kg_client)
    # NOTE: Time stamp for pure input is added when marking up derivation; only here
    #       for reference/testing purposes
    #deriv_client.addTimeInstanceCurrentTimestamp(ppi_iri)

    # Update time stamp attached to PropertyPriceIndex IRI
    update_ukhpi(kg_client=kg_client, derivation_client=deriv_client)
