from PVLibAgent.error_handling.exceptions import TSException
from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.tsClientForQuery import TSClientForQuery

import logging

def query_latest_timeseries(iri, query_endpoint: str, update_endpoint: str, db_query_url: str, db_query_user: str, db_query_password: str):
    logging.basicConfig(level=logging.DEBUG)
    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint=query_endpoint, update_endpoint=update_endpoint)
    ts_client = TSClientForQuery(kg_client=kg_client, rdb_url=db_query_url, rdb_user=db_query_user, rdb_password=db_query_password)
    try:
        with ts_client.connect() as conn:
            value = (ts_client.tsclient.getLatestData(iri, conn))
    except Exception as ex:
        logging.error("Unable to get latest data from knowledge graph!")
        raise TSException("Unable to get latest data from knowledge graph!") from ex
    return value

def query_all_timeseries(iri, query_endpoint: str, update_endpoint: str, db_query_url: str, db_query_user: str, db_query_password: str):
    logging.basicConfig(level=logging.DEBUG)
    kg_client = KGClient(query_endpoint=query_endpoint, update_endpoint=update_endpoint)
    ts_client = TSClientForQuery(kg_client=kg_client, rdb_url=db_query_url, rdb_user=db_query_user, rdb_password=db_query_password)

    with ts_client.connect() as conn:
        value = ts_client.tsclient.getTimeSeries([iri], conn)
    return value