from PVLibAgent.error_handling.exceptions import TSException
from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from PVLibAgent.kg_utils.tsClientForQuery import TSClientForQuery
from PVLibAgent.kg_utils.utils import DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD

import logging

def query_latest_timeseries(iri):
    logging.basicConfig(level=logging.DEBUG)
    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
    ts_client = TSClientForQuery(kg_client=kg_client, rdb_url=DB_QUERY_URL, rdb_user=DB_QUERY_USER, rdb_password=DB_QUERY_PASSWORD)
    try:
        with ts_client.connect() as conn:
            value = (ts_client.tsclient.getLatestData(iri, conn))
    except Exception as ex:
        logging.error("Unable to get latest data from knowledge graph!")
        raise TSException("Unable to get latest data from knowledge graph!") from ex

    return value
