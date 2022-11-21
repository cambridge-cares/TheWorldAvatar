from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from PVLibAgent.kg_utils.tsClientForQuery import TSClientForQuery
from PVLibAgent.kg_utils.utils import DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD

import agentlogging


# Initialise logger
logger = agentlogging.get_logger("prod")


def query_latest_timeseries(iri):
    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
    ts_client = TSClientForQuery(kg_client=kg_client, rdb_url=DB_QUERY_URL, rdb_user=DB_QUERY_USER, rdb_password=DB_QUERY_PASSWORD)
    with ts_client.connect() as conn:
        value = (ts_client.tsclient.getLatestData(iri, conn))

        return value
