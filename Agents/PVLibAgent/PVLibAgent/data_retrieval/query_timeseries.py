from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from PVLibAgent.kg_utils.tsClient import TSClient
from PVLibAgent.kg_utils.utils import DB_URL, DB_USER, DB_PASSWORD

import agentlogging


# Initialise logger
logger = agentlogging.get_logger("prod")


def query_latest_timeseries(iri):
    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, rdb_password=DB_PASSWORD)
    with ts_client.connect() as conn:
        latest_value = (ts_client.tsclient.getLatestData(iri, conn))

        return latest_value
