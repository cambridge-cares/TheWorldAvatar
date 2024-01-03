from NTUEnergyClusterAgent.kg_utils.kgClient import KGClient
from NTUEnergyClusterAgent.kg_utils.tsClientForUpdate import TSClientForUpdate
from NTUEnergyClusterAgent.error_handling.exceptions import TSException
from NTUEnergyClusterAgent.data_retrieval.query_data import QueryData
from NTUEnergyClusterAgent.kg_utils.utils import create_sparql_prefix

import logging
import uuid

class timeseries_instantiation:

    logging.basicConfig(level=logging.DEBUG)
    def add_timeseries_data(timeseries, query_endpoint: str, update_endpoint: str, db_query_url: str, db_query_user: str, db_query_password: str):
        kg_client = KGClient(query_endpoint, update_endpoint)
        ts_client = TSClientForUpdate(kg_client=kg_client, rdb_url=db_query_url, rdb_user=db_query_user, rdb_password=db_query_password)
        with ts_client.connect() as conn:
            try:
                (ts_client.tsclient.addTimeSeriesData(timeseries, conn))
            except Exception as ex:
                logging.error("Adding of timeseries data to knowledge graph was not successful.")
                raise TSException("Adding of timeseries data to knowledge graph was not successful.") from ex

    def init_timeseries(dataIRIs: list, dataClass: list, timeUnit, query_endpoint: str, update_endpoint: str, db_query_url: str, db_query_user: str, db_query_password: str):
        kg_client = KGClient(query_endpoint, update_endpoint)
        ts_client = TSClientForUpdate(kg_client=kg_client, rdb_url=db_query_url, rdb_user=db_query_user, rdb_password=db_query_password)
        with ts_client.connect() as conn:
            try:
                (ts_client.tsclient.initTimeSeries(dataIRIs, dataClass, timeUnit, conn, TSClientForUpdate.jpsBaseLibView.TimeSeriesClient.Type.INSTANTANEOUS, None, None))
            except Exception as ex:
                logging.error("Unable to initialise timeseries.")
                raise TSException("Unable to initialise timeseries.") from ex

    def check_data_has_timeseries(dataIRIs: list, query_endpoint: str, update_endpoint: str, db_query_url: str, db_query_user: str, db_query_password: str):
        kg_client = KGClient(query_endpoint=query_endpoint, update_endpoint=update_endpoint)
        ts_client = TSClientForUpdate(kg_client=kg_client, rdb_url=db_query_url, rdb_user=db_query_user, rdb_password=db_query_password)
        with ts_client.connect() as conn:
            try:
                for iri in dataIRIs:
                    response = (ts_client.tsclient.checkDataHasTimeSeries(iri, conn))
            except Exception as ex:
                if str(ex).__contains__("ERROR: relation \"dbTable\" does not exist"):
                    response = False
                else:
                    logging.error("Unable to check whether " + iri + " has timeseries")
                    raise Exception("Unable to check whether " + iri + " has timeseries")

            return response
