from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from PVLibAgent.kg_utils.tsClientForUpdate import TSClientForUpdate
from PVLibAgent.error_handling.exceptions import TSException
from PVLibAgent.kg_utils.utils import DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD

import logging

class timeseries_instantiation:

    logging.basicConfig(level=logging.DEBUG)
    def add_timeseries_data(timeseries):
        kg_client = KGClient(query_endpoint=UPDATE_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        ts_client = TSClientForUpdate(kg_client=kg_client, rdb_url=DB_UPDATE_URL, rdb_user=DB_UPDATE_USER, rdb_password=DB_UPDATE_PASSWORD)
        with ts_client.connect() as conn:
            try:
                (ts_client.tsclient.addTimeSeriesData(timeseries, conn))
            except Exception as ex:
                logging.error("Adding of timeseries data to knowledge graph was not successful.")
                raise TSException("Adding of timeseries data to knowledge graph was not successful.") from ex

    def init_timeseries(dataIRIs: list, dataClass: list, timeUnit):
        kg_client = KGClient(query_endpoint=UPDATE_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        ts_client = TSClientForUpdate(kg_client=kg_client, rdb_url=DB_UPDATE_URL, rdb_user=DB_UPDATE_USER, rdb_password=DB_UPDATE_PASSWORD)
        with ts_client.connect() as conn:
            try:
                (ts_client.tsclient.initTimeSeries(dataIRIs, dataClass, timeUnit, conn))
            except Exception as ex:
                logging.error("Unable to initialise timeseries.")
                raise TSException("Unable to initialise timeseries.") from ex

    def check_data_has_timeseries(dataIRIs: list):
        kg_client = KGClient(query_endpoint=UPDATE_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        ts_client = TSClientForUpdate(kg_client=kg_client, rdb_url=DB_UPDATE_URL, rdb_user=DB_UPDATE_USER, rdb_password=DB_UPDATE_PASSWORD)
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
