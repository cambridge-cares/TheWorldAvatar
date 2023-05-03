from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from PVLibAgent.kg_utils.tsClientForUpdate import TSClientForUpdate
from PVLibAgent.error_handling.exceptions import TSException
from PVLibAgent.kg_utils.utils import DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD
from PVLibAgent.data_retrieval.query_data import query_NTU_Buildings
import logging
import uuid

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

                (ts_client.tsclient.initTimeSeries(dataIRIs, dataClass, timeUnit, conn, TSClientForUpdate.jpsBaseLibView.TimeSeriesClient.Type.INSTANTANEOUS, None, None))
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

    def link_to_NTU_KG(dataIRIs: list):
        kg_client = KGClient(query_endpoint=UPDATE_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)

        NTU_buildings = query_NTU_Buildings(query_endpoint=UPDATE_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)

        for building in NTU_buildings:
            # Create IRI for current sensor
            pgIRI = create_sparql_prefix('powreal') + 'PhotovoltaicGenerator_' + building['name'] + str(uuid.uuid4())
            pvIRI = create_sparql_prefix('powreal') + 'PhotovoltaicPanel_' + building['name'] + str(uuid.uuid4())
            gpIRI = create_sparql_prefix('powsys') + 'GeneratedPower_' + building['name'] + str(uuid.uuid4())
            measureIRI = create_sparql_prefix('om') + 'Measure_' + building['name'] + str(uuid.uuid4())
            query = create_sparql_prefix('powreal') + \
                    create_sparql_prefix('Powsys') + \
                    create_sparql_prefix('ontocape') + \
                    create_sparql_prefix('om') + \
                    create_sparql_prefix('ts') + \
                    '''INSERT DATA { \
                    <%s> ontocape:contains <%s> . \
                    <%s> rdf:type powreal:PhotovoltaicGenerator . \
                    <%s> ontocape:hasSubsystem <%s> . \
                    <%s> rdf:type powreal:PhotovoltaicPanel . \
                    <%s> Powsys:hasPowerGenerated <%s> . \
                    <%s> rdf:type powsys:GeneratedPower . \
                    <%s> om:hasValue <%s> . \
                    <%s> rdf:type om:Measure . \
                    <%s> ts:hasTimeSeries <%s> . \
                    ''' % (building['building'], pgIRI, pgIRI, pgIRI, pvIRI, pvIRI, pgIRI, gpIRI, gpIRI, gpIRI, measureIRI, measureIRI, measureIRI, dataIRIs[0])
            KGClient.executeUpdate(query)

