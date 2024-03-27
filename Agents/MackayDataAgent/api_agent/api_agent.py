import logging

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from data_classes import ts_data_classes
from data_classes.ts_data_classes import KgAccessInfo
from downloader.downloaders import Downloader
from kg_access.apisource_kg_client import APISourceSparqlClient
from kg_access.tsclient_wrapper import TSClient,create_postgres_db_if_not_exists
from utils.conf_utils import create_property_file
from data_classes.iris import *


class APIAgent(DerivationAgent):
    def __init__(self, db_url,db_user,db_pw,**kwargs):
        #self.X = kwargs.pop('X')
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)
        self.agent_iri = kwargs.pop('agent_iri')
        kg_url = kwargs.pop('kg_url')
        self.api_sparql_client = APISourceSparqlClient(KgAccessInfo(kg_url, kwargs.pop('kg_password'), kwargs.pop('kg_user')))
        property_file = create_property_file('APIAgent',db_url , db_user, db_pw,kg_url)
        self.ts_client = TSClient(property_file)
        self.registered_APIs = []
        create_postgres_db_if_not_exists(db_url, db_user,  db_pw)

    ###############################
    ## II. Derivation agent part ##
    ###############################
    # The registration is by default, which can be altered by setting flag REGISTER_AGENT=false in the env file
    def agent_input_concepts(self) -> list:
        return [TSMAP]

    def agent_output_concepts(self) -> list:
        return [TS_TIMESERIES]

    def validate_inputs(self, http_request) -> bool:
        # You may want to add some specific validation after the generic checks
        if super().validate_inputs(http_request):
            # do some specific checking
            return True


    def get_self_update_func(self, ts_iri):
        try:
            deri_iri = self.derivation_client.getDerivationsOf([ts_iri]).get(ts_iri)
            if not deri_iri:
                return None
            def regular_self_update():
                logging.info("Update derivation {} for".format(ts_iri))
                self.derivation_client.unifiedUpdateDerivation(deri_iri)
            return regular_self_update
        except:
            return None



    def stamp_meta_current_time(self , meta_iri,replace_exist=False):
        if not replace_exist:# do nothing if exists timestamp
            self.derivation_client.addTimeInstanceCurrentTimestamp([meta_iri])
        else:
            self.derivation_client.updateTimestamp(meta_iri)

    def _register_regular_update(self, interval_str, meta_iri, ts_iri):
        self.stamp_meta_current_time(meta_iri, replace_exist=True) # Stamp Meta with now as last check time
        update_func = self.get_self_update_func(ts_iri)
        if update_func:
            self.scheduler.add_job(
                id=meta_iri.split('/')[-1], # the name for the periodical job
                func=update_func, # the function for the periodical job
                trigger='interval', # trigger type
                seconds=float(interval_str)*3600*24 # the time interval unitday you prefer for the job execution
            )
        else:
            logging.warn('API meta in KG {} has no derivation.'.format(meta_iri))


    # On restart, retreive all registered APIs (derivation) from KG and register them
    def init_register_all_api_in_kg(self):
        derivation_iris = self.derivation_client.getDerivations(self.agent_iri)
        api_iris = [ str(self.derivation_client.derivation_client.retrieveAgentInputIRIs(deriv, self.agent_iri).get("agent_input").get(TSMAP).get(0)) for deriv in derivation_iris] # For each API meta instance registered with derivaiton
        try:
            for api_meta_iri in api_iris:
                target_iri = self.api_sparql_client.get_target_iri_from_map_iri(api_meta_iri)
                print(target_iri)
                ts_iri = self.ts_client.get_ts_iri(target_iri) # retreive registered timeseries
                print(ts_iri)
                interval = self.api_sparql_client.get_update_interval(api_meta_iri)
                api_id = api_meta_iri.split('/')[-1]
                self.registered_APIs.append(api_id)
                self._register_regular_update(interval, api_meta_iri,ts_iri)
        except Exception as e:
            logging.error('Failed Initiation API Agent when retrieving existing API info in KG %s ', repr(e))
            raise Exception('Failed Initiation API Agent when retrieving existing API info in KG %s ', repr(e))


    # Case1: Sync: user send http request to register an newly inserted API meta
    # Case2: After each month, data need update, automatically update map timestamp via pre-set periodical job
    # Case3: After one year, api change, manually update api instance info, then call timestamp update
    # Case4: agent stop and restart, preserve of information from init
    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        #Retreive input IRIs
        api_meta_iri = derivation_inputs.getIris(TSMAP)[0]
        target_iri = self.api_sparql_client.get_target_iri_from_map_iri(api_meta_iri)
        api_id = api_meta_iri.split('/')[-1]
        interval = self.api_sparql_client.get_update_interval(api_meta_iri) # retreive interval

        #register TS in KG&RDB if none exists
        ts_exist = self.ts_client.check_timeseries_exist(target_iri)
        if not ts_exist: # register if not exist
            self.ts_client.register_timeseries( ts_data_classes.TimeSeriesMeta(time_unit=ts_data_classes.TSTR_FORMATS['Instant'], src_iri=target_iri))
        ts_iri = self.ts_client.get_ts_iri(target_iri)
        print('register ts:{}'.format(ts_iri))

        # First time, not registered before -> register a periodic job for self-udpating
        if api_id not in self.registered_APIs:
            self.registered_APIs.append(api_id)
            self._register_regular_update(interval, api_meta_iri,ts_iri)

        #Tag static input with timestamp
        # no existing timestamp => first call, add timestamp to map iri, if exist timestamp, this function has no effect
        self.stamp_meta_current_time(api_meta_iri,replace_exist=False)

        # construct parameters for downloader from KG
        api_url_info = self.api_sparql_client.get_api_info(api_meta_iri)

        # Download data TS and link to target
        data_downloader = Downloader(target_iri=target_iri,**api_url_info)

        # download TS instance
        TS_INSTANCE = data_downloader.download_tsinstance()

        #update TS record only (TS IRI not changed)
        self.ts_client.update_timeseries_if_new(TS_INSTANCE)
        derivation_outputs.createNewEntity(ts_iri, TS_TIMESERIES)
        out = self.ts_client.get_timeseries(target_iri)
        print('in rdb:')
        print(out)