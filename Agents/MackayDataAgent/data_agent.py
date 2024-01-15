import configparser
import os.path
from pathlib import Path
import string
import importlib
from kg_access import tsclient_wrapper,forecast_client
from typing import List
from downloader.downloaders import create_data_downloader
from utils.conf_utils import *
from data_types.ts_data_classes import PropertiesFileProtytype, ForecastMeta,KgAccessInfo


class MackayDataAgent:
    def __init__(self, confdir='./confs'):
        self.base_conf = load_conf(os.path.join(confdir, 'base.cfg'))
        data_confs = load_confs_from_dir(os.path.join(confdir, 'data'))
        # Download and Parse each TS from data source
        self.data_downloaders = [create_data_downloader(dataconf) for dataconf in data_confs]
        self.property_files_dict = self._create_property_files()

    # Generate Java property files to call TSClient for each TS instance (each needs a new one as sparql ep is different)
    def _create_property_files(self) -> List[str]:
        outdir = self.base_conf['paths']['resourcesDir']
        sqlcfg = self.base_conf['postgresql']
        outpaths = {}
        for dataobj in self.data_downloaders:
            datacfg = dataobj.conf
            dataname = datacfg['source']['name']
            props = PropertiesFileProtytype.copy()
            allcfg = dict(datacfg['kg']).copy()
            allcfg.update(dict(sqlcfg))
            updated_props = match_properties(props, allcfg)
            outpath = os.path.abspath(os.path.join(Path(__file__).parent, outdir, "{}.properties".format(dataname)))
            write_java_properties_conf(updated_props, outpath)
            outpaths[dataname] = outpath
        return outpaths

    # Create postgresql tables and meta info triples of all timeseries data
    def register_timeseries(self):
        # connection set up
        sqlcfg = self.base_conf['postgresql']
        dburls = sqlcfg['url'].split('/')
        db_name = dburls[-1]
        tsclient_wrapper.create_postgres_db_if_not_exists(db_name, sqlcfg['user'], sqlcfg['password'])
        # loop through each configuration object, one for each data source
        for reader in self.data_downloaders:
            timeseries_meta = reader.get_tsmeta()
            TSClient = tsclient_wrapper.TSClient(self.property_files_dict[reader.dataname])
            TSClient.register_timeseries( timeseries_meta)

    def update_from_external_and_predict(self):
        data_info = []
        #TODO: add proper logging for failure
        for data_factory in self.data_downloaders:
            timeseries_instance = data_factory.download_tsinstance()
            kg_cfg = data_factory.conf['kg']
            kg_info = KgAccessInfo(**kg_cfg)
            TSClient = tsclient_wrapper.TSClient(self.property_files_dict[data_factory.dataname])
            updated = TSClient.update_timeseries_if_new( timeseries_instance)
            if updated: # API have new data, needs to call prediction again
                #pad empty TS instance for prediction
                pad_start = int(timeseries_instance.times[-1])+1 # custom calculation of predict year start, allow other time type in future
                padded_for_forcast = data_factory.get_empty_tsinstance_for_predict(pad_start)
                TSClient.add_timeseries( padded_for_forcast)
                start = timeseries_instance.times[0]
                end = timeseries_instance.times[-1]
                history_duration = float(end) - float(start)
                forecast_cfg = data_factory.conf['forecast']
                predict_input = ForecastMeta(name = data_factory.dataname,iri=data_factory.src_iri, duration=history_duration, start_str=start, end_str = end, frequency= float(forecast_cfg['frequency']),unit_frequency=forecast_cfg['unitFrequency'])
                self.call_predict(kg_info,predict_input)


    def call_predict(self, kg_info:KgAccessInfo, forecast_meta:ForecastMeta):
        # construct a client  to forecast agent
        predict_agent =forecast_client.ForcastAgentClient(self.base_conf['forecast_agent']['agent_url'],kg_info)
        predict_input = predict_agent.update_forcast_meta(forecast_meta)
        predict_agent.run(predict_input)
