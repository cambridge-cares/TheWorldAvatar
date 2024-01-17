import configparser
import os.path
from pathlib import Path
import string
import importlib
from kg_access import tsclient_wrapper, forecast_client
from typing import List
from downloader.downloaders import create_data_downloader
from utils.conf_utils import *
from data_classes.ts_data_classes import PropertiesFileProtytype, ForecastMeta, KgAccessInfo

'''
Download and run Forecast for a list of timeseries as specified in agent config files.

'''


class MackayDataAgent:
    def __init__(self, confdir='./confs'):
        self.base_conf = load_conf(os.path.join(confdir, 'base.cfg'))
        data_confs = load_confs_from_dir(os.path.join(confdir, 'data'))
        # Download and Parse each TS from data source
        self.property_files_dict = self._create_property_files(data_confs)
        self.tsclients = {}
        self.forecastclients = {}
        self.name_to_iris = {d["source"]["name"]: d["output"]["srcIri"] for d in data_confs}
        self.data_downloaders = {d["source"]["name"]: create_data_downloader(d) for d in data_confs}
        for cfg in data_confs:
            data_name = cfg["source"]["name"]
            self.tsclients[data_name] = tsclient_wrapper.TSClient(self.property_files_dict[data_name])
            self.forecastclients[data_name] = forecast_client.ForcastAgentClient(
                self.base_conf['forecast_agent']['url'], self.base_conf['forecast_agent']['iri'],
                KgAccessInfo(**cfg['kg_access']))

    # Initiate the agent: 1. init RDB data 2. register all data timeseries in RDB and KB
    def initiate(self):
        self.init_RDB()
        self.register_all_timeseries_if_not_exist()

    def get_data(self, forecast=True) -> dict:
        data = {}
        for data_name in self.name_to_iris:
            TSClient = self.tsclients[data_name]
            data_iri = self.name_to_iris[data_name]
            times, values = TSClient.get_timeseries(data_iri)
            if forecast:# concat original series with forecasted one
                forecast_client = self.forecastclients[data_name]
                forecast_iri = forecast_client.get_forecast_iri(data_iri)
                ftimes, fvalues = TSClient.get_timeseries(forecast_iri)
                truncate_holder = [(t,v) for t,v in zip(times,values) if v!=0]
                times = [d[0] for d in truncate_holder]
                values =  [d[1] for d in truncate_holder]
                times.extend(ftimes)
                values.extend(fvalues)
            data[data_name] = {'time': times, 'value': values}
        return data

    # Generate Java property files to call TSClient for each TS instance (each needs a new one as sparql ep is different)
    def _create_property_files(self, data_confs) -> List[str]:
        outdir = self.base_conf['paths']['resourcesDir']
        sqlcfg = self.base_conf['rdb_access']
        outpaths = {}
        for datacfg in data_confs:
            dataname = datacfg['source']['name']
            props = PropertiesFileProtytype.copy()
            allcfg = dict(datacfg['kg_access']).copy()
            allcfg.update(dict(sqlcfg))
            updated_props = match_properties(props, allcfg)
            outpath = os.path.abspath(
                os.path.join(Path(__file__).parent.parent, outdir, "{}.properties".format(dataname)))
            write_java_properties_conf(updated_props, outpath)
            outpaths[dataname] = outpath
        return outpaths

    def init_RDB(self):
        sqlcfg = self.base_conf['rdb_access']
        dburls = sqlcfg['url'].split('/')
        db_name = dburls[-1]
        tsclient_wrapper.create_postgres_db_if_not_exists(db_name, sqlcfg['user'], sqlcfg['password'])

    def register_all_timeseries_if_not_exist(self):
        for data_name, data_factory in self.data_downloaders.items():
            TSClient = self.tsclients[data_name]
            if not TSClient.check_timeseries_exist(self.name_to_iris[data_name]):
                self._register_single_timeseries(data_name, data_factory)

    # Create postgresql tables and meta info triples of all timeseries data
    def _register_single_timeseries(self, data_name, data_factory):
        timeseries_meta = data_factory.get_tsmeta()
        TSClient = self.tsclients[data_name]
        TSClient.register_timeseries(timeseries_meta)

    def update_from_external_and_predict(self):
        for data_name, data_factory in self.data_downloaders.items():
            timeseries_instance = data_factory.download_tsinstance()
            TSClient = self.tsclients[data_name]
            updated = TSClient.update_timeseries_if_new(timeseries_instance)
            if updated:  # API have new data, needs to call prediction again
                # pad empty TS instance for prediction
                pad_start = int(timeseries_instance.times[
                                    -1]) + 1  # custom calculation of predict year start, allow other time type in future
                padded_for_forcast = data_factory.get_empty_tsinstance_for_predict(pad_start)
                TSClient.add_timeseries(padded_for_forcast)
                start = timeseries_instance.times[0]
                end = timeseries_instance.times[-1]
                history_duration = float(end) - float(start)
                forecast_cfg = data_factory.conf['forecast']
                predict_input = ForecastMeta(name=data_factory.dataname, iri=data_factory.src_iri,
                                             duration=history_duration, start_str=start, end_str=end,
                                             frequency=float(forecast_cfg['frequency']),
                                             unit_frequency=forecast_cfg['unitFrequency'])
                self.forecastclients[data_name].call_predict(predict_input)
