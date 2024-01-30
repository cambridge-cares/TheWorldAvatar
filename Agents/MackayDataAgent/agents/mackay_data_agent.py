'''
Data agent that manages initiations and updates of timeseries data required by Mackay model.
Download from external APIs and run Forecast for the list of datapoints specified in agent config files.
'''
import importlib
import logging
import os.path
from pathlib import Path
from kg_access import tsclient_wrapper, forecast_client
from typing import List
from downloader.downloaders import Downloader
from utils.conf_utils import *
from data_classes.ts_data_classes import  ForecastMeta, KgAccessInfo, get_padded_TSInstance,parse_incomplete_time,TimeSeriesInstance,get_duration_in_days,parse_time_to_format


class MackayDataAgent:
    def __init__(self, confdir='./confs'):
        self.base_conf = load_conf(os.path.join(confdir, 'base.cfg'))
        data_confs = load_confs_from_dir(os.path.join(confdir, 'data'))
        self.property_files_dict = create_property_files(self.base_conf,data_confs)
        self.tsclients = {}
        self.forecastclients = {}
        self.name_to_iris = {d["source"]["name"]: d["output"]["src_iri"] for d in data_confs}
        self.data_downloaders = {d["source"]["name"]: Downloader(d) for d in data_confs}
        self.requires_predict = {d["source"]["name"]: True if 'forecast' in d else False for d in data_confs}
        self.pre_serve = {d["source"]["name"]: d["pre_serve"]["calculation"] if 'pre_serve' in d else None for d in data_confs}
        for cfg in data_confs:
            data_name = cfg["source"]["name"]
            self.tsclients[data_name] = tsclient_wrapper.TSClient(self.property_files_dict[data_name])
            self.forecastclients[data_name] = forecast_client.ForcastAgentClient(
                self.base_conf['forecast_agent']['url'], self.base_conf['forecast_agent']['iri'],
                KgAccessInfo(**cfg['kg_access']))

    # Initiate the agent: 1. init RDB tables 2. register all data timeseries in RDB and KB
    def initiate(self):
        self.init_RDB()
        self.register_all_timeseries_if_not_exist()

    def get_data(self, forecast=True) -> dict:
        data = {}
        for data_name in self.name_to_iris:
            TSClient = self.tsclients[data_name]
            data_iri = self.name_to_iris[data_name]
            times, values = TSClient.get_timeseries(data_iri)
            if forecast and self.requires_predict[data_name]:#If both global flag to return forecast and local flag of has forecast are true
                forecast_client = self.forecastclients[data_name]# concat orignal timeseries with forecasted ones
                forecast_iri = forecast_client.get_forecast_iri(data_iri)
                ftimes, fvalues = TSClient.get_timeseries(forecast_iri)
                times.extend(ftimes)
                values.extend(fvalues)
            if self.pre_serve[data_name]:
                pre_cal_func= self.pre_serve[data_name]
                cal = getattr(importlib.import_module("data_classes.calculations"),
                              pre_cal_func) if pre_cal_func else None
                updated_ts = cal(TimeSeriesInstance(src_iri='',values=values,times=times))
                values = updated_ts.values
                times = [ parse_time_to_format(t) for t in updated_ts.times]
            data[data_name] = {'time': times, 'value': values}
        logging.debug(data)
        return data



    # Create DB if not exist
    def init_RDB(self):
        sqlcfg = self.base_conf['rdb_access']
        dburls = sqlcfg['url'].split('/')
        db_name = dburls[-1]
        tsclient_wrapper.create_postgres_db_if_not_exists(db_name, sqlcfg['user'], sqlcfg['password'])

    # Register TS in RDB and KB if not exist
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

    # main function : download TS data from API and run forecasting agent to create forecasted timeseries
    def update_from_external_and_predict(self, force_predict= True):
        for data_name, data_factory in self.data_downloaders.items():
            data_iri = self.name_to_iris[data_name]
            timeseries_instance = data_factory.download_tsinstance()
            TSClient = self.tsclients[data_name]
            updated = TSClient.update_timeseries_if_new(timeseries_instance)
            exist_predict = self.forecastclients[data_name].get_forecast_iri(data_iri)
            # Call predication agent if: requires to predict and either A. an update in timeseries record B. no predication has ever been made
            if self.requires_predict[data_name] and (updated or not exist_predict or force_predict):  #Run a required predict session if updated/no previous record/ force predict flag set
                # pad empty TS instance for prediction
                forecast_cfg = data_factory.conf['forecast']
                predict_end = parse_incomplete_time(forecast_cfg['predictEnd'])
                padding_ts = get_padded_TSInstance(data_iri,forecast_cfg['unitFrequency'], timeseries_instance.times[-1],predict_end )
                #TSClient.add_timeseries(padding_ts)
                logging.info('Prepare for forecast')
                history_duration = get_duration_in_days(timeseries_instance.times[0],timeseries_instance.times[-1])
                predict_input = ForecastMeta(name=data_name, iri=data_iri,
                                             duration=history_duration, start_dt=padding_ts.times[0], end_dt=padding_ts.times[-1],
                                             frequency=float(forecast_cfg['frequency']),
                                             unit_frequency=forecast_cfg['unitFrequency'])
                self.forecastclients[data_name].call_predict(predict_input)