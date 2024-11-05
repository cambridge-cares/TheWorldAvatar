'''
Data agent that manages initiations and updates of timeseries data required by Mackay model.
Download from external APIs and run Forecast for the list of datapoints specified in agent config files.
'''
import importlib
import logging
import os.path
from kg_access import tsclient_wrapper, forecast_client, api_agent_client
from typing import List
from kg_access.kgClient import KGClient
from kg_access.tsclient_wrapper import TSClient
from utils.conf_utils import *
from data_classes.ts_data_classes import  ForecastMeta, KgAccessInfo, get_padded_TSInstance,parse_incomplete_time,TimeSeriesInstance,get_duration_in_days,parse_time_to_format


class MackayDataAgent:
    def __init__(self, confdir='./confs'):
        # Input: IRI of API agent
        # Input: IRI of forecast agent
        # Input: A list of API map IRIs (to register and retrieve data point)
        # Input: forecast/calculation config for each data point
        self.base_conf = load_conf(os.path.join(confdir, 'base.cfg'))
        self.api_agent = api_agent_client.APIAgentClient(self.base_conf["output"]["base_url"], **self.base_conf["api_agent"],**self.base_conf["kg_access"])
        data_confs = self.data_confs = load_confs_from_dir(os.path.join(confdir, 'data'))
        self.meta_queries = load_json(os.path.join(confdir, 'data/meta_queries.json'))
        property_file = create_property_file('base', **self.base_conf['rdb_access'], **self.base_conf['kg_access'])
        self.ts_client = TSClient(property_file)
        self.name_to_iris = {d["output"]["name"]: d["output"]["target_iri"] for d in data_confs}
        self.name_to_apis = {d["output"]["name"]: d["output"]["api_iri"] for d in data_confs}
        self.predict_cfgs = {d["output"]["name"]: d[d["output"]["steps"]] for d in data_confs if d["output"]["steps"]  == 'forecast'}
        self.calculation_cfgs = {d["output"]["name"]:  d[d["output"]["steps"]] for d in data_confs if d["output"]["steps"]  == 'calculation'}
        self.forecast_client = forecast_client.ForcastAgentClient(self.base_conf['forecast_agent']['agent_url'], self.base_conf['forecast_agent']['agent_iri'],KgAccessInfo(**self.base_conf['kg_access']),self.base_conf["output"]["base_url"] )

    # main function : retrieve current data in RDB of all APIs, if requires forecast, return forecasted TS, if requires calculation, return cal-result
    def get_data(self, forecast=True) -> dict:
        data = {}
        for data_name in self.name_to_iris:
            data_iri = self.name_to_iris[data_name]
            times, values = self.ts_client.get_timeseries(data_iri)
            if forecast and data_name in self.predict_cfgs:#If both global flag to return forecast and local flag of has forecast are true
                forecast_iri = self.forecast_client.get_forecast_iri(data_iri)
                ftimes, fvalues = self.ts_client.get_timeseries(forecast_iri)
                times.extend(ftimes)  # append forecasted values to real TS values
                values.extend(fvalues)
            if data_name in self.calculation_cfgs:
                cal_cfg = self.calculation_cfgs[data_name]
                pre_cal_func= cal_cfg["function"]
                cal = getattr(importlib.import_module("data_classes.calculations"),
                              pre_cal_func) if pre_cal_func else None
                updated_ts = cal(TimeSeriesInstance(src_iri='',values=values,times=times),**cal_cfg)
                values = updated_ts.values
                times = [ parse_time_to_format(t) for t in updated_ts.times]
            data[data_name] = {'time': times, 'value': values}
        logging.debug(data)
        return data
    
    def query_meta_data(self) -> dict:
        data = {}
        for meta_query in self.meta_queries.get('queries'):
            query_endpoint = meta_query.get('url')
            kg_user = meta_query.get('user')
            kg_password = meta_query.get('pwd')
            query_string = meta_query.get('query_string')
            variable_name = meta_query.get('variable_name')
            self.kg_client = KGClient(query_endpoint, query_endpoint, kg_user, kg_password)
            query_response = self.kg_client.performQuery(query_string)
            logging.info(query_response)
            data[variable_name] = query_response[0]
            logging.info(data)
        return data
        

    def update_model(self):
        self.update_predict()

    def init_model_APIs(self):    # Call API register on api agent if not already registered
        for name in self.name_to_apis:
            map_iri = self.name_to_apis[name]
            registered = self.api_agent.check_if_API_registered(map_iri)
            print(registered)
            if not registered:
                logging.info('{} not in API agent, reggister now'.format(map_iri))
                self.api_agent.register_API(map_iri)
            else:
                logging.info('{} exists in API agent'.format(map_iri))


    #update predict with derivation logic
    def update_predict(self):
        for data_name in self.name_to_iris:
            data_iri = self.name_to_iris[data_name]
            forecast_iri = self.forecast_client.get_forecast_iri(data_iri)
            logging.info('has exist forecast {}'.format(forecast_iri))
            # Call predication agent if: requires to predict and either A. an update in timeseries record B. no predication has ever been made
            if forecast_iri:
                needs_update = self.forecast_client.check_if_TS_update(forecast_iri)
            else:
                needs_update = False
            if data_name in self.predict_cfgs and (needs_update or not forecast_iri):# either no forecast exist (first name), or needs update since original TS is updated
                # pad empty TS instance for prediction
                logging.info('start forcast')
                forecast_cfg = self.predict_cfgs[data_name]
                predict_end = parse_incomplete_time(forecast_cfg['predictEnd'])
                times,values = self.ts_client.get_timeseries(data_iri)
                timeseries_instance = TimeSeriesInstance(times,values,data_iri)
                padding_ts = get_padded_TSInstance(data_iri,forecast_cfg['unitFrequency'], timeseries_instance.times[-1],predict_end )
                logging.info('Prepare for forecast')
                history_duration = get_duration_in_days(timeseries_instance.times[0],timeseries_instance.times[-1])
                predict_input = ForecastMeta(name=data_name, iri=data_iri,
                                             duration=history_duration, start_dt=padding_ts.times[0], end_dt=padding_ts.times[-1],
                                             frequency=float(forecast_cfg['frequency']),
                                             unit_frequency=forecast_cfg['unitFrequency'])
                self.forecast_client.call_predict(predict_input)