'''
data reader from API
'''
import requests
from data_classes import ts_data_classes
import jsonpath_ng.ext
import configparser
import importlib
import string
import csv

def create_data_downloader(conf: configparser.ConfigParser):
    # Create a TS data reader from the properites specified in a configParser object
    reader_type = conf['source']['type']
    m_name, c_name = "downloader.downloaders", "{}Downloader".format(reader_type.capitalize())
    try:
        reader_class = getattr(importlib.import_module(m_name), c_name)
        reader = reader_class(conf)
        return reader
    except:
        raise ValueError('{} is not a valid reader class.'.format(reader_type))


'''
Use a ConfigParser object for configuration
Downloads from a selected type of datasource : [API]
Returns a parsed timeseries data instance

'''
class Downloader:
    def __init__(self, conf):
        self.conf = conf
        format = self.conf['source']['format']
        self.format_parser = getattr(importlib.import_module("downloader.downloaders"), format+'_parser')


    def get_tsmeta(self) -> ts_data_classes.TimeSeriesMeta:
        src_iri = self.conf['output']['srcIri']
        timeUnit = self.conf['output']['timeUnit']
        return ts_data_classes.TimeSeriesMeta(time_unit=timeUnit,src_iri=src_iri)

    def get_empty_tsinstance_for_predict(self, startyear:int) -> ts_data_classes.TimeSeriesInstance:
        # Allow other time type in future
        src_iri = self.conf['output']['srcIri']
        end = self.conf['forecast']['predictEnd']
        timevalues = [str(t) for t in range(int(startyear), int(end)+1)]
        values = [0.0 for _ in range(len(timevalues))]
        return ts_data_classes.TimeSeriesInstance(values=values, times=timevalues, src_iri=src_iri)

    def download_tsinstance(self) -> ts_data_classes.TimeSeriesInstance:
        #Need to set a user agent to bypass forbidden issues
        magic_header = {
            'User-Agent': 'TWA_data_agent'
        }
        conf = self.conf['source']
        if 'dynamicGenerated' in conf:
            url_res = requests.request(conf['methodDynamic'], conf['url'], headers=magic_header)
            url_obj = url_res.json()
            url = url_obj['url']
        else:
            url = conf['url']
        response = requests.request(conf['method'], url, headers=magic_header, params=dict(self.conf['sourceparams']) if 'sourceparams' in self.conf else None)
        raw_data = response.json()
        # Add switch of parser types later
        return self.format_parser(raw_data)


def csv_parser(raw_data, src_iri, header,valueCol,timeCol)-> ts_data_classes.TimeSeriesInstance:
    parsed_values, parsed_times = [], []
    reader = csv.reader(raw_data)
    if header:
        next(reader, None)
    for row in reader:
        parsed_values.append(row[valueCol])
        parsed_times.append(row[timeCol])
    return ts_data_classes.TimeSeriesInstance(values=parsed_values, times=parsed_times, src_iri=src_iri)


def json_parser(raw_data, src_iri, valueXpath, timeXpath) -> ts_data_classes.TimeSeriesInstance:
    value_exp = jsonpath_ng.ext.parse(valueXpath)
    time_exp = jsonpath_ng.ext.parse(timeXpath)
    parsed_values = [match.value for match in value_exp.find(raw_data)]
    parsed_times = [match.value for match in time_exp.find(raw_data)]
    return ts_data_classes.TimeSeriesInstance(values=parsed_values, times=parsed_times, src_iri=src_iri)
