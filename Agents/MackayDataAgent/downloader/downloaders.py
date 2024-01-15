'''
data reader from API
'''
import requests
from data_types import ts_data_classes
import jsonpath_ng.ext
import configparser
import importlib
import string

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


class ApiDownloader:
    def __init__(self, conf):
        self.conf = conf
        self.dataname = self.conf['source']['name']
        self.src_iri = self.conf['output']['srcIri']

    def get_tsmeta(self) -> ts_data_classes.TimeSeriesMeta:
        src_iri = self.conf['output']['srcIri']
        timeUnit = self.conf['output']['timeUnit']
        return ts_data_classes.TimeSeriesMeta(time_unit=timeUnit,src_iri=src_iri)

    def get_empty_tsinstance_for_predict(self, startyear:int) -> ts_data_classes.TimeSeriesInstance:
        # Allow other time type in future
        src_iri = self.conf['output']['srcIri']
        end = self.conf['output']['predictEnd']
        timevalues = [str(t) for t in range(int(startyear), int(end)+1)]
        values = [0.0 for _ in range(len(timevalues))]
        return ts_data_classes.TimeSeriesInstance(values=values, times=timevalues, src_iri=src_iri)

    def download_tsinstance(self) -> ts_data_classes.TimeSeriesInstance:
        ##A magic header to bypass some server forbidden issues
        magic_header = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.76 Safari/537.36',
            "Upgrade-Insecure-Requests": "1", "DNT": "1",
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            "Accept-Language": "en-US,en;q=0.5", "Accept-Encoding": "gzip, deflate"}
        conf = self.conf['source']
        response = requests.request(conf['method'], conf['url'], headers=magic_header, params=dict(self.conf['sourceparams']))
        raw_data = response.json()
        # Add switch of parser types later
        return self.json_parser(raw_data)

    def json_parser(self, raw_data) -> ts_data_classes.TimeSeriesInstance:
        src_iri = self.conf['output']['srcIri']
        valueXpath = self.conf['output']['valueXpath']
        timeXpath = self.conf['output']['timeXpath']
        value_exp = jsonpath_ng.ext.parse(valueXpath)
        time_exp = jsonpath_ng.ext.parse(timeXpath)
        parsed_values = [match.value for match in value_exp.find(raw_data)]
        parsed_times = [match.value for match in time_exp.find(raw_data)]
        return ts_data_classes.TimeSeriesInstance(values=parsed_values, times=parsed_times, src_iri=src_iri)
