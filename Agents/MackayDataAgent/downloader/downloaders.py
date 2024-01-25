'''
This module contains functions to download from external data source in json/csv/xlsx and parse them to timeseries
'''
import json

import requests
from data_classes import ts_data_classes
import jsonpath_ng.ext
import configparser
import importlib
import string
import csv
import pandas as pd
import uuid
import os

'''
Use a ConfigParser object for configuration
Downloads from an external datasource
Returns a parsed timeseries data instance

'''


class Downloader:
    def __init__(self, conf):
        self.conf = conf
        self.format = format = self.conf['source']['format']
        self.format_parser = getattr(importlib.import_module("downloader.downloaders"), format + '_parser')
        post_cal_name = self.conf['output']['calculation'] if 'calculation' in self.conf['output'] else None
        self.post_calculation = getattr(importlib.import_module("data_classes.calculations"),
                                        post_cal_name) if post_cal_name else None

    def get_tsmeta(self) -> ts_data_classes.TimeSeriesMeta:
        src_iri = self.conf['output']['src_iri']
        timeUnit = self.conf['output']['timeUnit']
        return ts_data_classes.TimeSeriesMeta(time_unit=timeUnit, src_iri=src_iri)

    def download_tsinstance(self) -> ts_data_classes.TimeSeriesInstance:
        # Need to set a user agent to bypass forbidden issues
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
        raw_response = requests.request(conf['method'], url, headers=magic_header,
                                        params=dict(self.conf['sourceparams']) if 'sourceparams' in self.conf else None)
        # Add switch of parser types later
        print(dict(self.conf['output']))
        parsed = self.format_parser(raw_response.content if self.format == 'xlsx' else raw_response.text,
                                    **dict(self.conf['output']))
        if self.post_calculation:
            parsed = self.post_calculation(parsed)
        return parsed


def csv_parser(raw_response, src_iri, header, value_col, time_col, **kwargs) -> ts_data_classes.TimeSeriesInstance:
    parsed_values, parsed_times = [], []
    lines = raw_response.splitlines()
    csv_reader = csv.reader(lines)
    if header:
        next(csv_reader)
    for row in csv_reader:
        parsed_values.append(row[int(value_col)])
        parsed_times.append(row[int(time_col)])
    return ts_data_classes.TimeSeriesInstance(values=parsed_values, times=parsed_times, src_iri=src_iri)


def json_parser(raw_response, src_iri, valuexpath, timexpath, **kwargs) -> ts_data_classes.TimeSeriesInstance:
    raw_data = json.loads(raw_response)
    value_exp = jsonpath_ng.ext.parse(valuexpath)
    time_exp = jsonpath_ng.ext.parse(timexpath)
    parsed_values = [match.value for match in value_exp.find(raw_data)]
    parsed_times = [match.value for match in time_exp.find(raw_data)]
    return ts_data_classes.TimeSeriesInstance(values=parsed_values, times=parsed_times, src_iri=src_iri)


def xlsx_parser(raw_response, src_iri, value_col, time_col, sheetname, **kwargs) -> ts_data_classes.TimeSeriesInstance:
    # write a temporary xlsx files,TODO: handle tmp file
    tmpname = './{}.xlsx'.format(uuid.uuid4().hex)
    open(tmpname, 'wb').write(raw_response)
    df = pd.read_excel(tmpname, sheet_name=sheetname)
    os.remove(tmpname)
    value_col_label = df.columns[int(value_col)]
    time_col_label = df.columns[int(time_col)]
    df = df.dropna(subset=[value_col_label])  # remove any NaN
    return ts_data_classes.TimeSeriesInstance(src_iri=src_iri, values=df[value_col_label].tolist(),
                                              times=df[time_col_label].tolist())
