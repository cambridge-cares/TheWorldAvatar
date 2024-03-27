'''
This module contains functions to download from external data source in json/csv/xlsx and parse them to timeseries
'''
import json
import logging
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
# Things required for download/parser values:
# API => download (move to KG meta triples)
# mappings to output format (can change to RML future, currently lacking RML parser)
# Predict / Post operation/calculation

class Downloader:
    def __init__(self,
                 target_iri,
                 format,
                 url,
                 method,
                 value_iter,
                 time_iter,
                 calculation= None,
                 dynamic_generated = None,
                 method_dynamic = None,
                 **kwargs
                 ):
        self.target_iri = target_iri
        self.format = format
        self.format_parser = getattr(importlib.import_module("downloader.downloaders"), format + '_parser')
        self.url = url
        self.method = method
        self.value_iter =value_iter
        self.time_iter = time_iter
        self.dynamicGenerated = dynamic_generated
        print(self.dynamicGenerated)
        self.methodDynamic= method_dynamic
        self.post_calculation = getattr(importlib.import_module("data_classes.calculations"),
                                        calculation) if calculation else None

    def get_tsmeta(self) -> ts_data_classes.TimeSeriesMeta:
        return ts_data_classes.TimeSeriesMeta(time_unit=ts_data_classes.TSTR_FORMATS['Instant'], src_iri=self.target_iri)

    def download_tsinstance(self) -> ts_data_classes.TimeSeriesInstance:
        # Need to set a user agent to bypass forbidden issues
        magic_header = {
            'User-Agent': 'TWA_data_agent'
        }
        if self.dynamicGenerated:
            url_res = requests.request(self.methodDynamic, self.url, headers=magic_header)
            url_obj = url_res.json()
            print(url_obj)
            self.url = url_obj['url']
        print('request:')
        print(self.url)
        print(self.method)
        raw_response = requests.request(self.method, self.url, headers=magic_header)
        parsed = self.format_parser(raw_response.content if self.format == 'xlsx' else raw_response.text,
                                    self.target_iri,self.value_iter,self.time_iter)
        logging.debug(raw_response)
        logging.info('Successfully download data for {}'.format(self.target_iri))
        if self.post_calculation:
            parsed = self.post_calculation(parsed)
        return parsed


def csv_parser(raw_response, src_iri, value_iter, time_iter, **kwargs) -> ts_data_classes.TimeSeriesInstance:
    parsed_values, parsed_times = [], []
    lines = raw_response.splitlines()
    csv_reader = csv.reader(lines)
    headers = next(csv_reader)
    value_col = headers.index(value_iter)
    time_col = headers.index(time_iter)
    for row in csv_reader:
        parsed_values.append(row[int(value_col)])
        parsed_times.append(row[int(time_col)])
    return ts_data_classes.TimeSeriesInstance(values=parsed_values, times=parsed_times, src_iri=src_iri)


def json_parser(raw_response, src_iri, value_iter, time_iter, **kwargs) -> ts_data_classes.TimeSeriesInstance:
    raw_data = json.loads(raw_response)
    value_exp = jsonpath_ng.ext.parse(value_iter)
    time_exp = jsonpath_ng.ext.parse(time_iter)
    parsed_values = [match.value for match in value_exp.find(raw_data)]
    parsed_times = [match.value for match in time_exp.find(raw_data)]
    return ts_data_classes.TimeSeriesInstance(values=parsed_values, times=parsed_times, src_iri=src_iri)


def xlsx_parser(raw_response, src_iri, value_iter, time_iter, **kwargs) -> ts_data_classes.TimeSeriesInstance:
    # write a temporary xlsx files,
    tmpname = './{}.xlsx'.format(uuid.uuid4().hex)
    open(tmpname, 'wb').write(raw_response)
    vpaths = value_iter.split('/')
    tpaths = time_iter.split('/')
    sheetname = vpaths[0] if len(vpaths)>1 else None
    value_iter = vpaths[-1]
    time_iter = tpaths[-1]
    df = pd.read_excel(tmpname, sheet_name=sheetname)
    os.remove(tmpname)
    df = df.dropna(subset=[value_iter])  # remove any NaN
    return ts_data_classes.TimeSeriesInstance(src_iri=src_iri, values=df[value_iter].tolist(),
                                              times=df[time_iter].tolist())