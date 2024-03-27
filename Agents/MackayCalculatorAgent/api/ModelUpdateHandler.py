# Data Handler for update the Calculator with all defined values retrieve from KG queries
# ===============================================================================
from flask_restful import Api, Resource, reqparse,request
from flask import Response
from mackay.calculator_model import CalculatorModel
from flask import jsonify
from utils.query_kg import query_all, query_data_agent
from utils.config import QUERYFILEPATH, DATA_AGENT_URL
import json
import datetime
from dateutil.parser import parse
from flask import current_app as app
import importlib
calculator = CalculatorModel()
TSTR_FORMAT = '%Y-%m-%dT%H:%M:%SZ'
DEFAULT_TIME = datetime.datetime.strptime('01/01/01 00:00:00', '%m/%d/%y %H:%M:%S')

def read_from_kg():
    with open(QUERYFILEPATH) as query_json:
        query_dict = json.load(query_json)
        new_values = query_all(query_dict)
        return new_values

def parse_incomplete_time(titem):
    if type(titem) != str:
        titem = str(titem)
    return parse(titem, default=DEFAULT_TIME).strftime(TSTR_FORMAT)

def placeholder(v):
    return v

# output format: {'key': []}
def read_from_data_agent():
    raw_data = query_data_agent(DATA_AGENT_URL)
    with open(QUERYFILEPATH) as query_json:
        query_dict = json.load(query_json)
        data_filters = query_dict['data_filter']
        out = {}
        for filterobj in data_filters:
            unit_convert_name = "unit_convert_{}".format(filterobj['unit_convert'] ) if 'unit_convert' in filterobj else None
            unit_convert_func = getattr(importlib.import_module("utils.query_kg"),unit_convert_name) if unit_convert_name else placeholder
            filterobj['times'] = [ parse_incomplete_time(t) for t in filterobj['times']]
            print(filterobj['times'])
            if filterobj['name'] in raw_data:
                entry = raw_data[filterobj['name']]
                print(entry['time'])
                out[filterobj['name']] =  [ unit_convert_func(v) for t,v in zip(entry['time'],entry['value']) if t in filterobj['times']]
        print('out values:')
        print(out)
        return out


def time_to_year(timestr):
    dt = datetime.datetime.strptime(timestr, TSTR_FORMAT)
    return dt.year

class ModelUpdateHandler(Resource):
    def get(self):
        # note: may need to add parameters in future
        #Run access agent to retrieve data
        #Update
        try:
            new_values = read_from_data_agent()
            calculator.updateFromKG(new_values)
            final_ret = {"status": "Success"}
            app.logger.info('Updated Mackay model values from TWA')
            return final_ret
        except Exception as e:
            app.logger.debug('ModelUpdateHandler error: %s', e)
            return Response(
                str(e),
                status=500
            )
