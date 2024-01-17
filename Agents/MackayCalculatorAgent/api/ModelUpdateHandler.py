# Data Handler for update the Calculator with all defined values retrieve from KG queries
# ===============================================================================
from flask_restful import Api, Resource, reqparse,request
from flask import Response
from utils.calculator_model import CalculatorModel
from flask import jsonify
from utils.query_kg import query_all, query_data_agent
from utils.config import QUERYFILEPATH, DATA_AGENT_URL
import json
import datetime
from flask import current_app as app
calculator = CalculatorModel()
TSTR_FORMATS = '%Y-%m-%dT%H:%M:%SZ'

def read_from_kg():
    with open(QUERYFILEPATH) as query_json:
        query_dict = json.load(query_json)
        new_values = query_all(query_dict)
        return new_values

# output format: {'key': []}
def read_from_data_agent():
    raw_data = query_data_agent(DATA_AGENT_URL)
    with open(QUERYFILEPATH) as query_json:
        query_dict = json.load(query_json)
        data_filters = query_dict['data_filter']
        out = {}
        for filter in data_filters:
            if filter['name'] in raw_data:
                entry = raw_data[filter['name']]
                print([time_to_year(t) for t in entry['time']])
                out[filter['name']] =  [ v for t,v in zip(entry['time'],entry['value']) if time_to_year(t) in filter['times']]
        print(out)
        return out

def time_to_year(timestr):
    dt = datetime.datetime.strptime(timestr, TSTR_FORMATS)
    return dt.year

class ModelUpdateHandler(Resource):
    def get(self):
        # note: may need to add parameters in future
        #Run access agent to retrieve data
        #Update
        try:
            new_values = read_from_data_agent()
            #TODO: test this work for new defined update dict with list
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
