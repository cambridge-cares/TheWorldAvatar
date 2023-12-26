# Data Handler for update the Calculator with all defined values retrieve from KG queries
# ===============================================================================
from flask_restful import Api, Resource, reqparse,request
from flask import Response
from utils.calculator_model import CalculatorModel
from flask import jsonify
from utils.query_kg import query_all
from utils.config import QUERYFILEPATH
import json

calculator = CalculatorModel()
class ModelUpdateHandler(Resource):
    def get(self):
        # note: may need to add parameters in future
        #Run access agent to retrieve data
        #Update
        try:
            with open(QUERYFILEPATH) as query_json:
                query_dict = json.load(query_json)
                new_values = query_all(query_dict)
                calculator.updateFromKG(new_values)
                final_ret = {"status": "Success"}
                return final_ret
        except Exception as e:
            return Response(
                str(e),
                status=500
            )
