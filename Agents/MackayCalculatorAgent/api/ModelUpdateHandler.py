from flask_restful import Api, Resource, reqparse,request
from utils.calculator_model import CalculatorModel
from flask import jsonify

calculator = CalculatorModel()
class ModelUpdateHandler(Resource):
    def get(self):
        # note: may need to add parameters in future
        #Run access agent to retrieve data

        #Update value
        calculator.updateFromKG()

        final_ret = {"status": "Success"}
        return final_ret