# Data Handler to twerk the handler to back to net zero after an specified action on a specified lever, the new plot charts dta are returned
# This router is still in development and not used for deployment
# ===============================================================================
from flask_restful import Api, Resource, reqparse,request
from utils.calculator_model import CalculatorModel
from flask import jsonify

calculator = CalculatorModel()
class ModelTwerkHandler(Resource):
    def post(self):
        # note, the post req from frontend needs to match the strings here (e.g. 'type and 'message')

        request_json = request.json
        # ret_status, ret_msg = ReturnData(request_type, request_json)
        # currently just returning the req straight
        action = request_json['action']
        pos_cate = request_json['options']

        calculator.twerkTillZero(action, pos_cate)
        plotdata,singlevalues = calculator.getData()
        final_ret = {"status": "Success", "values": plotdata}
        final_ret.update(singlevalues)
        return final_ret