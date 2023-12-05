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
        print(len(request_json['levers']))
        action = request_json['action']
        pos_cate = request_json['options']

        calculator.twerkTillZero(action, pos_cate)
        plotdata,singlevalues = calculator.getData()
        print(plotdata[1])
        final_ret = {"status": "Success", "values": plotdata}
        final_ret.update(singlevalues)
        return final_ret