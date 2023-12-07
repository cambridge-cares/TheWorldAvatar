from flask_restful import Api, Resource, reqparse,request
from utils.calculator_model import CalculatorModel
from flask import jsonify

calculator = CalculatorModel()
class PlotDataHandler(Resource):
    def post(self):
        # note, the post req from frontend needs to match the strings here (e.g. 'type and 'message')
        request_json = request.json
        # ret_status, ret_msg = ReturnData(request_type, request_json)
        # currently just returning the req straight
        calculator.setControls(request_json['levers'])
        plotdata,singlevalues = calculator.getData()
        final_ret = {"status": "Success", "values": plotdata}
        final_ret.update(singlevalues)
        return final_ret