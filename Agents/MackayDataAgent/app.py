from agents.mackay_data_agent import MackayDataAgent
from flask import Flask, Response
import json
import os,sys
from pathlib import Path


def create_app(testing=False):
    app = Flask(__name__)
    if testing:
        cfg = os.path.join( os.path.dirname(Path(__file__)), 'confs')
        print(cfg)
    else:
        cfg = os.path.join( os.path.dirname(os.path.abspath(__file__)), 'confs')
    agent = MackayDataAgent(cfg)
    agent.initiate()

    @app.route('/update', methods=['GET'])
    def run_update():
        try:
            agent.update_from_external_and_predict()
            final_ret = {"status": "Success"}
            return final_ret
        except Exception as e:
            app.logger.debug('update error: %s', e)
            return Response(
                str(e),
                status=500
            )


    @app.route('/data', methods=['GET'])
    def get_data():
        try:
            response = app.response_class(
                response=json.dumps(agent.get_data()),
                status=200,
                mimetype='application/json'
            )
            return response
        except Exception as e:
            app.logger.debug('get data error: %s', e)
            return Response(
                str(e),
                status=500
            )
    return app
