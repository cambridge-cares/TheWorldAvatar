from mackay_data_agent.mackay_data_agent import MackayDataAgent
from flask import Flask, Response
import json
import os,sys
from pathlib import Path
import logging

def create_app(testing=False):
    app = Flask(__name__)
    if testing:
        cfg = os.path.join(os.path.dirname(Path(__file__)), 'confs_files')
        print(cfg)
    else:
        cfg = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'confs_files')
    agent = MackayDataAgent(cfg)
    agent.init_model_APIs()


    @app.route('/', methods=['GET'])
    def default():
        """
            Instructional message at the app root.
        """
        msg  = "This is the data agent that manages and serves the Mackay calculator data."
        return msg



    @app.route('/update', methods=['GET'])
    def run_update():
        try:
            agent.update_model()
            final_ret = {"status": "Success"}
            return final_ret
        except Exception as e:
            app.logger.debug('update error: %s', e)
            return Response(
                str(e),
                status=500
            )


    @app.route('/timeseries_data', methods=['GET'])
    def get_timeseries_data():
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

    @app.route('/meta_data', methods=['GET'])
    def get_meta_data():
        try:
            response = app.response_class(
                response=json.dumps(agent.query_meta_data()),
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


if __name__ == "__main__":
    myapp = create_app()
    myapp.run(port=5002)