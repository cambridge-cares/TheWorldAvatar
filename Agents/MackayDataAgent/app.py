from agents.mackay_data_agent import MackayDataAgent
from flask import Flask, Response
import json

app = Flask(__name__)

agent = MackayDataAgent()
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
