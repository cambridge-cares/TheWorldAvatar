import time

from Marie.CrossGraphQAEngine import CrossGraphQAEngine
import json
from flask import Flask, request
from flask import render_template, send_from_directory
from Marie.Util.Logging import MarieLogger
import Marie.Util.Web.SPARQLHandshake as Handshake

logger = MarieLogger()

app = Flask(__name__)
logger.info("============= Initializing the server ===========")
logger.info("1. Initializing Cross Graph Engine")
my_qa_engine = CrossGraphQAEngine()
logger.info(" - Done initializing Cross Graph Engine")
logger.info("============= Server is ready to go! ===========")


def answer_question(question):
    logger.info("=======================================================================================")
    logger.info(f" The server received a question: {question}")
    answer = my_qa_engine.run(original_question=str(question).strip(), disable_alignment=False,
                              heads={})
    logger.info(f" The server returned answers: {json.dumps(answer)}")
    logger.info("=======================================================================================")
    return answer


@app.route('/')
def hello_world():
    return render_template('index_new.html')


@app.route('/static/<path:path>')
def send_js(path):
    return send_from_directory('static', path)


@app.route("/search", methods=['GET'])
def search():
    args = request.args
    question = args["question"]
    answer = answer_question(question)
    return json.dumps(answer)


# @app.route("/dashboard", methods=['GET'])
# def dashboard():
#     return render_template('dashboard.html')
#
#
# @app.route('/error_log')
# def stream_error():
#     def generate_error_log():
#         with open('error.log') as f:
#             while True:
#                 yield f.read()
#                 time.sleep(1)
#
#     return app.response_class(generate_error_log(), mimetype='text/plain')
#
#
# @app.route('/debug_log')
# def stream_debug():
#     def generate_debug_log():
#         with open('debug.log') as f:
#             while True:
#                 yield f.read()
#                 time.sleep(1)
#
#     return app.response_class(generate_debug_log(), mimetype='text/plain')


# @app.route('/full_test')
# def full_test():
#     return my_pubchem_engine.self_inspection()
#
#
# @app.route('/test_entity_linking')
# def test_entity_linking():
#     return my_pubchem_engine.test_entity_linking()
#
#
# @app.route('/test_score_model')
# def test_score_model():
#     return my_pubchem_engine.test_score_model()
#
#
# @app.route('/test_value_lookup')
# def test_value_lookup():
#     return my_pubchem_engine.test_value_lookup()


# @app.route('/hand_shake_pubchem')
# def test_hand_shake_pubchem():
#     if Handshake.handshake_pubchem():
#         return "Success: Pubchem endpoint is running properly"
#     else:
#         return "Error: Pubchem endpoint is NOT running properly"
#
#
# @app.route('/hand_shake_ontocompchem')
# def test_hand_shake_ontocompchem():
#     if Handshake.handshake_ontocompochem():
#         return "Success: OntoCompchem endpoint is running properly"
#     else:
#         return "Error: OntoCompchem endpoint is NOT running properly"


if __name__ == "__main__":
    # Only for debugging while developing
    # app.run(host='0.0.0.0', debug=False, port=8080, threaded=False, processes=1)
    app.run(host='0.0.0.0', debug=False, port=5003)
