import time

import json
from flask import Flask, request
from flask import render_template, send_from_directory
from Marie.Util.Logging import MarieLogger
import Marie.Util.Web.SPARQLHandshake as Handshake

logger = MarieLogger()

app = Flask(__name__)
timer = 0

def answer_question(question):
    #answer = '{"multiple_results":[{"value": ["65.17", "65.51", "82.54", "96.74", "108.19", "117.53", "125.32", "131.93", "137.61", "146.76", "156.47", "161.08", "166.15", "171.51", "174.72", "176.76", "178.14", "179.10", "179.81"], "unit": "J/mol/K"}]}'

    answer = [{
    "node": "{\"Heat capacity at constant pressure\": {\"y\": {\"value\": [\"65.17\", \"65.51\", \"82.54\", \"96.74\", \"108.19\", \"117.53\", \"125.32\", \"131.93\", \"137.61\", \"146.76\", \"156.47\", \"161.08\", \"166.15\", \"171.51\", \"174.72\", \"176.76\", \"178.14\", \"179.10\", \"179.81\"], \"unit\": \"J/mol/K\"}, \"x\": {\"value\": [\"298.15\", \"300.00\", \"400.00\", \"500.00\", \"600.00\", \"700.00\", \"800.00\", \"900.00\", \"1000.00\", \"1200.00\", \"1500.00\", \"1700.00\", \"2000.00\", \"2500.00\", \"3000.00\", \"3500.00\", \"4000.00\", \"4500.00\", \"5000.00\"], \"unit\": \"K\"}}, \"Heat capacity at constant volume\": {\"y\": {\"value\": [\"56.86\", \"57.19\", \"74.22\", \"88.43\", \"99.87\", \"109.21\", \"117.00\", \"123.62\", \"129.29\", \"138.44\", \"148.15\", \"152.77\", \"157.84\", \"163.20\", \"166.40\", \"168.45\", \"169.82\", \"170.79\", \"171.49\"], \"unit\": \"J/mol/K\"}, \"x\": {\"value\": [\"298.15\", \"300.00\", \"400.00\", \"500.00\", \"600.00\", \"700.00\", \"800.00\", \"900.00\", \"1000.00\", \"1200.00\", \"1500.00\", \"1700.00\", \"2000.00\", \"2500.00\", \"3000.00\", \"3500.00\", \"4000.00\", \"4500.00\", \"5000.00\"], \"unit\": \"K\"}}}",
    "domain": "ontoagent",
    "score": 2.0,
    "target": "EMPTY SLOT"}]

    answer_wrong = [{
        "bullshit_node": "{\"Heat capacity at constant pressure\": {\"y\": {\"value\": [\"65.17\", \"65.51\", \"82.54\", \"96.74\", \"108.19\", \"117.53\", \"125.32\", \"131.93\", \"137.61\", \"146.76\", \"156.47\", \"161.08\", \"166.15\", \"171.51\", \"174.72\", \"176.76\", \"178.14\", \"179.10\", \"179.81\"], \"unit\": \"J/mol/K\"}, \"x\": {\"value\": [\"298.15\", \"300.00\", \"400.00\", \"500.00\", \"600.00\", \"700.00\", \"800.00\", \"900.00\", \"1000.00\", \"1200.00\", \"1500.00\", \"1700.00\", \"2000.00\", \"2500.00\", \"3000.00\", \"3500.00\", \"4000.00\", \"4500.00\", \"5000.00\"], \"unit\": \"K\"}}, \"Heat capacity at constant volume\": {\"y\": {\"value\": [\"56.86\", \"57.19\", \"74.22\", \"88.43\", \"99.87\", \"109.21\", \"117.00\", \"123.62\", \"129.29\", \"138.44\", \"148.15\", \"152.77\", \"157.84\", \"163.20\", \"166.40\", \"168.45\", \"169.82\", \"170.79\", \"171.49\"], \"unit\": \"J/mol/K\"}, \"x\": {\"value\": [\"298.15\", \"300.00\", \"400.00\", \"500.00\", \"600.00\", \"700.00\", \"800.00\", \"900.00\", \"1000.00\", \"1200.00\", \"1500.00\", \"1700.00\", \"2000.00\", \"2500.00\", \"3000.00\", \"3500.00\", \"4000.00\", \"4500.00\", \"5000.00\"], \"unit\": \"K\"}}}",
        "domain": "ontoagent",
        "score": 2.0,
        "target": "EMPTY SLOT"}]


    answer = [{'node': 'CID297_tpsa', 'domain': 'pubchem', 'score': 2.0, 'target': 'methane'}, {'node': 'CID297_inchi_standard', 'domain': 'pubchem', 'score': 1.3661125898361206, 'target': 'methane'}, {'node': 'CID297_count_def_bond_stereo', 'domain': 'pubchem', 'score': 1.2681689262390137, 'target': 'methane'}, {'node': 'CID297_count_hydrogen_bond_donor', 'domain': 'pubchem', 'score': 1.2308056354522705, 'target': 'methane'}, {'node': 'CID297_count_hydrogen_bond_acceptor', 'domain': 'pubchem', 'score': 1.2289433479309082, 'target': 'methane'}]
    #'[{"node": "{\"Power conversion efficiency\": [2.4610229511396935]}", "domain": "ontoagent", "score": 2.0, "target": "EMPTY SLOT"}]'
    global  timer
    timer +=  1
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
    app.run(host='0.0.0.0', debug=True, port=5003)
