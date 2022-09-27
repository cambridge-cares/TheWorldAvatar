import time

from Marie.PubChem import PubChemEngine
import json
from flask import Flask, request
from flask import render_template, send_from_directory

app = Flask(__name__)
my_pubchem_engine = PubChemEngine()


@app.route('/')
def hello_world():
    return render_template('index_agent_for_develop.html')


@app.route('/static/<path:path>')
def send_js(path):
    return send_from_directory('static', path)


@app.route("/search", methods=['GET'])
def search():
    '''
    :return:
    '''
    args = request.args
    question = args["question"]
    return json.dumps(my_pubchem_engine.run(str(question).strip()))


@app.route("/dashboard", methods=['GET'])
def dashboard():

    
    return render_template('dashboard.html')


@app.route('/log')
def stream():
    def generate():
        with open('marie.log') as f:
            while True:
                yield f.read()
                time.sleep(1)

    return app.response_class(generate(), mimetype='text/plain')

if __name__ == "__main__":
    # Only for debugging while developing
    app.run(host='0.0.0.0', debug=True, port=8080)
