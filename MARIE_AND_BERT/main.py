import time

from Marie.PubChem import PubChemEngine
import json
from flask import Flask, request
from flask import render_template, send_from_directory

app = Flask(__name__)
print(" - Initializing Pubchem Engine")
my_pubchem_engine = PubChemEngine()
# 
print(" - Done initializing Pubchem Engine")


def answer_question(question):
    print("Received question", question)
    answer = my_pubchem_engine.run(question=str(question).strip())
    print("Got answer", answer)
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


@app.route("/dashboard", methods=['GET'])
def dashboard():
    return render_template('dashboard.html')


#
# @app.route('/log')
# def stream():
#     def generate():
#         with open('marie.log') as f:
#             while True:
#                 yield f.read()
#                 time.sleep(1)
#     return app.response_class(generate(), mimetype='text/plain')


# @app.route('/full_test')
# def full_test():
#     return my_pubchem_engine.self_inspection()


if __name__ == "__main__":
    # Only for debugging while developing
    # app.run(host='0.0.0.0', debug=False, port=8080, threaded=False, processes=1)
    app.run(host='0.0.0.0', debug=False, port=8080)
