import json
import sys, os
import sys

# source_path = os.path.join(file_path, 'UI/source')
# sys.path.insert(1, source_path)

from pprint import pprint

from flask import Flask, request
from flask import render_template, send_from_directory
from flask_cors import CORS
from flask_socketio import SocketIO, send, emit
from functools import lru_cache

# sys.path.insert(1, os.path.realpath(os.path.dirname(__file__)))
# sys.path.append('/source')
from UI.source.wolfram_alpha_and_google.GoogleAPI import GoogleAPI
from UI.source.wolfram_alpha_and_google.WolframGoogle import WolframGoogle
from UI.source.CoordinateAgent import CoordinateAgent

app = Flask(__name__)
CORS(app)
socketio = SocketIO(app)
socketio.init_app(app, cors_allowed_origins="*")


@app.route('/chemistry_chatbot/static/<path:path>')
def send_js(path):
    return send_from_directory('static', path)


# this serves as a web interface for the http request from agent wrapper, Question classification
@app.route('/chemistry_chatbot/QuestionClassification')
def question_classification_nlu():
    question = request.args.get('question')
    return CoordinateAgent.question_classification(question)


# this serves as a web interface for the http request from agent wrapper, Question classification
@app.route('/chemistry_chatbot/NamedEntityClassification')
def question_classification():
    question = request.args.get('question')
    return CoordinateAgent.named_entity_recognition(question)


@app.route('/chemistry_chatbot/OntologyLookup')
def ontology_lookup():
    term = request.args.get('term')
    return CoordinateAgent.parse_entities(term)


@app.route('/chemistry_chatbot/query')
def make_query():
    question_type = request.args.get('type')
    question = request.args.get('question')

    try:
        if question_type == 'worldavatar':
            result = coordinate_agent.run(question)
            return json.dumps(result)
        elif question_type == 'google':
            question = request.args.get('question').strip()
            r = google_api.run(question)
            return str(r)

        elif question_type == 'wolfram':
            result = wolfram_and_google.get_result_from_wolfram(question)
            return json.dumps(result)

    except:
        return 'Nothing'


@app.route('/')
def hello_world():
    return render_template('index_dln22.html')



google_api = GoogleAPI()
coordinate_agent = CoordinateAgent(socketio)
wolfram_and_google = WolframGoogle()

if __name__ == '__main__':
    app.run(host='https://kg.cmclinnovations.com/', port=8080, debug=True)
