import json
import sys, os
import sys

# source_path = os.path.join(file_path, 'UI/source')
# sys.path.insert(1, source_path)
import time

from pprint import pprint

from flask import Flask, request, send_file
from flask import render_template, send_from_directory
from flask_cors import CORS
from flask_socketio import SocketIO, send, emit
from functools import lru_cache

# sys.path.insert(1, os.path.realpath(os.path.dirname(__file__)))
# sys.path.append('/source')
if __name__ == "__main__":
    from CoordinateAgent import CoordinateAgent
    from full_test import FullTest
else:
    from .CoordinateAgent import CoordinateAgent
    from .full_test import FullTest

app = Flask(__name__)
CORS(app)
socketio = SocketIO(app)
socketio.init_app(app, cors_allowed_origins="*")

# Instantiate Coordinate Agent
coordinate_agent = CoordinateAgent()

@app.route('/start_test')
def start_test():
    ft = FullTest()
    ft.start()
    return 'starting the full test'


@app.route('/log')
def stream():
    def generate():
        with open('Marie.log') as f:
            while True:
                yield f.read()
                time.sleep(1)

    return app.response_class(generate(), mimetype='text/plain')


@app.route('/chemistry_chatbot/static/<path:path>')
def send_js(path):
    return send_from_directory('static', path)


@app.route('/chemistry_chatbot/mock_pce_agent')
def pce_agent():
    species = request.args.get('species')
    print(species)
    return '0.55'


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

    # N.B. It's possible to have different 'type' values, but only 'worldavatar' is currently used
    try:
        if question_type == 'worldavatar':
            result = coordinate_agent.run(question)
            return json.dumps(result)
        else: 
            raise ValueError("%s isn't a valid value for the 'type' parameter." % question_type)
    except:
        return 'Nothing'


@app.route('/')
def hello_world():
    return render_template('index.html')

if __name__ == '__main__':
    app.run(host='https://kg.cmclinnovations.com/', port=8080, debug=True)
