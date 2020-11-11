import json
import sys, os

# source_path = os.path.join(file_path, 'UI/source')
# sys.path.insert(1, source_path)

from pprint import pprint

from flask import Flask, request
from flask import render_template, send_from_directory
from flask_socketio import SocketIO, send, emit
from functools import lru_cache
sys.path.insert(1, os.path.realpath(os.path.dirname(__file__)))
sys.path.append('/source')
app = Flask(__name__)
socketio = SocketIO(app)
socketio.init_app(app, cors_allowed_origins="*")

@app.route('/chemistry_chatbot/static/<path:path>')
def send_js(path):
    print('getting request from client')
    return send_from_directory('static',path)


@app.route('/query')
def make_query():
    try:
        question = request.args.get('question')
        print(question)
        print('the questions received', question)
        result = coordinate_agent.run(question)
        pprint(result)
        return json.dumps(result)
    except:
        return 'Nothing'

@app.route('/query_wolfram')
def make_query_wolfram():
    socketio.emit('coordinate_agent', 'Querying the wolfram alpha engine')
    question = request.args.get('question').strip()
    print(question)
    print('the questions received', question)
    result = wolfram_and_google.get_result_from_wolfram(question)
    socketio.emit('coordinate_agent', 'Obtained result from the Wolfram alpha engine')

    pprint(result)
    return json.dumps(result)


@app.route('/query_google')
def make_query_google():
    socketio.emit('coordinate_agent', 'Querying the Google engine')
    question = request.args.get('question').strip()
    # r = wolfram_and_google.get_result_from_google_directly(question)
    r = google_api.run(question)
    print('========== the result from google ========\n')
    print(r)
    socketio.emit('coordinate_agent', 'Obtained result from the Google engine')
    return str(r)


@app.route('/')
def hello_world():
    return render_template('index_dln22.html')

print('----------- sys path ------------')
print(sys.path)
from CoordinateAgent import CoordinateAgent
from wolfram_alpha_and_google.WolframGoogle import WolframGoogle
from wolfram_alpha_and_google.GoogleAPI import GoogleAPI

coordinate_agent = CoordinateAgent(socketio)
wolfram_and_google = WolframGoogle()
google_api = GoogleAPI()

if __name__ == '__main__':
    app.run(host='https://kg.cmclinnovations.com/', port=8080, debug=True)
    # socketio.run(app)