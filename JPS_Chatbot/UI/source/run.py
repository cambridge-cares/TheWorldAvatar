import json
import sys, os
sys.path.insert(0, os.path.realpath(os.path.dirname(__file__)))

from pprint import pprint

from flask import Flask, request
from flask import render_template
from flask_socketio import SocketIO, send, emit


sys.path.append('/source')
app = Flask(__name__)
socketio = SocketIO(app)


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
    question = request.args.get('question')
    print(question)
    print('the questions received', question)
    result = wolfram_and_google.get_result_from_wolfram(question)
    socketio.emit('coordinate_agent', 'Obtained result from the Wolfram alpha engine')

    pprint(result)
    return json.dumps(result)


@app.route('/query_google')
def make_query_google():
    socketio.emit('coordinate_agent', 'Querying the Google engine')
    question = request.args.get('question')
    r = wolfram_and_google.get_result_from_google_directly(question)
    socketio.emit('coordinate_agent', 'Obtained result from the Google engine')
    return r


@app.route('/')
def hello_world():
    return render_template('index_dln22.html')


from CoordinateAgent import CoordinateAgent
from wolfram_alpha_and_google.WolframGoogle import WolframGoogle

coordinate_agent = CoordinateAgent()
wolfram_and_google = WolframGoogle()


if __name__ == '__main__':
    app.run(host='https://kg.cmclinnovations.com/', port=8080, debug=True)
    socketio.run(app)