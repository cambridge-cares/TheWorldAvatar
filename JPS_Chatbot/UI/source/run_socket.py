import json

from flask import Flask, request
from flask import render_template
from flask_cors import CORS
from flask_socketio import SocketIO, send, emit
import sys, os
sys.path.insert(0, os.path.realpath(os.path.dirname(__file__)))
sys.path.append('/source')



app = Flask(__name__)
CORS(app)
socketio = SocketIO(app)

# @socketio.on('message')
# def handle_message(message):
#     #send('Hello, this is a message')
#     print('username:', message['username'])
#     username = message['username']
#     print('received message: ' + message['data'])
#     rsp = {'data': 'Hello, this is the data', 'username': username}
#     emit('response', json.dumps(rsp))


@app.route('/')
def hello_world():
    return render_template('index_socket_test.html')



@app.route('/query')
def query():
    try:
        question = request.args.get('question')
        print(question)
        print('the questions received', question)
        result = ca.run(question)
        print(result)
        return json.dumps(result)
    except:
        return 'Nothing'

from CoordinateAgent import CoordinateAgent
ca = CoordinateAgent()

import test_function

if __name__ == '__main__':
    app.run(host='127.0.0.1', port=5000, debug=True)
    socketio.run(app)