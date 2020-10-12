# The purpose of this class is to test the
#  performance of flask-based sockets in
#  apache + gunicorn environment.
#  also, this mini-project serves as a template for future flask projects

from flask import Flask, request, abort, jsonify
import json
from flask import render_template
from flask_socketio import SocketIO, send, emit
import sys, os
from .chatbot.calculate  import Calculator


sys.path.insert(0, os.path.realpath(os.path.dirname(__file__)))
sys.path.append('/source')


app = Flask(__name__)
socketio = SocketIO(app)

@app.route('/index')
def render_index():
    return render_template('index_socket_test.html')


@socketio.on('message')
def handle_message(message):
    print('message:', message['message'])
    cal.run()
    emit('response', json.dumps({'response': 'Hello'}))

# def get_square():
#     if not request.json or 'number' not in request.json:
#         abort(400)
#     num = request.json['number']
#
#     return jsonify({'answer': num ** 2})

cal = Calculator(socketio)


if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8080, debug=True)
    socketio.run(app)