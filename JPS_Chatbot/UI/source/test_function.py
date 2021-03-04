try:
    from __main__ import socketio
except ImportError:
    from run import socketio


from flask import session
from flask_socketio import emit, join_room, leave_room

socketio.emit('coordinate_agent', 'Hi from test function')

# @socketio.on('message')
# def test(data):
#     print('----------------------------------')
#     print('Here we go with a thing')
#     print(data)
#
#     print('----------------------------------')