class Calculator:

    def __init__(self, socket):
        self.socket = socket

    def run(self):
        self.socket.emit('response', 'Here is your result')

        

