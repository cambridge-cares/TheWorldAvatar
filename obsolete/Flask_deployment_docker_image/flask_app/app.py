from flask import Flask
server = Flask(__name__)


@server.route('/index')
def static_file():
	return server.send_static_file('index.html')
	
	
@server.route('/sayHello')
def hello():
	return "Hello Flask"

	
if __name__ == '__main__':
	server.run(debug=False)