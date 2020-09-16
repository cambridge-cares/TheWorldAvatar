import json
import sys
from pprint import pprint

from flask import Flask, request
from flask import render_template

sys.path.append('/source')
from CoordinateAgent import CoordinateAgent
from wolfram_alpha_and_google.WolframGoogle import WolframGoogle

coordinate_agent = CoordinateAgent()
wolfram_and_google = WolframGoogle()

app = Flask(__name__)


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
    question = request.args.get('question')
    print(question)
    print('the questions received', question)
    result = wolfram_and_google.get_result_from_wolfram(question)
    pprint(result)
    return json.dumps(result)


# TODO: plug in the google api, visualize it
# TODO: make sure the visualization match
# TODO: then we call it a day ... (Sunday, )
# TODO: If you feel really good about yourself, lets plugin the part B of the system
# TODO: If you are doing sooo great today, lets prepare the stuff for deployment...
@app.route('/query_google')
def make_query_google():
    question = request.args.get('question')
    r = wolfram_and_google.get_result_from_google_directly(question)
    return r


@app.route('/')
def hello_world():
    return render_template('index_dln22.html')


if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8080, debug=True)
