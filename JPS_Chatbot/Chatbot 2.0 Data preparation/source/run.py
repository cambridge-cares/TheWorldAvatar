import json
import sys
from pprint import pprint

from flask import Flask, request

sys.path.append('/source')
from CoordinateAgent import CoordinateAgent

coordinate_agent = CoordinateAgent()

app = Flask(__name__)


@app.route('/query')
def make_query():
    question = request.args.get('question')
    print(question)
    print('the questions received', question)
    result = coordinate_agent.run(question)
    pprint(result)
    return json.dumps(result)


if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8080, debug=True)
