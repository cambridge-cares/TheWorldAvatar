from Marie.CrossGraphQAEngine import CrossGraphQAEngine
import json
from flask import Flask, request
from flask import render_template, send_from_directory
from Marie.Util.Logging import MarieLogger

logger = MarieLogger()
logger.info("============= Initializing the server ===========")
logger.info("1. Initializing Cross Graph Engine")
my_qa_engine = CrossGraphQAEngine()
logger.info(" - Done initializing Cross Graph Engine")
logger.info("============= Server is ready to go! ===========")
app = Flask(__name__)


def result_filter(result):
    if "ontoagent" in str(result):
        return result

    if type(result) == type([]):
        sample_node = result[0]
        if "domain" in sample_node:
            domain = sample_node["domain"]
            if domain == "ontokin_reaction":
                result_tmp = []
                for row in result[0]["node"]:
                    result_tmp.append({"node": row})
                return result_tmp

    result_tmp = []

    if "score" in str(result):
        for row in result:
            row_tmp = {}
            for row_key in row:
                if row_key.lower() != "score":
                    row_tmp[row_key] = row[row_key]
            result_tmp.append(row_tmp)
        # this is important, this only keeps the core information about a single element answer
        return {"single": result_tmp[0]}
        # return [result_tmp[0]]
    else:
        return result


def answer_question(question):
    logger.info("=======================================================================================")
    logger.info(f" The server received a question: {question}")
    answer = my_qa_engine.run(input_question=str(question).strip(), disable_alignment=False,
                              heads={})
    logger.info(f" The server returned answers: {json.dumps(answer)}")
    logger.info("=======================================================================================")
    return answer


@app.route('/')
def hello_world():
    return render_template('index_new.html')


@app.route('/ontospecies')
def ontospecies():
    return render_template('ontospecies.html')


@app.route('/static/<path:path>')
def send_js(path):
    return send_from_directory('static', path)


@app.route("/search", methods=['GET'])
def search():
    args = request.args
    question = args["question"]
    answer = answer_question(question)
    return json.dumps(result_filter(answer))


if __name__ == "__main__":
    # Default setup for deployment
    app.run(host='0.0.0.0', debug=False, port=5003, threaded=False, processes=1)
    # Only for debugging while developing, uncomment the next line and comment the aboveline
    # app.run(host='0.0.0.0', debug=True, port=5003)
