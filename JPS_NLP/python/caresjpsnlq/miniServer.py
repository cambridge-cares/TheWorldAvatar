from flask import Flask, request, current_app
import time
import json


from NLQ_Chunker import NLQ_Chunker
app = Flask(__name__)
app.config.update(TEMPLATES_AUTO_RELOAD= True)
chunker = NLQ_Chunker()


@app.route('/jps-nlq/<sentence>')
def query_a_sentence(sentence='The answer is 42'):
    start = time.time()
    query_result = chunker.chunk_a_sentence(sentence)
    print('took:', time.time() - start, 'seconds')


    return(json.dumps(query_result))
    # return '42'

@app.route('/main-query/')
def show():
    return current_app.send_static_file('index.html')


