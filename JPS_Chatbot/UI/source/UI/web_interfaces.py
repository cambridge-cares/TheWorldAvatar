from flask import Flask, request

from chatbot.chatbot_interface import Chatbot
from chatbot.search_interface import SearchInterface
from source.UI.chatbot.JPS_query_constructor import JPS_query_constructor

bot = Chatbot()
search_interface = SearchInterface()
query_constructor = JPS_query_constructor()
# what is the pka constant of Benzene
# find all reactions with water as a reactant
# show me the melting point of all the metals
# show me the melting point of all the chemical species
# Species with an molecular weight larger than 200
# what are the chemical structures of all alkane

app = Flask(__name__)

 

@app.route('/NER')
def ner(question):
    return bot.tc.classify_topic(question)


@app.route('/OLS')
def ols(question):
    return search_interface.get_topics()


@app.route('/SPARQLConstruction')
def SPARQLConstruction(data):
    return query_constructor.construct_query(data)


@app.route('/Query')
def intent_classification(query):
    return bot.analyse_questions(question)


@app.route('/test')
def go():
    question = request.args.get('question')
    print('the questions received', question)
    return bot.analyse_questions(question)
    # return bot.analyse_questions('Species with an molecular weight larger than 200')
