from flask import Flask, request
from flask import render_template


from pprint import pprint

from chatbot.chatbot_interface import Chatbot
from chatbot.search_interface import SearchInterface

bot = Chatbot()
# what is the pka constant of Benzene
# find all reactions with water as a reactant
# show me the melting point of all the metals
# show me the melting point of all the chemical species
# Species with an molecular weight larger than 200
# what are the chemical structures of all alkane

app = Flask(__name__)
@app.route('/')
def hello_world():
    return render_template('index_dln22.html')

@app.route('/test')
def go():
    question = request.args.get('question')
    print('the questions received', question)
    return bot.analyse_questions(question)
    # return bot.analyse_questions('Species with an molecular weight larger than 200')

if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8080, debug=True)