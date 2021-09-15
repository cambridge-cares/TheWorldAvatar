from pprint import pprint

from chatbot_interface import Chatbot
from search_engine.search_interface import SearchInterface

bot = Chatbot()
# what is the pka constant of Benzene
# find all reactions with water as a reactant
# show me the melting point of all the metals
# show me the melting point of all the chemical species
# Species with an molecular weight larger than 200
# what are the chemical structures of all alkane
question = ''
while question != 'quit':
    question = input('what is your question? ')
    result = bot.analyse_questions(question)
    print('--------------------- query result ------------------------')
    pprint(result)
