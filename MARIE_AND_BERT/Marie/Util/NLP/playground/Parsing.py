import nltk
from nltk.corpus import stopwords

stop_words = set(stopwords.words('english'))
stop_words.remove("o")
stop_words.add("show")

import nltk.tag, nltk.data

tagger_path = r'taggers/maxent_treebank_pos_tagger/english.pickle'
print(tagger_path)
default_tagger = nltk.data.load(tagger_path)
model = {'reaction': 'TARGET', 'mechanism': 'TARGET', 'product': 'REL', 'reactant': 'REL',
         'produce': 'PRD', 'produces': 'PRD', 'ADD': 'ADD', 'reaction_rate': 'REL', 'reversible': 'REL',
         'contains': 'REL'}
tagger = nltk.tag.UnigramTagger(model=model, backoff=default_tagger)
reaction_pattern = """NP: {<NNP><NN>|<NN><NNP>|<NN>*<NNP>*}
    PRD: {<PRD>}
    CLASS: {<WP><NP>|<TARGET>}
    REACTION: {<NP><ADD><NP>|<NP>}
    REACTION_PART: {<REAC_C><PRD><REAC_C>}
    REL: {<REL>}
    IN: {<IN>}"""

numerical_pattern = """CLASS: {}
"""


def token_and_tag(text):
    # replace "+" and "=="
    text = text.replace("Reaction rate ", "reaction_rate ")
    text = text.replace("reaction rate ", "reaction_rate ")
    text = text.replace(" and ", " ADD ")
    text = text.replace(" + ", " ADD ")
    text = text.replace(" == ", " produce ")
    text = text.replace(" -> ", " produce ")

    text = text.split()
    text = [w for w in text if not w.lower() in stop_words]
    tokens_tag = tagger.tag(text)
    return tokens_tag


def parsing(pattern, sentence):
    NPChunker = nltk.RegexpParser(pattern)
    result = NPChunker.parse(sentence)
    print(result)
    result.draw()


numerical_questions = """Find all aromatic hydrocarbons with molecular weight more than 100 
"""

reaction_questions = """What reaction produces H2 + OH
Show me the reaction rate of H2 + O2 == H2O2
Is the reaction H + H2O == H2 + OH reversible
Is the reaction H2 + O2 == H2O reversible
What reaction has CH4 as reactant
What mechanism contains CH4 + OH
What reaction has CH4 + H2 as product
What mechanism contains CH4 + OH == H2 + H2
Reaction rate of H + O2 -> O + OH"""
for sentence in reaction_questions.split('\n'):
    t = token_and_tag(sentence)
    parsing(reaction_pattern, t)
