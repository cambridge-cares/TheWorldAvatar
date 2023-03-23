import os

import nltk
from nltk import Tree
from nltk.corpus import stopwords
from nltk.draw import TreeWidget
from nltk.draw.util import CanvasFrame
from nltk.stem import *

# stop_words = set(stopwords.words('english'))
stop_words = ["what", "is", "the", "list", "of", "are", "with", "a", "one", "does", "do", "species", "after", ","]
# stop_words.remove("o")
stop_words.append("show")
stemmer = PorterStemmer()
from PIL import Image
import nltk.tag, nltk.data

tagger_path = r'taggers/maxent_treebank_pos_tagger/english.pickle'
print(tagger_path)
default_tagger = nltk.data.load(tagger_path)
model = {'reaction': 'TARGET', 'mechanism': 'TARGET', 'products': 'PRODUCT_INDICATOR',
         'product': 'PRODUCT_INDICATOR', 'reactants': 'REACTANT_INDICATOR', 'reactant': 'REACTANT_INDICATOR',
         'form': 'PRD', 'forms': 'PRD', 'formed': 'PRD', 'give': 'PRD', 'gives': 'PRD',
         'produce': 'PRD', 'produces': 'PRD', 'ADD': 'ADD', 'reaction_rate': 'REL', 'reversible': 'REL',
         'contains': 'REL', 'react': 'ADD', 'combin': 'ADD', 'compris':'PRD', 'produc': 'PRD'}
tagger = nltk.tag.UnigramTagger(model=model, backoff=default_tagger)
# reaction_pattern = """NP: {<NNP><NN>|<NN><NNP>|<NN>*<NNP>*}
#     PRD: {<PRD>}
#     CLASS: {<WP><NP>|<TARGET>}
#     REACTION: {<NP><ADD><NP>|<NP>}
#     REACTION_PART: {<REAC_C><PRD><REAC_C>}
#     REL: {<REL>}
#     IN: {<IN>}"""


reaction_pattern = """
SPECIES: {<NNP>}
SPECIES_SET: {<SPECIES><ADD><SPECIES>|<SPECIES>}
AS_INDICATOR: {<SPECIES_SET><IN>|<SPECIES><IN>}
ALL_PRODUCTS: {<AS_INDICATOR><PRODUCT_INDICATOR>|<PRD><SPECIES_SET>|<SPECIES_SET><PRD>}
ALL_REACTANTS: {<AS_INDICATOR><REACTANT_INDICATOR>|<SPECIES_SET>}
"""



def token_and_tag(text):
    # replace "+" and "=="
    text = text.replace(",", "")
    text = text.replace("Reaction rate ", "reaction rate ")
    text = text.replace("reaction rate ", "reaction rate ")
    text = text.replace(" and ", " ADD ")
    text = text.replace(" + ", " ADD ")
    text = text.replace(" == ", " produce ")
    text = text.replace(" -> ", " produce ")
    text = text.replace("reactants", "reactant")
    print("question after", text)
    text = text.split()
    text = [w for w in text if not w.lower() in stop_words]
    filtered_text = []
    for plural in text:
        if plural.isupper():
            filtered_text.append(plural)
        else:
            filtered_text.append(stemmer.stem(plural))
    tokens_tag = tagger.tag(filtered_text)
    print(tokens_tag)
    return tokens_tag


def parsing(pattern, sentence, original_question, index):
    NPChunker = nltk.RegexpParser(pattern)
    # NPChunker = nltk.RegexpParser(pattern)
    result = NPChunker.parse(sentence)
    result.set_label(original_question.replace(" ", "_"))
    cf = CanvasFrame(bg="blue", height=100, width=100)
    t = Tree.fromstring(str(result))
    tc = TreeWidget(cf.canvas(), t)

    tc['node_font'] = 'arial 10 bold'
    tc['leaf_font'] = 'arial 10'
    tc['node_color'] = '#005990'
    tc['leaf_color'] = '#3F8F57'
    tc['line_color'] = '#175252'
    cf.add_widget(tc, 0, 0)  # (10,10) offsets
    cf.print_to_file(f'{index}.ps')

    # ps_file = open("tree.ps", "rb")
    # image_path = os.path.abspath('tree.ps')
    # print("image path", image_path)
    # img = Image.open(image_path)
    # img.save("img.png")
    cf.destroy()
    # print(result)
    # result.draw()


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
breaking_questions = """
What is the result of a reaction between C and O2
What are the reactants of a reaction with H2 and OH as products
What are the products of a reaction with H2 and OH as reactants
With H2 and O2 as the reactants, what are the products
With H2 and O2 as the products, what are the reactants
What does H and O2 react to produce
What does a reaction between H and O2 result in
What does H and O2 react to produce
What is the result of a reaction between C and O2
How does CO2 and H2O react
What does the combination of H2 and OH give
Which species combine to give H2 and OH
List all reactions with H2 and OH as the product
In which reactions does H2 and O2 combine to give OH as one of the products
Which reactions comprise of H2 and OH
Which reactions give OH after combining O2 and H2
What is the rate of a reaction which combines CH4 and OH to give H2 and H2
What species are involved in the reaction between H2 and OH
Which elements are combined to give H2 and OH
Which compounds react together to form H2O
When H2 reacts with OH, what is the final outcome of the reaction
When H2 is combined with O2, what is the end product
What species are mixed together to produce H2 and OH
What occurs when H2 and O2 are mixed
When H2 and OH are formed, what are the initial components involved
When C is added to H2O, what is the ultimate result
In the reaction that gives H2 and OH, what are the substances that react
Which components undergo change to produce H2 and OH
What does a combination of H2 and OH produce"""

# breaking_questions = """When H2 and OH are formed, what are the initial components involved"""
breaking_questions = breaking_questions +  reaction_questions
counter = 0
for sentence in breaking_questions.split('\n'):
    print(sentence)
    counter += 1
    t = token_and_tag(sentence)
    parsing(reaction_pattern, t, sentence, str(counter))
    print("================================================")
