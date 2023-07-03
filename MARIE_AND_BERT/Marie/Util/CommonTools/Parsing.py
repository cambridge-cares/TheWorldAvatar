import os
import sys
sys.path.append("")
from nltk.stem import *
import nltk.tag, nltk.data
import nltk
class Parsing():
    def __init__(self):
        # self.question = question
        # stop_words = set(stopwords.words('english'))
        self.stop_words = ["what", "is", "the", "list", "of", "are", "with", "a", "one", "does", "do", "species", "after", ",", "show"]
        self.stemmer = PorterStemmer()
        nltk.download('maxent_treebank_pos_tagger')
        tagger_path = r'taggers/maxent_treebank_pos_tagger/english.pickle'
        default_tagger = nltk.data.load(tagger_path)


        model = {'reaction': 'TARGET', 'mechanism': 'TARGET', 'products': 'PRODUCT_INDICATOR',
                'product': 'PRODUCT_INDICATOR', 'reactants': 'REACTANT_INDICATOR', 'reactant': 'REACTANT_INDICATOR',
                'form': 'PRD', 'forms': 'PRD', 'formed': 'PRD', 'give': 'PRD', 'gives': 'PRD',
                'produce': 'PRD', 'produces': 'PRD', 'ADD': 'ADD', 'reaction_rate': 'REL', 'reversible': 'REL',
                'contains': 'REL', 'react': 'ADD', 'combin': 'ADD', 'compris':'PRD', 'produc': 'PRD', 'split': 'SPLIT'}
        self.tagger = nltk.tag.UnigramTagger(model=model, backoff=default_tagger)


        self.reaction_pattern = """
        SPECIES: {<NNP>}
        SPECIES_SET: {<SPECIES><ADD><SPECIES>|<SPECIES>}
        AS_INDICATOR: {<SPECIES_SET><IN>|<SPECIES><IN>}
        ALL_PRODUCTS: {<AS_INDICATOR><PRODUCT_INDICATOR>|<PRD><SPECIES_SET>|<SPECIES_SET><PRD>|<SPLIT><SPECIES_SET>}
        ALL_REACTANTS: {<AS_INDICATOR><REACTANT_INDICATOR>|<SPECIES_SET>}
        """



    def token_and_tag(self, text):
        # replace "+" and "=="
        text = text.replace(",", "")
        text = text.replace("Reaction rate ", "reaction rate ")
        text = text.replace("reaction rate ", "reaction rate ")
        text = text.replace(" and ", " ADD ")
        text = text.replace(" + ", " ADD ")
        text = text.replace(" == ", " split ")
        text = text.replace(" -> ", " split ")
        text = text.replace("reactants", "reactant")
        # print("question after", text)
        text = text.split()
        text = [w for w in text if not w.lower() in self.stop_words]
        filtered_text = []
        for plural in text:
            if plural.isupper():
                filtered_text.append(plural)
            else:
                filtered_text.append(self.stemmer.stem(plural))
        tokens_tag = self.tagger.tag(filtered_text)
        # print(tokens_tag)
        return tokens_tag


    def parsing(self, sentence):
        NPChunker = nltk.RegexpParser(self.reaction_pattern)
        # NPChunker = nltk.RegexpParser(pattern)
        result = NPChunker.parse(sentence)
        # result.set_label(self.question.replace(" ", "_"))

        reactants = []
        products = []
        for subtree in result.subtrees(filter=lambda t: t.label() == "ALL_REACTANTS" or t.label() == "ALL_PRODUCTS"):
            species = []
            for leaf in subtree.leaves():
                if leaf[1] == "NNP":
                    species.append(leaf[0])
            if "ALL_REACTANTS" in subtree.label():
                for sp in species:
                    reactants.append(sp)
            elif "ALL_PRODUCTS" in subtree.label():
                for sp in species:
                    products.append(sp)

        print("Reactants:", reactants)
        print("Products:", products)

        return reactants, products
        
if __name__=="__main__":
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
    Reaction rate of H + O2 -> O + OH
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
    Which species combine to give H2 and OH
    List all reactions with H2 and OH as the product
    In which reactions does H2 and O2 combine to give OH as one of the products
    Which reactions comprise of H2 and OH
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
    """

    breaking_questions = """
    Which reactions give OH after combining O2 and H2
    What does a combination of H2 and OH produce
    What does the combination of H2 and OH give
    """
    # breaking_questions = """When H2 and OH are formed, what are the initial components involved"""
    breaking_questions =  breaking_questions + reaction_questions
    
    for sentence in breaking_questions.split('\n'):
        my_parser = Parsing()
        print(sentence)
        t = my_parser.token_and_tag(sentence)
        my_parser.parsing(t)
        print("================================================")
