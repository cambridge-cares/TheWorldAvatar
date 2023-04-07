import sys
sys.path.append("")

from Marie.Util.CommonTools.Parsing import Parsing
from Marie.Util.LDFTools.LdfRequest import LdfRequest

class OntoKinReactionInterface():

    def __init__(self, question):
        self.question = question
    
    def run(self):
        
        my_parser = Parsing(self.question)
    
        t = my_parser.token_and_tag(self.question)
        reactants, products = my_parser.parsing(t)
        request = LdfRequest(reactants, products)
        equations = request.get_equations()

        return equations
