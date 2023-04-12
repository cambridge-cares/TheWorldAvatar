import sys
sys.path.append("")

from Marie.Util.CommonTools.Parsing import Parsing
from Marie.Util.LDFTools.LdfRequest import LdfRequest

class OntoKinReactionInterface():

    def __init__(self):
        pass

    def run(self, question, head=None, mention=None):
        
        my_parser = Parsing(question)
    
        t = my_parser.token_and_tag(question)
        reactants, products = my_parser.parsing(t)
        request = LdfRequest(reactants, products)
        equations = request.get_equations()

        return equations

if __name__ == "__main__":
    my_ontokin = OntoKinReactionInterface()
    rst = my_ontokin.run("Reactions with H2 + O2")
    print(rst)

