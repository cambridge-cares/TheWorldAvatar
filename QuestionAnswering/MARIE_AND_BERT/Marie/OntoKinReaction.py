import sys

sys.path.append("")

from Marie.Util.CommonTools.Parsing import Parsing
from Marie.Util.LDFTools.LdfRequest import LdfRequest


class OntoKinReactionInterface():

    def __init__(self):
        self.my_parser = Parsing()

    def run(self, question, head=None, mention=None):
        t = self.my_parser.token_and_tag(question)
        reactants, products = self.my_parser.parsing(t)
        request = LdfRequest(reactants, products)
        equations = request.get_equations()

        return [equations], [1], ["reactions"]


if __name__ == "__main__":
    my_ontokin = OntoKinReactionInterface()
    rst = my_ontokin.run("Reactions with H2 + O2")
    print(rst)
