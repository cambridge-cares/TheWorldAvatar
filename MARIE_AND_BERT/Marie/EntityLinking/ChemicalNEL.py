from Marie.Util.Logging import MarieLogger
# ==============================================================================================================

import json
import time
import os
from chemdataextractor import Document
from Marie.Util.location import DICTIONARY_DIR
import fuzzyset
import chemparse
import re


def rearrange_formula(mention):
    formula_regex = r"([A-Z]+[0-9]+)+"
    if re.fullmatch(formula_regex, mention):
        # the thing is a formula, rearrange it
        rst = ''.join([str(k) + '_' + str(int(v)) + '_' for k, v in sorted(chemparse.parse_formula(mention).items())])
        rst = rst.replace('_1_', '').replace('_', '')
        return rst
    else:
        return mention.strip()


class ChemicalNEL:
    """

    """

    def __init__(self, dataset_name=None):
        self.marie_logger = MarieLogger()
        try:
            print(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json'))
            self.name_list = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json')).read())
            self.name_dict = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_dict.json')).read())
            self.marie_logger.info("5. Done loading the dictionaries for entity linking")
        except:
            self.marie_logger.critical(f"Failed at loading dictionaries for entity linking from {__name__}.__init__")

        self.fuzzyset = fuzzyset.FuzzySet(self.name_list)

    def find_cid(self, question):
        q_cap = question.upper()
        doc = Document(q_cap)
        mentions = doc.cems
        self.marie_logger.info(f"mentions: {mentions} in question {question}")
        try:
            mention_str = str(mentions[0])
            # TODO: rearrange the chemical formula
            rearranged_mention_str = rearrange_formula(mention_str)
            print(rearranged_mention_str)
            confidence, key = self.fuzzy_search(rearranged_mention_str)[0]
            print(f"the key is {key}")
            return confidence, self.name_dict[key], str(mention_str), key

        except (IndexError,TypeError) :
            self.marie_logger.error(
                f"Could not recognise any target from the question: {question} from {__name__}.{self.find_cid.__name__}")
            return None

    def cid_lookup(self, mention):
        return self.name_dict(mention)

    def fuzzy_search(self, mention):
        return self.fuzzyset.get(mention)


if __name__ == '__main__':
    cn = ChemicalNEL(dataset_name="pubchem")
    START_TIME = time.time()
    rst = cn.find_cid('what is the molar mass of benzene')
    print(rst)

    print(time.time() - START_TIME)
    # print(chemparse.parse_formula('H4(C1)CCC'))
    # print(chemparse.parse_formula('H4C1'))
    # print(chemparse.parse_formula('H4C'))
