import json
import logging
import time
import os
from chemdataextractor import Document
from Marie.Util.location import DICTIONARY_DIR
import fuzzyset
import chemparse
import re
logging.basicConfig(level=logging.DEBUG, filename='marie.log', format='%(asctime)s %(levelname)s:%(message)s')


def rearrange_formula(mention):
    formula_regex = r'[A-Z0-9\(\)+]+'
    if re.fullmatch(formula_regex, mention):
        # the thing is a formula, rearrange it
        rst = ''.join([str(k) + '_' + str(int(v)) + '_' for k, v in sorted(chemparse.parse_formula(mention).items())])
        rst = rst.replace('_1_', '').replace('_', '')
        print(rst)
        return rst
    else:
        return mention.strip()


class ChemicalNEL:
    def __init__(self):
        self.name_list = json.loads(open(os.path.join(DICTIONARY_DIR, 'name_list.json')).read())
        self.cid_dict = json.loads(open(os.path.join(DICTIONARY_DIR, 'cid_dict.json')).read())
        self.fuzzyset = fuzzyset.FuzzySet(self.name_list)

    def find_cid(self, question):
        doc = Document(question)
        mentions = doc.cems
        print("mentions", mentions)
        try:
            mention_str = str(mentions[0])
            # TODO: rearrange the chemical formula
            rearranged_mention_str = rearrange_formula(mention_str)
            confidence, key = self.fuzzy_search(rearranged_mention_str)[0]
            return confidence, self.cid_dict[key], str(mention_str), key

        except IndexError:
            logging.error(f"Error - Could not recognise any target from the question: {question}")
            return None

    def cid_lookup(self, mention):
        return self.cid_dict(mention)

    def fuzzy_search(self, mention):
        return self.fuzzyset.get(mention)


if __name__ == '__main__':
    cn = ChemicalNEL()
    START_TIME = time.time()
    rst = cn.find_cid('what is the molar mass of CH2H2')
    print(rst)
    rst = cn.find_cid('what is the molar mass of H4C')
    print(rst)
    rst = cn.find_cid('what is the molar mass of C1=CC=CC=C1')
    print(rst)

    print(time.time() - START_TIME)
    # print(chemparse.parse_formula('H4(C1)CCC'))
    # print(chemparse.parse_formula('H4C1'))
    # print(chemparse.parse_formula('H4C'))
