from Marie.Util.Logging import MarieLogger
# ==============================================================================================================

import json
import time
import os
# from chemdataextractor import Document
from Marie.Util.location import DICTIONARY_DIR
import fuzzyset
import chemparse
import re
from Marie.EntityLinking.Inference import BertNEL


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
            self.fuzzyset = fuzzyset.FuzzySet(self.name_list)
        except:
            loadpath = os.path.join(DICTIONARY_DIR, dataset_name) if dataset_name is not none else 'dataset is not specified'
            self.marie_logger.critical(f"Failed at loading dictionaries for entity linking from {loadpath}.__init__")

        self.ner = BertNEL()

    def get_mention(self, question):
        # _, mention_string = self.ner.find_cid(question)
        # return mention_string
        question = question.replace("'s", " of ")
        results, smiles_string = self.ner.find_cid(question)  # [2]
        if smiles_string is None:
            smiles_string = results[2]
        print("results:", results)
        print("smiles_string:", smiles_string)
        return smiles_string
        # except:
        #     self.marie_logger.error(
        #         f"Could not recognise any target from the question: {question} from {__name__}.{self.find_cid.__name__}")

    def find_cid(self, mention):
        try:
            mention_str = mention
            # TODO: rearrange the chemical formula
            rearranged_mention_str = rearrange_formula(mention_str)
            print(rearranged_mention_str)
            confidence, key = self.fuzzy_search(rearranged_mention_str)[0]
            print(f"the key is {key}")
            return confidence, self.name_dict[key], str(mention_str), key

        except (IndexError, TypeError):
            return None, None, None, None

    def cid_lookup(self, mention):
        return self.name_dict(mention)

    def fuzzy_search(self, mention):
        return self.fuzzyset.get(mention)


if __name__ == '__main__':
    cn = ChemicalNEL(dataset_name="wikidata_numerical")
    START_TIME = time.time()

    print("====================================================")
    mention = cn.get_mention("what is the mass of [I-].[I-].[I-].[I-].[Po]")
    rst = cn.find_cid(mention=mention)
    print(rst)
    print("====================================================")

    mention = cn.get_mention("what is the mass of C6H6")
    rst = cn.find_cid(mention=mention)
    print(rst)
    print("====================================================")

    mention = cn.get_mention("what is C6H6's smiles")
    rst = cn.find_cid(mention=mention)
    print(rst)
    print("====================================================")

    mention = cn.get_mention("what is CC[CH2]'s inchi")
    rst = cn.find_cid(mention=mention)
    print(rst)
    print("====================================================")

    mention = cn.get_mention("what is the molecular weight of methane")
    rst = cn.find_cid(mention=mention)
    print(rst)
    print("====================================================")

    print(time.time() - START_TIME)
