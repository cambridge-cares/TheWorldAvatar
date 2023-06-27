#  move all dictionary duties from ChemicalNEL to IRILookup
import json
import os
import re

import chemparse
import fuzzyset

from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.Util.CommonTools import NumericalTools
from Marie.Util.CommonTools.NLPTools import NLPTools
from Marie.Util.Logging import MarieLogger
from Marie.Util.location import DICTIONARY_DIR


def rearrange_formula(mention):
    formula_regex = r"([A-Z]+[0-9]+)+"
    if re.fullmatch(formula_regex, mention):
        # the thing is a formula, rearrange it
        rst = ''.join([str(k) + '_' + str(int(v)) + '_' for k, v in sorted(chemparse.parse_formula(mention).items())])
        rst = rst.replace('_1_', '').replace('_', '')
        return rst
    else:
        return mention.strip()


class IRILookup:

    def __init__(self, dataset_name=None, enable_class_ner=False, nel=None):
        self.nel = nel
        self.nlp_tool = NLPTools()
        self.marie_logger = MarieLogger()
        self.enable_class_ner = enable_class_ner
        if enable_class_ner and (dataset_name is not None):
            print("DATASET NAME", dataset_name)
            print(f"Loading dictionaries from {os.path.join(DICTIONARY_DIR, dataset_name)}")
            self.name_list = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json')).read())
            self.name_dict = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_dict.json')).read())
            self.type_dict = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'type_dict.json')).read())
            self.fuzzyset = fuzzyset.FuzzySet(self.name_list)

        else:
            try:
                print(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json'))
                self.name_list = json.loads(
                    open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json')).read())
                self.name_dict = json.loads(
                    open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_dict.json')).read())
                self.marie_logger.info("5. Done loading the dictionaries for entity linking")
                self.fuzzyset = fuzzyset.FuzzySet(self.name_list)

            except:
                loadpath = os.path.join(DICTIONARY_DIR,
                                        dataset_name) if dataset_name is not None else 'dataset is not specified'
                self.marie_logger.critical(
                    f"Failed at loading dictionaries for entity linking from {loadpath}.__init__")

    def get_mention_type(self, mention):
        for type_key, label_list in self.type_dict.items():
            if mention in label_list:
                return type_key

    def ner_with_class(self, question):
        # List the Chemical Building Units with 2-linear as the Generic Building Unit
        dummy = [{'text': question}]
        out = self.nel.translator.extract_ner(dummy)
        mention_list = out[0][0]
        class_list = out[1][0]
        target = None
        instance_list = None
        instance_label = None
        target_candidates = []
        mention_types = []
        for original_mention, mention_class in zip(mention_list, class_list):
            fuzzy_search_result = self.fuzzyset.get(original_mention)
            if fuzzy_search_result:
                mention = fuzzy_search_result[0][1]
                mention_score = fuzzy_search_result[0][0]
                if mention_score > 0.7:
                    mention_type = self.get_mention_type(mention)
                    label_result = self.name_dict[mention_type][mention]
                    if mention_type == "class":
                        target_candidates.append(label_result)
                    else:
                        mention_types.append(mention_type)
                        if type(label_result) == type(""):
                            instance_list = [label_result]
                        else:
                            instance_list = list(set(label_result))
                        instance_label = original_mention
        target_candidates = [target for target in target_candidates if target not in mention_types]
        if len(target_candidates) > 0:
            target = target_candidates[0]
        return target, instance_list, instance_label

    def get_mention_without_class(self, question):
        question = question.replace("'s", " of ")
        results, smiles_string = self.nel.ner.find_cid(question)  # [2]
        # smiles_string = results[2]
        if smiles_string is None:
            smiles_string = results[2]
            if smiles_string == "species":
                return "species", [], []
        # print("from get mention without class", smiles_string)
        return smiles_string

    def get_mention(self, question):
        question = question.replace("lennard jones well depth", "molecular weight")
        question = question.replace("chemical species", "species")
        question = question.replace("more than", "over")
        question = self.nlp_tool.filter_stop_words_for_nel(question)
        if self.enable_class_ner:
            if "find" not in question:
                question = f"find all {question}"
            else:
                question = question.replace("find all", "find").replace("find", "find all")

        question = question.replace("degrees", "")
        # remove numbers from the question
        numerical_value, numerical_string = NumericalTools.numerical_value_extractor(
            question=question)
        if numerical_value is not None:
            question = question.replace(numerical_string, "")
        print(f"Processed question string is: {question}")
        instance_list = None
        target = None
        instance_label = None
        if self.enable_class_ner:
            target, instance_list, instance_label = self.ner_with_class(question)
        if (target is None) and (instance_list is None):
            return self.get_mention_without_class(question)
        else:
            return target, instance_list, instance_label

    def parse_agent_questions(self, question, agent_name):
        # what is the heat capacity of benzene at temperature of 100K
        pass

    def find_cid(self, mention):
        try:
            mention_str = mention
            rearranged_mention_str = rearrange_formula(mention_str)
            confidence, key = self.fuzzy_search(rearranged_mention_str)[0]
            if key not in self.name_dict:
                return confidence, self.name_dict["species"][key], str(mention_str), key
            else:
                return confidence, self.name_dict[key], str(mention_str), key

        except (IndexError, TypeError):
            return None, None, None, None

    def cid_lookup(self, mention):
        return self.name_dict(mention)

    def fuzzy_search(self, mention):
        return self.fuzzyset.get(mention)


if __name__ == "__main__":
    text = "List the Chemical Building Units with 2-linear as the Generic Building Unit"
    cn = ChemicalNEL()
    # cn = ChemicalNEL(dataset_name="wikidata_numerical", enable_class_ner=True)
    iri_lookup = IRILookup(dataset_name="OntoMoPs", enable_class_ner=True, nel=cn)

    while text != "quit":
        text = input("Question: ")
        mention = iri_lookup.get_mention(text)
        print("============== MENTION ===============")
        print("mention:", mention)
        print("======================================")

    # iri_lookup = IRILookup(dataset_name="ontocompchem", enable_class_ner=False, nel=cn)
    # iri_lookup.get_mention("melting point of benzene")
    #
    # iri_lookup = IRILookup(dataset_name="OntoMoPs", enable_class_ner=True, nel=cn)
    # iri_lookup.get_mention("melting point of benzene")
    #
    # iri_lookup = IRILookup(dataset_name="ontospecies_new", enable_class_ner=True, nel=cn)
    # iri_lookup.get_mention("melting point of benzene")
