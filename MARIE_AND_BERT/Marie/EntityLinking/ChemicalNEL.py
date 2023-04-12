from Marie.EntityLinking.translator.translate_smile import Translator
from Marie.Util.CommonTools import NumericalTools
from Marie.Util.Logging import MarieLogger
# ==============================================================================================================

import json
import time
import os
# from chemdataextractor import Document
from Marie.Util.location import DICTIONARY_DIR, ENTITY_LINKING_DATA_DIR
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

    def __init__(self, dataset_name=None, enable_class_ner=False):
        self.marie_logger = MarieLogger()
        self.enable_class_ner = enable_class_ner

        if enable_class_ner:
            print("DATASET NAME", dataset_name)
            print(f"Loading dictionaries from {os.path.join(DICTIONARY_DIR, dataset_name)}")
            self.name_list = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json')).read())
            self.name_dict = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_dict.json')).read())
            self.type_dict = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'type_dict.json')).read())
            self.fuzzyset = fuzzyset.FuzzySet(self.name_list)

        else:
            try:
                print(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json'))
                self.name_list = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json')).read())
                self.name_dict = json.loads(open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_dict.json')).read())
                self.marie_logger.info("5. Done loading the dictionaries for entity linking")
                self.fuzzyset = fuzzyset.FuzzySet(self.name_list)
            except:
                loadpath = os.path.join(DICTIONARY_DIR,
                                        dataset_name) if dataset_name is not None else 'dataset is not specified'
                self.marie_logger.critical(
                    f"Failed at loading dictionaries for entity linking from {loadpath}.__init__")

        self.ner = BertNEL()
        self.translator = Translator(
            modelpath=os.path.join(ENTITY_LINKING_DATA_DIR, "SMILES_NER_V10.bin"))

    def get_mention_type(self, mention):
        for type_key, label_list in self.type_dict.items():
            if mention in label_list:
                return type_key

    def ner_with_class(self, question):
        # List the Chemical Building Units with 2-linear as the Generic Building Unit
        dummy = [{'text': question}]
        out = self.translator.extract_ner(dummy)
        print("out", out)
        mention_list = out[0][0]
        class_list = out[1][0]
        # TODO: create the list of all possible thing for Mops class
        # e.g., ['[(C6H3)(C6H4)3(CO2)3]', 'CBU', 'MOPS']
        # search through the class label list and find the best match
        target = None
        instance_list = None
        instance_label = None
        target_candidates = []
        mention_types = []
        print("mention_list", mention_list)
        print("class_list", class_list)
        for original_mention, mention_class in zip(mention_list, class_list):
            print("original mention", original_mention)
            fuzzy_search_result = self.fuzzyset.get(original_mention)
            print("fuzzy_search_result", fuzzy_search_result)
            mention = fuzzy_search_result[0][1]
            mention_score = fuzzy_search_result[0][0]
            print("for mention", mention, "score is", mention_score)
            if mention_score > 0.7:
                mention_type = self.get_mention_type(mention)
                label_result = self.name_dict[mention_type][mention]
                print("mention type", mention_type)
                print("label_result", label_result)
                if mention_type == "class":
                    target_candidates.append(label_result)
                else:
                    mention_types.append(mention_type)
                    if type(label_result) == type(""):
                        instance_list = [label_result]
                    else:
                        instance_list = list(set(label_result))
                    instance_label = original_mention
            # except (IndexError, TypeError):
            #     pass

        target_candidates = [target for target in target_candidates if target not in mention_types]
        if len(target_candidates) > 0:
            target = target_candidates[0]

        # print("target", target)
        # print("instance list", instance_list)
        # print("=================================================")
        return target, instance_list, instance_label

    def get_mention_without_class(self, question):
        question = question.replace("'s", " of ")
        results, smiles_string = self.ner.find_cid(question)  # [2]
        print("results", results)
        print("smiles_string", smiles_string)
        smiles_string = results[2]
        if smiles_string is None:
            smiles_string = results[2]
            if smiles_string == "species":
                return "species", [], []
        # print("from get mention without class", smiles_string)
        return smiles_string

    def get_mention(self, question):

        if "find" not in question:
            question = f"find all {question}"
        else:
            question = question.replace("find", "find all")

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
            #    try:
            target, instance_list, instance_label = self.ner_with_class(question)
            # print("target:", target)
            # print("instances list", instance_list)
            # print("instance label", instance_label)
        # except KeyError:
        #     return self.get_mention_without_class(question)

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
            # TODO: rearrange the chemical formula
            rearranged_mention_str = rearrange_formula(mention_str)
            # print(rearranged_mention_str)
            confidence, key = self.fuzzy_search(rearranged_mention_str)[0]
            # print(f"the key is {key}").
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


if __name__ == '__main__':
    cn = ChemicalNEL(dataset_name="wikidata_numerical", enable_class_ner=True)
    START_TIME = time.time()

    # text = "find species with molecular weight more than"
    question_list = ["show me the melting point of CH4",
                     "what is the smiles string of CH4O4S",
                     "find species with boiling point larger than 10 degrees",
                     "find species with boiling point smaller than 10 degrees",
                     "find species with boiling point around 100 degrees"]
    for text in question_list:
        # text = "find species with molecular weight over 100"
        mention = cn.get_mention(text)
        print("mention", mention)
        print("---------------")

    # text = "what is the molecular weight of CH4"
    # mention = cn.get_mention(text)
    # print("mention", mention)
    # print("---------------")
    #
    # text = "what is the boiling point of benzene"
    # mention = cn.get_mention(text)
    # print("mention", mention)
    # print("---------------")

    # cn = ChemicalNEL(dataset_name="OntoMoPs", enable_class_ner=True)
    # START_TIME = time.time()
    #
    # text = "MoPs with molecular weight more than 100"
    # mention = cn.get_mention(text)
    # print("mention", mention)
    # print("---------------")
    #
    # text = "List the MOPs with (3-pyramidal)8(2-bent)12(Cs) as the assembly model"
    # mention = cn.get_mention(text)
    # print("mention", mention)
    # print("---------------")
    #
    # cn = ChemicalNEL(dataset_name="ontospecies_new", enable_class_ner=True)
    # START_TIME = time.time()
    #
    # text = "find species with molecular weight more than"
    # mention = cn.get_mention(text)
    # print("mention", mention)
    # print("---------------")
    #
    # text = "what is the molecular weight of CH4"
    # mention = cn.get_mention(text)
    # print("mention", mention)
    # print("---------------")
    #
    #
    # text = "what is the boiling point of benzene"
    # mention = cn.get_mention(text)
    # print("mention", mention)
    # print("---------------")

    # text = "List the MOPs with (3-pyramidal)8(2-bent)12(Cs) as the assembly model"
    # while text != "quit":
    #     text = input("Question:")
    #     mention = cn.get_mention(text)
    #     print(mention)
    #     print(type(mention))
    #     print(type(mention) == type(()))

    # print("====================================================")
    # mention = cn.get_mention("what is the mass of [I-].[I-].[I-].[I-].[Po]")
    # rst = cn.find_cid(mention=mention)
    # print(rst)
    # print("====================================================")
    #
    # mention = cn.get_mention("what is the mass of C6H6")
    # rst = cn.find_cid(mention=mention)
    # print(rst)
    # print("====================================================")
    #
    # mention = cn.get_mention("what is C6H6's smiles")
    # rst = cn.find_cid(mention=mention)
    # print(rst)
    # print("====================================================")
    #
    # mention = cn.get_mention("what is CC[CH2]'s inchi")
    # rst = cn.find_cid(mention=mention)
    # print(rst)
    # print("====================================================")
    #
    # mention = cn.get_mention("what is the molecular weight of methane")
    # rst = cn.find_cid(mention=mention)
    # print(rst)
    # print("====================================================")
    #
    # print(time.time() - START_TIME)
