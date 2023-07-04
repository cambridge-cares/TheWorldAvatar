from Marie.EntityLinking.translator.translate_smile import Translator
from Marie.Util.CommonTools import NumericalTools
from Marie.Util.Logging import MarieLogger
# ==============================================================================================================

import json
import time
import os
from Marie.Util.location import DICTIONARY_DIR, ENTITY_LINKING_DATA_DIR
from Marie.EntityLinking.Inference import BertNEL


class ChemicalNEL:
    """

    """

    def __init__(self):
        self.marie_logger = MarieLogger()
        self.ner = BertNEL()
        self.translator = Translator(
            modelpath=os.path.join(ENTITY_LINKING_DATA_DIR, "SMILES_NER_V10.bin"))


if __name__ == '__main__':
    cn = ChemicalNEL()
    text = "mops with cuboctahedron shape"


    # cn = ChemicalNEL(dataset_name="wikidata_numerical", enable_class_ner=True)
    # START_TIME = time.time()
    #
    # # text = "find species with molecular weight more than"
    # question_list = ["show me the melting point of CH4",
    #                  "what is the smiles string of CH4O4S",
    #                  "find species with boiling point larger than 10 degrees",
    #                  "find species with boiling point smaller than 10 degrees",
    #                  "find species with boiling point around 100 degrees"]
    # for text in question_list:
    #     # text = "find species with molecular weight over 100"
    #     mention = cn.get_mention(text)
    #     print("mention", mention)
    #     print("---------------")

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
