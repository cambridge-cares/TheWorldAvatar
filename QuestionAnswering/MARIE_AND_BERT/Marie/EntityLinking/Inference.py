import os.path
import sys

from Marie.EntityLinking.translator.translate_smile import Translator
from Marie.EntityLinking.blink.common.params import BlinkParser
from Marie.EntityLinking.blink.biencoder.eval_biencoder import NEL
from Marie.EntityLinking.elq.wrapper import NEL_ELQ
import yaml
from Marie.EntityLinking.util.ParseResult import prase_inference
from Marie.EntityLinking.chemspot.annotator import Annotator
from Marie.EntityLinking.util.EntityDict import load_entity_dict
from Marie.Util.location import ENTITY_LINKING_CONF_DIR, ENTITY_LINKING_DATA_DIR

'''
All files needed for Entity linking go to "MARIE_AND_BERT/DATA/EntityLinking"
'''


class BertNEL():
    def __init__(self,
                 config_name='base500.yaml',
                 one_pass=True,
                 use_translation=True):
        self.one_pass = one_pass
        if one_pass:
            # Basic config to load ELQ-NEL model
            self.params = {}
            self.readconf(config_name)
            self.params["biencoder_model"] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['path_to_model'])
            self.params["biencoder_config"] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['path_to_model_config'])
            self.params['entity_encoding'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['cand_encode_path'])
            self.params['entity_catalogue'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['entity_dict_path'])
            self.params['entity_dict_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['entity_dict_path'])
            self.params['output_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['output_path'])
            self.nel = NEL_ELQ(self.params)
            print('-Loading BERT NEL Model')
        else:
            # Basic config to load BLINK-NEL model
            parser = BlinkParser(add_model_args=True)
            parser.add_eval_args()
            args = parser.parse_args()
            self.params = args.__dict__
            self.readconf(config_name)
            self.params['mode'] = 'test'
            self.params['zeshel'] = False
            # entity dictionary for reading labels from id
            self.params['path_to_model'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['path_to_model'])
            self.params['cand_encode_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['cand_encode_path'])
            self.params['entity_dict_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['entity_dict_path'])
            self.params['output_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['output_path'])
            self.annotator = Annotator()
            self.nel = NEL(self.params)
        self.entity_dict = load_entity_dict(self.params['entity_dict_path'])
        self.params['smile_model_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['smile_model_path'])
        smile_translator = Translator(modelpath=self.params['smile_model_path'])
        self.translate = smile_translator.translate
        self.use_translation = use_translation

    def readconf(self, config_name):
        if config_name is not None:
            with open(os.path.join(ENTITY_LINKING_CONF_DIR, config_name), "r") as f:
                config = yaml.load(f, Loader=yaml.FullLoader)
                for key in config:
                    self.params[key] = config[key]

    def infer(self, mention_data, use_translation=True, use_ner=False):
        if use_translation:
            mention_data = self.translate(mention_data)
        if not self.one_pass:  # Use separate NER instead
            mention_data = self.ner_tag(mention_data)
        else:  # check input format
            mention_data = self.process_data(mention_data)
        # run main function
        result_matrix = self.nel.infer(mention_data)
        # parse topk result to entity id&label
        inferred = prase_inference(self.entity_dict, result_matrix, mention_data, one_pass_format=self.one_pass)
        return inferred

    def find_cid(self, question):
        question = self.process_data(question)
        smiles = ""
        if self.use_translation:
            question, smiles = self.translate(question)
            # pass
        if not self.one_pass:  # Use separate NER instead
            question = self.ner_tag(question)
        # run main function
        result_matrix = self.nel.infer(question)
        # parse topk result to entity id&label
        results = prase_inference(self.entity_dict, result_matrix, question, one_pass_format=self.one_pass)
        # confidence, cid, mention_string, name
        # If smiles, return orginal smile string!
        if len(question) == 1:
            results = results[0]
        return results, smiles

    def ner_tag(self, raw_data):
        """Tag out possible mention from raw data.
        raw_data: data string list
        """
        processed = []
        for entry in raw_data:
            entry = entry["text"].lower()
            tagged = self.annotator.tag(entry)
            if tagged:
                start, end, mention = tagged
                marked_entry = {}
                marked_entry["mention"] = mention
                marked_entry["context_left"] = entry[:start]
                marked_entry["context_right"] = entry[end:]
                marked_entry["label"] = "unknown"
                marked_entry["label_id"] = -1
                processed.append(marked_entry)
            else:  # Handle failure
                print('ner failed for {}'.format(entry))
        return processed

    def process_data(self, raw_data):
        if type(raw_data) == str:
            raw_data = [{"text": raw_data}]
        if type(raw_data) is not list or len(raw_data)==0:
                raise ValueError('question not in right format')
        if 'id' not in raw_data[0]:
            raw_data = [{"text": entry['text'], "id": id} for id, entry in enumerate(raw_data)]
        return raw_data


if __name__ == '__main__':
    # read a test file

    testdata = [{"mention": "urea", "context_left": "what is the chemical formula of", "context_right": ""}]
    rawdata = [{"text": "what is the exact mass of C6H6"}, {"text": "what is C9H18NO4+."}]
    # rawdata = load_mention_entries("data/pubchem/test.jsonl")
    # rawdata = [{"text":l+' '+m+' '+r} for m,l,r in rawdata]
    model = BertNEL('base500.yaml')
    # tagged = model.ner_tag(rawdata)

    import time

    start = time.time()

    print('start')
    result = model.find_cid("what is the exact mass of C=C=C=C and CH4")
    print(result)
    print(time.time() - start)

    print('start')
    result = model.find_cid("what is the exact mass of CO2")
    print(result)
    print(time.time() - start)

    print('start')
    result = model.find_cid("what is the exact mass of benzene")
    print(result)
    print(time.time() - start)
