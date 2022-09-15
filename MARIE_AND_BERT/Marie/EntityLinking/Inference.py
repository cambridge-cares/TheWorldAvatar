import os.path
#import sys
#sys.path.append("C:/Users/Shaocong/Documents/GitHub/TheWorldAvatar/MARIE_AND_BERT")
from Marie.EntityLinking.blink.common.params import BlinkParser
from Marie.EntityLinking.blink.biencoder.eval_biencoder import NEL
import yaml
from Marie.EntityLinking.util.ParseResult import prase_inference
from Marie.EntityLinking.chemspot.annotator import Annotator
from Marie.EntityLinking.util.EntityDict import load_entity_dict
from Marie.Util.location import ENTITY_LINKING_CONF_DIR, ENTITY_LINKING_DATA_DIR

'''
All files needed for Entity linking go to "MARIE_AND_BERT/DATA/EntityLinking"
'''


class NELInfer():
    def __init__(self, config_name='base500.yaml'):
        # Basic config to load NEL model
        parser = BlinkParser(add_model_args=True)
        parser.add_eval_args()
        args = parser.parse_args()
        self.params = args.__dict__
        self.params['mode'] = 'test'
        self.params['zeshel'] = False
        if config_name is not None:
            with open(os.path.join(ENTITY_LINKING_CONF_DIR, config_name), "r") as f:
                config = yaml.load(f, Loader=yaml.FullLoader)
                for key in config:
                    self.params[key] = config[key]
        # entity dictionary for reading labels from id
        self.params['path_to_model'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['path_to_model'])
        self.params['cand_encode_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['cand_encode_path'])
        self.params['entity_dict_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['entity_dict_path'])
        self.params['output_path'] = os.path.join(ENTITY_LINKING_DATA_DIR, self.params['output_path'])

        self.entity_dict = load_entity_dict(self.params['entity_dict_path'])
        self.annotator = Annotator()
        self.nel = NEL(self.params)

    def infer(self, mention_data, use_ner=False):
        if use_ner:
            mention_data = self.ner_tag(mention_data)
        # run main function
        result_path = self.nel.infer(mention_data)
        # parse topk result to entity id&label
        inferred = prase_inference(self.entity_dict, result_path)
        return inferred

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


if __name__ == '__main__':
    from util.MentionEntry import load_mention_entries

    # read a test file
    testdata = [{"mention": "urea", "context_left": "what is the chemical formula of", "context_right": ""}]
    rawdata = [{"text": "what is the chemical formula of urea"}, {"text":"what is the chemical formula of benzene"}]
    # rawdata = load_mention_entries("data/pubchem/test.jsonl")
    # rawdata = [{"text":l+' '+m+' '+r} for m,l,r in rawdata]
    model = NELInfer('base500.yaml')
    # tagged = model.ner_tag(rawdata)
    import time
    start = time.time()
    print('start')
    result = model.infer(rawdata, use_ner=True)
    print(result)
    print(time.time() - start)
