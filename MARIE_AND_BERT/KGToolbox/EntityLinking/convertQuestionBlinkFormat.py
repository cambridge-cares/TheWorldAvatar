#{text, entity, idx, title}
'''
Convert question files train/val/test files |jsonl to specfic format required by method BLINK/ELQ.
'''
import json

def convert2Blink(raw_file_addr = 'data/questionstest.jsonl', out_file_addr = 'test.jsonl'):
    with open(raw_file_addr, 'r') as f:
        questions = []
        for line in f.readlines():
            questions.append(json.loads(line))
    #[{"id": "1176", "label": "unknown", "label_id": -1, "mention": "urea", "context_left": "what is the melting point of", "context_right": ""}
    #({'mention':chosen, 'text':gq, 'id':id, 'entity':name})

    with open(out_file_addr, 'w') as outfile:
        entries = []
        for idx, entry in enumerate(questions):
            item = {}
            item['label'] = entry['des']
            item['id'] = idx
            item['label_id'] = entry['id']
            item['label_title'] = entry['entity']
            item['mention'] = m = entry['mention']
            cs = entry['text'].split(m)
            item['context_left'] = cs[0].strip()
            item['context_right'] = cs[1].strip()
            json.dump(item, outfile)
            outfile.write('\n')


#use only one dict for all training/test/valid file

def convert2ELQ(raw_file_addr = './data/elqvalid.jsonl', out_file_addr = 'valid.jsonl', start_id=0):
    with open(raw_file_addr, 'r') as f:
        questions = []
        for line in f.readlines():
            questions.append(json.loads(line))
    #{"id": "WebQTrn-0", "text": "what is the name of justin bieber brother?", "mentions": [[20, 33]], "label_id": [2798632], "wikidata_id": ["Q34086"], "entity": ["Justin Bieber"], "label": ["text"], "entities_fb": ["m.06w2sn5"], "main_entity_idx": 0}
    #({'mention':chosen, 'text':question txt, 'id':id, 'entity':name})

    with open(out_file_addr, 'w') as outfile:
        entries = []
        for idx, entry in enumerate(questions):
            item = {}
            item['id'] = idx+start_id
            mentions = entry['mention']
            mentions = [mentions] if type(mentions)==str else mentions
            item['mentions'] = mentions
            if type(entry['id']) is not list:
                ids = [entry['id']]
            else:
                ids = entry['id']
            ids = [int(id) for id in ids]
            item['label_id'] = ids
            item['text'] = entry['text']
            entities = entry['entity']
            entities = [entities] if type(entities)==str else entities
            item['entity'] = entities
            dess= entry['des']
            dess = [dess] if type(dess)==str else dess
            item['label'] = dess
            item['context_left'] = []
            item['context_right'] = []
            for m in mentions:
                cs = entry['text'].split(m)
                item['context_left'].append(cs[0].strip())
                item['context_right'].append(cs[1].strip())
            json.dump(item, outfile)
            outfile.write('\n')





#TODO: what of double mention?
def getMentionBound(text, mentions):
    for m in mentions:
        start = text.index(m)
        end = len(m)+start
        return [[start, end]]



if __name__ == "__main__":
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('--convert_type', type=str, choices=['blink', 'elq'],help='convert to blink(1st step) format or elq(2nd step) format')
    parser.add_argument('--infile', type=str, default='generate_training_data/test_tbox.jsonl',help='path to input raw file')
    parser.add_argument('--outfile', type=str, default='test_tbox.jsonl',help='path to output formatted file')

    opts = parser.parse_args()
    opts = vars(opts)
    infile = opts['infile']
    ctype = opts['convert_type']
    outfile = opts['outfile']
    if ctype == 'elq':
        convert2ELQ(raw_file_addr = infile, out_file_addr =outfile )
    else:
        convert2Blink(raw_file_addr = infile, out_file_addr =outfile )