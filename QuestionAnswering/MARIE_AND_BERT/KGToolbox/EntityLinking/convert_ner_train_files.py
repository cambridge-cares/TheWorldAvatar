import json
tags_2_idx = {'O': 0 , 'B': 1, 'P': 2, 'C':3, 'I-K':4, 'I-M':5, 'S':6}
from transformers import BertTokenizer
import numpy as np
import csv
import random

#read jsonl original file and convert it to token-label format


def labelling(data,clabel,mixed=False,start_idx=0):
    '''
    This function is to iterate each of the training data and get it labelled
    from the form_labels() function.
    '''
    outs = []
    tokenizer = BertTokenizer.from_pretrained("bert-base-cased")
    for idx, datum in enumerate(data):
        text_tokens = tokenizer.tokenize(datum[0])
        label_tokens = [tokenizer.tokenize(d) for d in datum[1]]
        if not mixed:
            clabel_seq = [clabel] * len(label_tokens)
            print(clabel_seq)
            labeled = form_labels_mixed_type(text_tokens, label_tokens,clabel_seq)
        else:
            #assert type(clabel) is list
            labeled = form_labels_mixed_type(text_tokens, label_tokens, clabel[idx])
        out = {}
        out['id'] = idx+start_idx
        out['label'] = labeled[0]
        out['text'] = datum[0]
        outs.append(out)
    return outs

def form_labels( text_tokens, entity_tokens, clabel=1):
    '''
    This function labels the training data
    '''
    label = []
    z = np.array([0] * len(text_tokens)) # Create final label == len(tokens) of each sentence
    matched_keywords = 0 # Initially no kword matched, label all tokens 0


    for i in range(len(text_tokens)):
        if text_tokens[i: (i + len(entity_tokens))] == entity_tokens:#yeah, find our match
            matched_keywords += 1

            if (len(entity_tokens) == 1):
                z[i] = clabel #label name token 1
            else:
                z[i] = clabel
                z[(i+1) : (i+ len(entity_tokens))]= clabel

            label.append(z.tolist())

    return label

def form_labels_mixed_type( text_tokens, entities_tokens, label_types):
    '''
    This function labels the training data
    '''
    label = []
    z = np.array([0] * len(text_tokens)) # Create final label == len(tokens) of each sentence
    matched_keywords = 0 # Initially no kword matched, label all tokens 0

    idxEntity = 0
    last = len(entities_tokens) - 1
    this_entity_token = entities_tokens[idxEntity]
    for i in range(len(text_tokens)):
        if text_tokens[i: (i + len(this_entity_token))] == this_entity_token:#yeah, find our match
            label_digit = label_types[idxEntity]
            if (len(this_entity_token) == 1):
                z[i] = label_digit #label name token 1
            else:
                z[i] = label_digit
                z[(i+1) : (i+ len(this_entity_token))]= label_digit
            idxEntity = min(idxEntity + 1, last)
            this_entity_token = entities_tokens[idxEntity]


    label.append(z.tolist())

    return label


def getlabelFile(range, savepath):
    templates = []
    with open('templates.csv', newline='') as csvfile:
        reader = csv.reader(csvfile, delimiter = '.')
        for row in reader:
            templates.append(row[0].strip())
    raw = []
    with open('./data/pubchem.csv') as f:
        lines = csv.reader(f)
        next(lines)
        for line in list(lines)[range[0]:range[1]]:
            smile = line[15]
            template = templates[random.randrange(0, 25)]
            question = template.format(smile)
            raw.append((question, smile))

    out = labelling(raw)
    with open(savepath, 'w') as f:
        for datum in out:
            json.dump(datum, f)
            f.write('\n')


#{"mention": "Solvents", "text": "Do all Solvents contains neutrons", "id": 90, "entity": "Solvents", "des": ""}
def generate_from_question_files(qfile, clabel,savepath, mixed =False):
    raw = []
    types_all = []
    with open(qfile, 'r') as f:
        for line in f.readlines():
            item = json.loads(line)
            question = item['text']
            mention = item['mention']
            types = item['types'] if 'types' in item else None
            raw.append((question, mention))
            types_all.append(types)
    if mixed:
        out = labelling(raw,types_all, mixed)
    else:
        if clabel is None:
            clabel = 1
        out = labelling(raw, clabel)
    with open(savepath, 'w') as f:
        for datum in out:
            json.dump(datum, f)
            f.write('\n')

if __name__ == '__main__':
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('--outfile', type=str,help='path to ouput converted file')
    parser.add_argument('--infile', type=str, default='generate_training_data/test_tbox.jsonl',help='path to input raw file')
    opts = parser.parse_args()
    opts = vars(opts)
    outfile = opts['outfile']
    infile = opts['infile']
    generate_from_question_files(infile , None , outfile,mixed=False)