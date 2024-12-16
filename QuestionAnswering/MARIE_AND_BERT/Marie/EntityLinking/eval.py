import json
import os

from Marie.EntityLinking.IRILookup import IRILookup
from Marie.Util.location import DATA_DIR

'''
accu metrics

'''


def metrics(result, questions):
    correct = 0
    if len(result) != len(questions):
        raise ValueError('results and questions length do not match')
    for idx, item in enumerate(result):
        if item is None:
            continue
        cid_str = item[1]
        cid = int(cid_str.replace('CID', ''))
        # bounds = item[2]
        # pre_entity_label = item[3]
        GT_cid = questions[idx]['label_id'][0]
        if cid == GT_cid:
            correct = correct + 1

    print('acc: {}'.format(correct / len(questions)))


def load_testfile(path):
    questions = []
    with open(path, 'rt') as f:
        for line in f:
            sample = json.loads(line.rstrip())
            questions.append(sample)
    return questions


if __name__ == '__main__':
    from ChemicalNEL import ChemicalNEL
    cn = ChemicalNEL()
    my_iri = IRILookup(dataset_name="pubchem")
    questions = load_testfile(os.path.join(DATA_DIR, 'Evaluation', 'entitylinking', 'test.jsonl'))
    result = []
    for q in questions:
        rst = my_iri.find_cid(q['text'])
        result.append(rst)
    metrics(result, questions)
