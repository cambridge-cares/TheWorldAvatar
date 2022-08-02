import json
import torch


def load_infer_data(path):
    q_list = []
    with open(path, 'rt') as f:
        for line in f:
            sample = json.loads(line.rstrip())
            id = ['label_id'] if 'label_id' in sample else None
            label = sample.get("label_title", "").strip() if "label_title" in sample else None

        q_list.append((id,label))

    return q_list


def prase_inference(entitydict, candidate_result):
    labels = candidate_result['labels']
    inferred=[]
    for qid, ids in enumerate(labels):
        #print('golden: {}'.format(golden_label))
        #print(' | '.join(names))
        inferred_entity_id = entitydict[ids[0]][2]
        inferred_entity_label = entitydict[ids[0]][0]
        inferred.append((inferred_entity_id, inferred_entity_label))

    #print('Eval: {}/500 not in top 1'.format(not_top))
    return inferred

def write_jsonl(outpath, inferred):
    with open(outpath, 'w') as outfile:
        for entry in inferred:
            item = {}
            item['id'] = entry[0]
            item['label'] = entry[1]
            json.dump(item, outfile)
            outfile.write('\n')