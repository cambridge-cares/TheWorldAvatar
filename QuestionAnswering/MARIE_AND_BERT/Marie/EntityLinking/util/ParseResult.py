import json
import torch
import math

def load_infer_data(path):
    q_list = []
    with open(path, 'rt') as f:
        for line in f:
            sample = json.loads(line.rstrip())
            id = ['label_id'] if 'label_id' in sample else None
            label = sample.get("label_title", "").strip() if "label_title" in sample else None

        q_list.append((id,label))

    return q_list


#TODO: the mention is lowered case, restore original
def prase_inference(entitydict, candidate_result, mention_data, one_pass_format=True):
    inferred=[]
    if one_pass_format:
        for entry in candidate_result:
            print(entry)
            entity_line_idx = int(entry['pred_triples'][0][0])
            inferred_entity_cid = 'CID'+str(entitydict[entity_line_idx][2]) #The
            inferred_entity_label = entitydict[entity_line_idx][0]
            text = entry['text']
            score = entry['scores'][0]
            score = 1+ score/4.5   # A log scared score in [-4.5, 0], corresponding to probability [0, 1]
            mentionbounds = entry['pred_tuples_string'][0][2]
            mention = text[mentionbounds[0]:mentionbounds[1]]
            inferred.append((score, inferred_entity_cid, mention, inferred_entity_label ))
    else:
        labels = candidate_result['labels']
        for qid, ids in enumerate(labels):
            #print('golden: {}'.format(golden_label))
            #print(' | '.join(names))
            inferred_entity_cid = entitydict[ids[0]][2]
            inferred_entity_label = entitydict[ids[0]][0]
            mention = mention_data[qid]['mention']
            inferred.append((inferred_entity_cid, inferred_entity_label, mention))
            #confidence, cid, mention_string, name
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