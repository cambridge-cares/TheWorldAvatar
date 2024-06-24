#load predictions, check success rate
# Prediction
import torch
import transformers
from util import loadjsonl
import os
from transformers import BertTokenizer, AutoTokenizer
import re

SPECIAL_SYM = "[,\'\[\]\{\}\(\)\-\]\*\+\-\_\.\/\:\>]"
def prediction_fn(model, tokenized_sub_sentence):

    tkns = tokenized_sub_sentence
    indexed_tokens = config['tokenizer'].convert_tokens_to_ids(tkns)
    segments_ids = [1] * len(indexed_tokens)

    tokens_tensor = torch.tensor([indexed_tokens]).to(config['device'])
    segments_tensors = torch.tensor([segments_ids]).to(config['device'])

    model.eval()
    with torch.no_grad():
        logit = model(tokens_tensor,
                      token_type_ids=None,
                      attention_mask=segments_tensors)

        logit_new = logit[0].argmax(2).detach().cpu().numpy().tolist()
        prediction = logit_new[0]

        kword = ''
        kword_list = []
        label_list = []

        for k, j in enumerate(prediction):
            if (len(prediction)>1):#The sentence is more than one token long
                if j == 2:
                    j = 0
                if (j!=0) & (k==0):
                    #if it's the first word in the first position
                    #print('At begin first word')
                    begin = tkns[k]
                    kword = begin

                elif (j!=0) & (k>=1) & (prediction[k-1]==0):
                    #begin word is in the middle of the sentence
                    begin = tkns[k]
                    previous = tkns[k-1]

                    if begin.startswith('##'):
                        kword = previous + begin[2:]
                    else:
                        kword = begin

                    if k == (len(prediction) - 1):#end of sentence
                        #print('begin and end word is the last word of the sentence')
                        kword_list.append(kword.rstrip().lstrip())
                        label_list.append(j)

                elif (j!=0) & (k>=1) & (prediction[k-1]!=0):
                    # Or first word of a keyword of another category
                    if prediction[k-1] != j:#Not same category
                        #Add last keyword to list
                        kword_list.append(kword.rstrip().lstrip())
                        label_list.append(j)
                        #Restart the word counting
                        begin = tkns[k]
                        kword = begin

                    else:
                        # intermediate word of the same keyword
                        inter = tkns[k]


                        if inter.startswith('##'):
                            kword = kword + "" + inter[2:]
                        elif re.match(SPECIAL_SYM, inter) or re.match(SPECIAL_SYM, tkns[k-1]):
                            kword = kword + "" + inter
                        else:
                            kword = kword + " " + inter


                    if k == (len(prediction) - 1):#end of sentence
                        #print('begin and end')
                        kword_list.append(kword.rstrip().lstrip())
                        label_list.append(j)

                elif (j==0) & (k>=1) & (prediction[k-1] !=0):
                    # End of a keywords but not end of sentence.
                    kword_list.append(kword.rstrip().lstrip())
                    label_list.append(prediction[k-1])
                    kword = ''
                    inter = ''
            else:
                if (j!=0):
                    begin = tkns[k]
                    kword = begin
                    kword_list.append(kword.rstrip().lstrip())
                    label_list.append(j)

    return kword_list,label_list,prediction

#data format?
def get_predictions(model, sentence_list):
    results = {}
    types = {}
    logits = {}
    for id, sentence in enumerate(sentence_list):
        current_id_predictions = []
        tokenized_sub_sentence = config['tokenizer'].tokenize(sentence)

        if len(tokenized_sub_sentence) == 0:
            # If the tokenized sentence are empty
            sub_sentence_prediction_kword_list,sub_type_list = [],[]

        elif len(tokenized_sub_sentence) <= 512:#TODO:ignore sentences that are too long for now
            # If the tokenized sentence are less than 512
            sub_sentence_prediction_kword_list,sub_type_list,sub_logit = prediction_fn(model, tokenized_sub_sentence)

        if len(sub_sentence_prediction_kword_list) !=0:#found some keywords
            current_id_predictions = current_id_predictions + sub_sentence_prediction_kword_list

        results[id] = [x.upper() for x in list(set(current_id_predictions))]
        types[id] = sub_type_list
        logits[id] = sub_logit
    return results, types,logits

import json
def load_entity_dict(path, name_only=False):
    assert path is not None, "Error! entity_dict_path is empty."
    entity_list = []
    with open(path, 'rt') as f:
        for line in f:
            sample = json.loads(line.rstrip())
            entity_list.append(sample)


    return entity_list


def kword2original(sentence, kwords):
    orginal = []
    for kword in kwords:
        start = sentence.lower().find(kword)
        end = start + len(kword)
        ori = sentence[start:end]
        orginal.append(ori)
    return orginal


def metric(predictions, questions, compareLabel=True):
    correct = 0
    for idx in range(len(questions)):
        if compareLabel:
            goldens = questions[idx]['label']
            prediction = predictions[2][idx]
            if goldens == prediction:
               correct+=1
            else:
                print(idx)
                print(questions[idx]['text'])
                print(goldens)
                print(prediction)
        else:
            goldens = questions[idx]['entity']
            goldens = [g.upper() for g in goldens]
            true_types = questions[idx]['types']

            prediction,types = predictions[0][idx], predictions[1][idx]
            if len(prediction) < 1:
                continue
            allCorrect = True
            for g in goldens:
                if g not in prediction:
                    allCorrect = False
            if allCorrect:
                correct+=1
            else:
                print(idx)
                print(questions[idx]['text'])
                print(goldens)
                print(prediction)

    all = len(questions)
    print('acc: {}'.format(correct/all))


def main(config):
    pass
    #config parameters
    #read model
    model = transformers.BertForTokenClassification.from_pretrained('bert-base-cased',  num_labels = 7)
    model = torch.nn.DataParallel(model)
    model.load_state_dict(torch.load(config['model_path']))
    #read test data
    test_file = os.path.join(config['datadir'], 'valid_species_use_cased.jsonl')
    final_test_id_list, final_test_sentences, final_test_labels = loadjsonl(test_file)
    #get prediction
    results = get_predictions(model, final_test_sentences)
    questions = load_entity_dict(test_file)
    metric(results, questions, False)
    #for id in range(len(results)):
    #    print(results[id])
    #postprocess result format

if __name__ == '__main__':
    config = {
              'tokenizer': BertTokenizer.from_pretrained('bert-base-cased'),
              'datadir':'./data/multiple',
              'device': 'cuda' if torch.cuda.is_available() else 'cpu',
              'model_path':'./SMILES_NER_V11.bin'
              }
    main(config)