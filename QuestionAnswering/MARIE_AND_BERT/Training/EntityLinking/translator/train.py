'''
Train NER for smile string recognition

'''
import os.path
import sys
sys.path.append("")
sys.path.append("../../Marie/Util")
sys.path.append("../../Marie")
sys.path.append("../..")
sys.path.append("../../..")
sys.path.append("../../../..")
import torch
from torch.utils.data import DataLoader
from tqdm import tqdm
import numpy as np
from torch import nn
import  transformers
from transformers import BertTokenizer, AutoTokenizer
from util import loadjsonl
tags_2_idx={'O': 0 , 'B': 1, 'P': 2, 'C':3, 'I-K':4, 'I-M':5, 'S':6}
# 0: no-tag, 1: SMILES tag, 2: padding-tag, 3: class-tag, 4:instance_ontokin tag, 5:instance ontomops tag, 6:species_use tag
#Only first three code is used for SMILES-NER, can expand to include other type NER by supplying files under this code conduct

class form_input():
    def __init__(self, ID, sentence, label, data_type='test',config ={}):
        self.id = ID
        self.sentence = sentence
        self.label = label
        self.max_length = config['MAX_LEN']
        self.tokenizer = config['tokenizer']
        self.data_type = data_type
        self.config = config

    def __len__(self):
        return len(self.sentence)

    def __getitem__(self, item):
        toks = self.config['tokenizer'].tokenize(self.sentence[item])
        label = self.label[item]

        if len(toks)>self.max_length:
            toks = toks[:self.max_length]
            label = label[:self.max_length]


        ########################################
        tag_pad = tags_2_idx['P']
        # Forming the inputs
        ids = self.config['tokenizer'].convert_tokens_to_ids(toks)
        tok_type_id = [0] * len(ids)
        att_mask = [1] * len(ids)

        # Padding
        pad_len = self.max_length - len(ids)
        ids = ids + [2] * pad_len
        tok_type_id = tok_type_id + [0] * pad_len
        att_mask = att_mask + [0] * pad_len

        ########################################
        # Forming the label
        if self.data_type !='test':
            label = label + [tag_pad]*pad_len
        else:
            label = 1


        return {'ids': torch.tensor(ids, dtype = torch.long),
                'tok_type_id': torch.tensor(tok_type_id, dtype = torch.long),
                'att_mask': torch.tensor(att_mask, dtype = torch.long),
                'target': torch.tensor(label, dtype = torch.long)
                }




#train loop
def train_fn(data_loader, model, optimizer, config):
    '''
    Functiont to train the model
    '''

    train_loss = 0
    for index, dataset in enumerate(tqdm(data_loader, total = len(data_loader))):
        batch_input_ids = dataset['ids'].to(config['device'], dtype = torch.long)
        batch_att_mask = dataset['att_mask'].to(config['device'], dtype = torch.long)
        batch_tok_type_id = dataset['tok_type_id'].to(config['device'], dtype = torch.long)
        batch_target = dataset['target'].to(config['device'], dtype = torch.long)

        output = model(batch_input_ids,
                       token_type_ids=None,
                       attention_mask=batch_att_mask,
                       labels=batch_target)

        step_loss = output[0]
        prediction = output[1]

        step_loss.sum().backward()
        optimizer.step()
        train_loss += step_loss
        optimizer.zero_grad()

    return train_loss.sum()

def eval_fn(data_loader, model, config):
    '''
    Function to evaluate the model on each epoch.
    We can also use Jaccard metric to see the performance on each epoch.
    '''

    model.eval()

    eval_loss = 0
    predictions = np.array([], dtype = np.int64).reshape(0, config['MAX_LEN'])
    true_labels = np.array([], dtype = np.int64).reshape(0, config['MAX_LEN'])

    with torch.no_grad():
        for index, dataset in enumerate(tqdm(data_loader, total = len(data_loader))):
            batch_input_ids = dataset['ids'].to(config['device'], dtype = torch.long)
            batch_att_mask = dataset['att_mask'].to(config['device'], dtype = torch.long)
            batch_tok_type_id = dataset['tok_type_id'].to(config['device'], dtype = torch.long)
            batch_target = dataset['target'].to(config['device'], dtype = torch.long)

            output = model(batch_input_ids,
                           token_type_ids=None,
                           attention_mask=batch_att_mask,
                           labels=batch_target)

            step_loss = output[0]
            eval_prediction = output[1]

            eval_loss += step_loss

            eval_prediction = np.argmax(eval_prediction.detach().to('cpu').numpy(), axis = 2)
            actual = batch_target.to('cpu').numpy()

            predictions = np.concatenate((predictions, eval_prediction), axis = 0)
            true_labels = np.concatenate((true_labels, actual), axis = 0)

    return eval_loss.sum(), predictions, true_labels

#define model
def train_engine(epoch, train_data, valid_data,config):
    model = transformers.BertForTokenClassification.from_pretrained('bert-base-cased',  num_labels = len(tags_2_idx))
    model = nn.DataParallel(model)
    if 'ckpt' in  config:#load checkpoint
        model.load_state_dict(torch.load(config['ckpt']))

    model = model.to(config['device'])

    params = model.parameters()
    optimizer = torch.optim.Adam(params, lr= 3e-5)

    best_eval_loss = 1000000
    for i in range(epoch):
        train_loss = train_fn(data_loader = train_data,
                              model=model,
                              optimizer=optimizer, config=config)
        eval_loss, eval_predictions, true_labels = eval_fn(data_loader = valid_data,
                                                           model=model, config=config)

        print(f"Epoch {i} , Train loss: {train_loss}, Eval loss: {eval_loss}")

        if eval_loss < best_eval_loss:
            best_eval_loss = eval_loss

            print("Saving the model")
            torch.save(model.state_dict(), config['model_name'])

    return model, eval_predictions, true_labels


def main(config):
    train_file3 = config['train_file_name']
    valid_file3 = config['valid_file_name']

    final_train_id_list, final_train_sentences, final_train_labels = loadjsonl([train_file3])
    final_valid_id_list, final_valid_sentences, final_valid_labels = loadjsonl([valid_file3])

    train_prod_input = form_input(ID=final_train_id_list,
                                  sentence=final_train_sentences,
                                  label=final_train_labels,
                                  data_type='train', config = config)

    valid_prod_input = form_input(ID=final_valid_id_list,
                                  sentence=final_valid_sentences,
                                  label=final_valid_labels,
                                  data_type='valid', config = config)

    train_prod_input_data_loader = DataLoader(train_prod_input,
                                              batch_size= config['batch_size'],
                                              shuffle=True)

    valid_prod_input_data_loader = DataLoader(valid_prod_input,
                                              batch_size= config['batch_size'],
                                              shuffle=True)
    model, val_predictions, val_true_labels = train_engine(epoch=config['Epoch'],
                                                       train_data=train_prod_input_data_loader,
                                                       valid_data=valid_prod_input_data_loader, config=config)#save

if __name__ == '__main__':
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('--MAX_LEN', type=int, default=256,help='max token length of input question')
    parser.add_argument('--batch_size', type=int,default=4, help='batch size for training')
    parser.add_argument('--Epoch', type=int, default=1,help='epoches to train')
    parser.add_argument('--model_name', type=str, default='./models/SMILES_NER.bin',help='model name to save')
    parser.add_argument('--valid_file_name', type=str, default='valid_smiles.jsonl')
    parser.add_argument('--train_file_name', type=str, default='train_smiles.jsonl')

    opts = parser.parse_args()
    opts = vars(opts)
    opts['tokenizer'] = BertTokenizer.from_pretrained('bert-base-cased')
    opts['device'] = 'cuda' if torch.cuda.is_available() else 'cpu'
    main(opts)