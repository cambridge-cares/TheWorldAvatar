from STOUT import translate_forward, translate_reverse
import re
import transformers
import torch
from Marie.EntityLinking.translator.ner_smile import NerSMILE
from transformers import BertTokenizerFast


class Translator():
    def __init__(self, modelpath=None):
        if modelpath:
            modelconfig = {
                'tokenizer': BertTokenizerFast.from_pretrained('bert-base-uncased'),
                'device': 'cuda' if torch.cuda.is_available() else 'cpu',
                'model_path': modelpath
            }
            self.ner = NerSMILE(modelconfig)
            self.translate = self.translate_ner
            print('use ner')
        else:
            self.translate = self.translate_regex

    def translate_regex(self, raw_data):
        for entry in raw_data:
            text = entry["text"]
            # TODO: this won't work when . is inside the SMILE
            tokens = [x.strip().replace('.', '').replace('?', '') for x in text.split(' ')]
            for idx, t in enumerate(tokens):
                if self.regexSMILES(t):
                    translated = self.translateSMILES(t)
                    if translated:
                        tokens[idx] = translated
            entry['text'] = ' '.join(tokens)
        return raw_data

    def translate_ner(self, raw_data):
        smiles_string = None
        sentences = [entry['text'] for entry in raw_data]
        smiles = self.ner.extractSMILE(sentences)
        for smile_candidates, entry in zip(smiles, raw_data):
            if len(smile_candidates) > 0:
                for c in smile_candidates:
                    if c and c.lower() != "inchi":
                        smiles_string = c
                        t_c = self.translateSMILES(c)
                        if t_c:
                            pass
                            # entry['text'] = entry['text'].replace(c, t_c)
        return raw_data, smiles_string

    def regexSMILESlist(self, text_list):
        out = []
        for t in text_list:
            found = self.regexSMILES(t)
            found = found if found else ''
            out.append([found])
        return out

    def regexSMILES(self, text):
        regchem = a = re.compile(r"^([^J][a-z0-9@+\.\-\[\]\(\)\\\/%=#$]{6,})$", re.I)
        x = re.search(regchem, text)
        return x

    def translateSMILES(self, token):
        try:
            IUPAC_name = translate_forward(token)
            if "Could not generate IUPAC name from invalid SMILES." in IUPAC_name:
                return None
            return IUPAC_name
        except:
            return None


if __name__ == "__main__":
    import json


    def loadjsonl(filepath):
        with open(filepath, 'r') as f:
            items = []
            for line in f.readlines():
                items.append(json.loads(line))

        return items


    # dummy = loadjsonl("C:/Users\Shaocong\Documents\GitHub\TheWorldAvatar\MARIE_AND_BERT\DATA\EntityLinking\smile_test.jsonl")

    dummy = [{
                 'text': 'what is the chemical formula of What sort of molecule is CC=CCCC(=CCCC(=CCCC(=CCCC(=CCCC(=CCCC(=CCCC(=CCC1=C(C(=CC=C1)OC)O)C)C)C)C)C)C)C'}]
    t = Translator(modelpath="D:\work\Marie\MARIE_AND_BERT\DATA\EntityLinking\SMILES_NER.bin")
    out = t.translate(dummy)
    print([i['text'] for i in out])
