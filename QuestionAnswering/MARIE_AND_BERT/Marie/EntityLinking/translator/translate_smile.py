import sys

from STOUT import translate_forward, translate_reverse
import re
import transformers
import torch
from Marie.EntityLinking.translator.ner_smile import NerSMILE
from transformers import BertTokenizerFast

class Translator():
    def __init__(self, modelpath=None, model='bert-base-cased',num_label=7):
        if modelpath:
            modelconfig = {
                'tokenizer': BertTokenizerFast.from_pretrained('bert-base-cased'),
                'device': 'cuda' if torch.cuda.is_available() else 'cpu',
                'model_path': modelpath,
				'num_label' :num_label,
				'model':model
            }
            self.ner = NerSMILE(modelconfig)
            self.translate = self.translate_ner
            print('use ner')
        else:
            self.translate = self.translate_regex

    def translate_regex(self, raw_data):
        for entry in raw_data:
            text = entry["text"]
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
        # print(smiles)
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

    def extract_ner(self,raw_data):
        sentences = [entry['text'] for entry in raw_data]
        names, types = self.ner.extractAllTypes(sentences)
        return names, types


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


    dummy = [{'text': 'List the Chemical Building Units with 4-planar as the Generic Building Unit'},
             {'text': 'List the Chemical Building Units with 3-pyramidal as the Generic Building Unit'},
             {'text': 'List the Chemical Building Units with 2-bent as the Generic Building Unit '},
             {'text': 'List the MOPs with  (3-pyramidal)2(2-bent)3(D3h) as the assembly model '},
             {'text': 'List the MOPs with assembly model (3-planar)4(3-pyramidal)4(Td)  '},
             {'text': 'What type of assembly models can be contructed with Ru2 as the Chemical Building Unit '},
             {'text': 'With [(C12H6NH)(C6H4)2(CO2)2] as the CBU, list the possible assembly models '},
             {'text': 'Show all assembly model types with Mo2 CBU '},
             {'text': 'What are the assembly models that can be formed with [(C10H4)(CO)4N2)(CHCH)2(CH3)2(CH2CH3)2(CO2)2] as the CBU '},
             {'text': 'With Chemical Building Unit [(C3N3)(C6H4)3(CO2)3], list the types of assembly models '},
             {'text': 'Show all assembly model types with Mo2 CBU '},
             {'text': 'Give the assembly model types that can be created with [(C4H2S)(CO2)2] as the CBU'},
             {'text': 'Using [WV5O11] as the chemical building unit, what assembly model types can be formed '},
             {'text': 'Using [PW9O37Ni6NH2C4H3] as the CBU, what are the assembly model types that can be formed '},
             {'text': 'List the MOPS with a tetrahedral shape '},
             {'text': 'List the Generic Building Unit characteristics to form a triangular assembly model'},
             {'text': 'Show the characteristics of the GBUs required to form an assembly model of Icosahedron shape'},
             {'text': 'What is the modularity of the CBU [(C6H3)((C2H4O2)(C9H5O2))(CO2)2] in some MOPs'},
             {'text': 'List the MOPS with a tetrahedral shape '},
             {'text': 'Starting with Oh symmetry point group, list all the MOPs'},
             {'text': 'Give the MOPs that are uncharged'},
             {'text': 'Give MOPs with a negative charge'},
             {'text': 'What is the associated modularity with [Rh2] in MOPs'},
             {'text': 'What types of GBUs can be used to form a tetrahedral assembly model'},
             {'text': 'what MOPs have [Zr3O(OH)3(C5H5)3] as the Chemical Building Unit'},
             {'text': 'With [Mo2] as the CBU, list all the MOPs'},
             {'text': 'Give the MOPs with Ru2 as the Chemical Buildinng Unit'},
             {'text': 'Give the MOPs which are Anticuboctahedron shaped'},
             {'text': 'Give the MOPs that are cube'},
             {'text': 'List the MOPs that are Icosahedron shape'},
             {'text': 'Show all the cube-shaped MOPs'},
             {'text': 'List the assembly models that can be formed with Cu2'},
             {'text': 'List all the MOPs with Ih symmetry point group'},
             {'text': 'Starting with Oh symmetry point group, list all the MOPs'},
             {'text': 'List all the MOPs with Td symmetry point group'},
             {'text': 'Starting with the D3h symmetry point group, list all the MOPs'},
             {'text': 'What are the assembly model types with [Zr3O(OH)3(C5H5)3] chemical building unit'},
             {'text': 'Show all the MOPs having Octahedron shape'},
             {'text': 'List the MOPs with trigonal bipyramid as the shape'}
             ]
    dummy = [{
        'text': 'What is the name for C(=O)C(C(=O)O)O'}]
    t = Translator(modelpath="./DATA/EntityLinking/SMILES_NER_V10.bin")
    out = t.translate_ner(dummy)
    print(out)
