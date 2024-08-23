import json
import os
import pickle
from Marie.Util.location import DATA_DIR


class IRIDictionary():
    def __init__(self):
        self.dataset_path = os.path.join(DATA_DIR, 'CrossGraph')
        self.cid2comp = pickle.load(open(os.path.join(self.dataset_path, "cid2compchem.pkl"), 'rb'))
        self.cid2ontokin = pickle.load(open(os.path.join(self.dataset_path, "cid2ontokin.pkl"), 'rb'))
        self.cid2ontospecies = pickle.load(open(os.path.join(self.dataset_path, "cid2ontospecies.pkl"), 'rb'))
        self.ontokinNormaizedDict = pickle.load(open(os.path.join(self.dataset_path, "ontokinNormalized.pkl"), 'rb'))

    def getIRI(self, domain, cid):
        cid = str(cid)
        if domain == 'ontocompchem':
            if cid not in self.cid2comp:
                return None
            return [self.cid2comp[cid]]
        elif domain == 'ontokin':
            if cid not in self.cid2ontokin:
                return None
            return self.ontokinNormaizedDict[self.cid2ontokin[cid]][0]
        elif domain == 'ontospecies':
            if cid not in self.cid2ontospecies:
                return None
            return [self.cid2ontospecies[cid]]
        else:
            raise Exception('Domain not defined')

    def shorten_IRI(self, iri):
        if '#' in iri:
            iri = iri.split('#')[-1]
        elif '/' in iri:
            iri = iri.split('/')[-1]
        return iri

    def create_missing_cid_list(self):
        k = list(self.cid2comp.keys())
        k = [int(cid) for cid in k if int(cid) > 10000]
        k = sorted(k)
        with open(os.path.join(DATA_DIR, 'CrossGraph', 'missing_cid_list.json'),'w') as f:
            f.write(json.dumps(k))
            f.close()



if __name__ == '__main__':
    d = IRIDictionary()
    d.create_missing_cid_list()
