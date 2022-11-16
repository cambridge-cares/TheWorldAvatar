import json
import os
import pickle
from Marie.Util.location import DATA_DIR



class IRIDictionary():
    def __init__(self):
        self.dataset_path = os.path.join(DATA_DIR, 'CrossGraph')
        self.cid2comp  = pickle.load(open(os.path.join(self.dataset_path, "cid2compchem.pkl"),'rb'))
        self.cid2ontokin  = pickle.load( open(os.path.join(self.dataset_path, "cid2ontokin.pkl"), 'rb'))
        self.cid2ontospecies  = pickle.load( open(os.path.join(self.dataset_path, "cid2ontospecies.pkl"), 'rb'))
        self.ontokinNormaizedDict  = pickle.load( open(os.path.join(self.dataset_path, "ontokinNormalized.pkl"), 'rb'))

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


if __name__ == '__main__':
    d = IRIDictionary()
    selected_iri = []
    for cid in d.cid2ontokin:
        ontokin_label = d.cid2ontokin[cid]
        if ontokin_label == 'CO2':
            print(d.ontokinNormaizedDict[ontokin_label])
        ontokin_iri_list = d.ontokinNormaizedDict[ontokin_label]
        ontokin_iri_list = [d.shorten_IRI(ontokin_iri) for ontokin_iri in ontokin_iri_list]
        selected_iri = selected_iri + ontokin_iri_list


    with open(os.path.join(d.dataset_path, 'selected_iris.json'), 'w') as f:
        f.write(json.dumps(list(set(selected_iri))))
        f.close()