"""
The Cross Graph Score model aligns scores across graphs embedded differently
e.g. Pubchem and OntoCompchem, which uses TransE and Complex
"""
from Marie.PubChem import PubChemEngine


# Dataset: (question, domain, true answer)
# s_pubchem = Pubchem(question) = [x1, x2, x3, x4, x5]
# s_onto =    OntoCompChem(question) = [y1, y2, y3, y4, y5]
# true answer [y1]

# Model: BERT(q) -> Linear (768, 2) [w1, w2]
# s_pubchem = w1 * s_pubchem
# s_onto = w2 * s_onto
# y_pos = y1, y_neg = x_n, randomly selected

# quick experiment
# Train: L = MarginRankingLoss
class Trainer:

    def __init__(self):
        pass
