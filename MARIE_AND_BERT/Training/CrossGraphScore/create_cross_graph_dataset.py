import pandas as pd
import os
# from Marie.PubChem import PubChemEngine
# from Marie.OntoChemistry import OntoChemistryEngine
from Marie.Util.location import DATA_DIR


# pubchem_engine = PubChemEngine()
# ontochemistry_engine = OntoChemistryEngine()
#
# answers, scores = pubchem_engine.find_answers(question='charge', head_entity='CID1')
# scores = [10 - s for s in scores]
# print(answers)
# print(scores)
#
# answers, scores = ontochemistry_engine.run(question='charge', head_entity='1b206169-7539-3491-85ec-a40dfe351a2a')
# print(answers)
# print(scores)

df_pubchem = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'pubchem_cross_score.tsv'), sep='\t', index_col=0)
df_onto = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'ontochemistry_cross_score.tsv'), sep='\t', index_col=0)
# sample 110 from df_onto
df_onto = df_onto.sample(n=110)
df_onto = df_onto.reset_index(drop=True)

df = pd.concat([df_onto, df_pubchem])
df = df.reset_index(drop=True)
df.to_csv(os.path.join(DATA_DIR, 'CrossGraph', 'all_cross_score.tsv'), sep='\t')

