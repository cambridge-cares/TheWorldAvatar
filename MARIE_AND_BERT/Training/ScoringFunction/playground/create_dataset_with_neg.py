# randomly select candidates and create negative samples.


# load "question_set" into dataframe

import os
import pandas as pd
import random
from Marie.Util.location import TRAINING_DIR
raw_question_set = pd.read_csv(os.path.join(TRAINING_DIR, 'question_set'), sep=',', header = None)

# add col 3 with all 1
# extract col 2 and get the list of entities
# for each row, create n negative samples and assign 0


raw_question_set.insert(3, 3, 1)
raw_question_set.columns = ['question', 'head', 'tail', 'score']
question_set_with_neg = pd.DataFrame(columns=raw_question_set.columns)

# entity_list = list(set( raw_question_set['tail'].values.tolist()))
#
# print(entity_list)
# for index, row in raw_question_set.iterrows():
#     q = row['question']
#     e_h = row['head']
#     e_t = row['tail']
#     label = 0
#     fake_candidates = random.sample([f_c for f_c in entity_list if f_c != e_t and f_c.startswith(e_h + '_')], 2)
#     # fake_candidates = [f_c for f_c in entity_list if f_c != e_t and f_c.startswith(e_h + '_')]
#     for f_c in fake_candidates:
#         tmp = [q, e_h, f_c, label]
#         tmp_series = pd.Series(tmp, index = raw_question_set.columns)
#         raw_question_set = raw_question_set.append(tmp_series, ignore_index=True)


raw_question_set = raw_question_set.reset_index(drop=True)
raw_question_set.to_csv(os.path.join(TRAINING_DIR, r'question_set_full'), sep='\t')
print('exported')

