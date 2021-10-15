import logging

import nltk
import numpy as np
import pandas as pd
from tqdm import tqdm

def check_str(v1, v2):
    return (v1 and v2 and isinstance(v1, str) and isinstance(v2, str))

def check_numerical(v1, v2):
    return (v1 and v2 and ((isinstance(v1, int) and isinstance(v2, int))
            or (isinstance(v1, float) and isinstance(v2, float) and not (np.isnan(v1) or np.isnan(v2)))))

def dist_nltk_edit(v1, v2):
    if not check_str(v1, v2):
        return None
    return nltk.edit_distance(v1, v2)

def dist_bounded_edit(max_edit_distance):
    def dist_bounded_edit_internal(v1, v2):
        if not check_str(v1, v2):
            return None
        edit_dist = nltk.edit_distance(v1, v2)
        if edit_dist > max_edit_distance:
            return 1
        return edit_dist / (edit_dist + 1)
    return dist_bounded_edit_internal

def dist_absolute(v1, v2):
    if not check_numerical(v1, v2):
        return None
    return abs(v1 - v2)

def dist_relative(v1, v2):
    if not check_numerical(v1, v2):
        return None
    return abs(v1 - v2) / max(abs(v1), abs(v2))

def dist_equal(v1, v2):
    if v1 is None or v2 is None:
        return
    return (0 if v1 == v2 else 1)

def convert_to_score_fct(dist_fct, cut_off_mode='fixed', cut_off_value=1, decrease = 'linear'):
    #TODO-AE
    # parameters to describe a monotone decreasing conversion fct c:[0, oo) --> [0,1] with c(0) = 1
    # cut_off_mode could also be 'max' or 'quantile' (withc cut_off_value = percentage)
    # monotonicity could also by 1 - 1/x for (discrete values 1, 2, 3) or 1/exponential or ...
    # (fixed, 1, linear) --> score(v1, v2) = 1 - dist_fct(v1, v2)
    def score_fct_internal(v1, v2):
        dist = dist_fct(v1, v2)
        if dist is not None:
            return 1 - (min(dist, cut_off_value) / cut_off_value)
        return None

    return score_fct_internal

class ScoreManager():

    def __init__(self, data1: pd.DataFrame, data2: pd.DataFrame, pair_iterator):
        self.data1 = data1
        self.data2 = data2
        self.pair_iterator = pair_iterator
        self.score_fcts = []
        self.df_scores = None
        self.df_max_scores_1 = None
        self.df_max_scores_2 = None

    def get_score_fcts(self):
        return self.score_fcts

    def get_scores(self):
        return self.df_scores

    def get_max_scores_1(self):
        return self.df_max_scores_1

    def get_max_scores_2(self):
        return self.df_max_scores_2

    def add_score_fcts(self, property1: str, property2: str, score_fcts):
        if property1 and (not property1 in self.data1.columns):
            raise RuntimeError('property1 not found in dataframe columns', property1)
        if property2 and (not property2 in self.data2.columns):
            raise RuntimeError('property2 not found in dataframe columns', property2)

        if property1:
            props1 = [property1]
        else:
            props1 = [ str(c) for c in self.data1.columns]

        if property2:
            props2 = [property2]
        else:
            props2 = [ str(c) for c in self.data2.columns]

        try:
            iter(score_fcts)
        except:
            score_fcts = [score_fcts]

        for p1 in props1:
            for p2 in props2:
                for score_fct in score_fcts:
                    # TODO-AE check whether scoring_fcts works for p1 / p2
                    self.score_fcts.append((p1, p2, score_fct))

    @staticmethod
    def calculate_between_entities(entity1, entity2, prop_prop_fcts):

        result = []
        for prop1, prop2, fct in prop_prop_fcts:
            v1 = entity1[prop1]
            v2 = entity2[prop2]
            value = fct(v1, v2)
            assert value is None or (value >= 0 and value <= 1)
            result.append(value)

        return result

    def calculate_scores_between_datasets(self):

        logging.info('calculating scores')
        count = 0
        rows = []
        for pos1, pos2 in self.pair_iterator:
            count += 1
            #print(pos1, pos2)
            idx1 = self.data1.index[pos1]
            idx2 = self.data2.index[pos2]
            row1 = self.data1.loc[idx1]
            row2 = self.data2.loc[idx2]
            assert row1['pos'] == pos1
            assert row2['pos'] == pos2
            row = [idx1, idx2, pos1, pos2]
            scores = ScoreManager.calculate_between_entities(row1.to_dict(), row2.to_dict(), self.score_fcts)
            row.extend(scores)
            rows.append(row)

        columns = ['idx_1', 'idx_2', 'pos_1', 'pos_2']
        for i, s in enumerate(self.score_fcts):
            #prop1, prop2, _ = s
            #j = prop1.rfind('/')
            #str_prop1 = (prop1 if j==-1 else prop1[j+1:])
            #j = prop2.rfind('/')
            #str_prop2 = (prop2 if j==-1 else prop2[j+1:])
            #dist_column = '_'.join([str(i), 'dist', str_prop1, str_prop2])
            dist_column = i
            columns.append(dist_column)

        self.df_scores = pd.DataFrame(data=rows, columns=columns)
        self.df_scores.set_index(['idx_1', 'idx_2'], inplace=True)
        logging.info('calculated scores, number of pairs=%s, score columns=%s', len(self.df_scores), columns)
        return self.df_scores

    def calculate_maximum_scores(self):
        self.df_max_scores_1 = self.__calculate_maximum_scores(1)
        #TODO-AE
        #self.df_max_scores_2 = self.__calculate_maximum_scores(2)
        return self.df_max_scores_1, self.df_max_scores_2

    def __calculate_maximum_scores(self, dataset_id):

        logging.info('calculating maximum scores, dataset_id=%s, number of scores=%s', dataset_id, len(self.score_fcts) )
        idx_values = self.df_scores.index.get_level_values(dataset_id-1).unique()
        logging.info('number of entities=%s', len(idx_values))

        #TODO-AE store the position of column in score_fct ...
        columns = []
        for c in range(len(self.score_fcts)):
            columns.append(c)

        index_column_name = 'idx_' + str(dataset_id)

        result_rows = []
        count = 0

        for idx in tqdm(idx_values):
            result_row = {}
            cand = self.df_scores.loc[idx]
            count += len(cand)

            for c in columns:
                max_score = None
                for _, row in cand.iterrows():
                    score = row[c]
                    if not ((score is None) or np.isnan(score)):
                        if max_score is None or score > max_score:
                            max_score = score

                result_row.update({str(c) + '_max': max_score})

            result_row.update({index_column_name: idx})
            result_rows.append(result_row)

        df_result = pd.DataFrame(result_rows)
        df_result.set_index([index_column_name], inplace=True)
        logging.info('calculated maximum scores, number of entities=%s, number of pairs=%s', len(df_result), count)
        return df_result
