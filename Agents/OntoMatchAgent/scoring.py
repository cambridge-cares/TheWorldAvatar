import logging

import nltk
import numpy as np
import pandas as pd
from tqdm import tqdm

import blocking

def check_str(v1, v2):
    return (v1 and v2 and isinstance(v1, str) and isinstance(v2, str))

def check_numerical(v1, v2):
    check_num = lambda v : (v is not None) and (isinstance(v, int) or (isinstance(v, float) and not np.isnan(v)))
    return check_num(v1) and check_num(v2)

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

def similarity_from_dist_fct(dist_fct, cut_off_mode='fixed', cut_off_value=1, decrease = 'linear'):
    #TODO-AE
    # parameters to describe a monotone decreasing conversion fct c:[0, oo) --> [0,1] with c(0) = 1
    # cut_off_mode could also be 'max' or 'quantile' (withc cut_off_value = percentage)
    # monotonicity could also by 1 - 1/x for (discrete values 1, 2, 3) or 1/exponential or ...
    # (fixed, 1, linear) --> score(v1, v2) = 1 - dist_fct(v1, v2)
    def similarity_from_dist_fct_internal(v1, v2):
        dist = dist_fct(v1, v2)
        if dist is not None:
            return 1 - (min(dist, cut_off_value) / cut_off_value)
        return None

    return similarity_from_dist_fct_internal

def create_similarity_functions_from_params(params_sim_fcts):

    sim_fcts = []

    for s_fct in params_sim_fcts:
        name = s_fct['name']
        cut_off_mode = s_fct.get('cut_off_mode')
        if cut_off_mode:
            # name refers to a distance function
            dist_fct = globals()[name]
            cut_off_value = s_fct.get('cut_off_value', 1)
            decrease = s_fct.get('decrease', 'linear')
            sim_fct = similarity_from_dist_fct(dist_fct, cut_off_mode, cut_off_value, decrease)
        else:
            raise RuntimeError('configured similarity or distance function is not implemented', name)

        sim_fcts.append(sim_fct)

    return sim_fcts

class ScoreManager():

    def __init__(self, data1: pd.DataFrame, data2: pd.DataFrame, pair_iterator):
        self.data1 = data1
        self.data2 = data2
        self.pair_iterator = pair_iterator
        self.prop_prop_fct_tuples = []
        self.df_scores = None
        self.df_max_scores_1 = None
        self.df_max_scores_2 = None

    def get_data1(self):
        return self.data1

    def get_data2(self):
        return self.data2

    def get_prop_prop_fct_tuples(self):
        return self.prop_prop_fct_tuples

    def get_scores(self):
        return self.df_scores

    def get_max_scores_1(self):
        return self.df_max_scores_1

    def get_max_scores_2(self):
        return self.df_max_scores_2

    def add_prop_prop_fct_tuples(self, property1: str, property2: str, fcts):
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
            iter(fcts)
        except:
            fcts = [fcts]

        for p1 in props1:
            for p2 in props2:
                for fct in fcts:
                    # TODO-AE check whether scoring_fcts works for p1 / p2
                    self.prop_prop_fct_tuples.append((p1, p2, fct))

    @staticmethod
    def calculate_between_entities(entity1, entity2, prop_prop_fct_tuples):

        result = []
        for prop1, prop2, fct in prop_prop_fct_tuples:
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
        for pos1, pos2 in tqdm(self.pair_iterator):
            count += 1
            #print(pos1, pos2)
            idx1 = self.data1.index[pos1]
            idx2 = self.data2.index[pos2]
            row1 = self.data1.loc[idx1]
            row2 = self.data2.loc[idx2]
            assert row1['pos'] == pos1
            assert row2['pos'] == pos2
            row = [idx1, idx2, pos1, pos2]
            scores = ScoreManager.calculate_between_entities(row1.to_dict(), row2.to_dict(), self.prop_prop_fct_tuples)
            row.extend(scores)
            rows.append(row)

        columns = ['idx_1', 'idx_2', 'pos_1', 'pos_2']
        for i, s in enumerate(self.prop_prop_fct_tuples):
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

        logging.info('calculating maximum scores, dataset_id=%s, number of scores=%s', dataset_id, len(self.prop_prop_fct_tuples) )
        idx_values = self.df_scores.index.get_level_values(dataset_id-1).unique()
        logging.info('number of entities=%s', len(idx_values))

        #TODO-AE store the position of column in score_fct ...
        columns = []
        for c in range(len(self.prop_prop_fct_tuples)):
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

def create_score_manager(srconto, tgtonto, params_blocking):
    dframe1 = blocking.create_dataframe_from_ontology(srconto)
    dframe2 = blocking.create_dataframe_from_ontology(tgtonto)
    logging.info('columns of dataset 1 =%s', dframe1.columns)
    logging.info('columns of dataset 2 =%s', dframe2.columns)

    it = blocking.create_iterator(srconto, tgtonto, params_blocking)
    manager = ScoreManager(dframe1, dframe2, it)

    return manager

def find_property_mapping(manager: ScoreManager, similarity_functions:list, props1=None, props2=None) -> list :

    dframe1 = manager.get_data1()
    dframe2 = manager.get_data2()

    if not props1:
        props1 = [ str(c) for c in dframe1.columns ]
        props1.remove('pos')

    if not props2:
        props2 = [ str(c) for c in dframe2.columns ]
        props2.remove('pos')

    for prop1 in props1:
        for prop2 in props2:
            for sim_fct in similarity_functions:
                # TODO-AE remove sim_fct that do not fit to property datatype
                manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)

    logging.info('added prop prop sim tuples, number=%s', len(manager.get_prop_prop_fct_tuples()))

    manager.calculate_scores_between_datasets()

    df_max_scores_1, df_max_scores_2 = manager.calculate_maximum_scores()

    logging.info('\nmax scores for dataset 1:\n------------------')
    logging.info('\n%s', df_max_scores_1)
    logging.info('\n%s', df_max_scores_1.describe())

    means = df_max_scores_1.describe().loc['mean']
    logging.info('means=\n%s', means)

    pos_sim_fcts = {}
    for i, s in enumerate(similarity_functions):
        pos_sim_fcts.update({s:i})

    rows = []
    for i, prop_prop_sim in enumerate(manager.get_prop_prop_fct_tuples()):
        prop1, prop2, s = prop_prop_sim
        pos_sfct = pos_sim_fcts[s]
        key = str(i) + '_max'
        mean = means.get(key)
        if mean is None:
            key = None
        row = {
            'key': key,
            'mean': mean,
            'prop1': prop1,
            'prop2': prop2,
            'pos_sfct': pos_sfct,
        }
        rows.append(row)

    df_means = pd.DataFrame(rows)
    logging.debug('mean values:\n%s', df_means[:1000].to_string())

    # TODO-AE stable marriage, symmetrical, skip below some threshold....
    min_threshold = 0.5
    property_mapping = []
    for prop1 in df_means['prop1'].unique():
        mask = ((df_means['prop1'] == prop1) & (df_means['key'].apply(lambda x: x is not None)))
        df_tmp = df_means[mask].sort_values(by='mean', ascending=False)
        max_count = min(3, len(df_tmp))
        logging.debug('best mean values for %s: \n%s', prop1, df_tmp[:max_count])
        max_row = df_tmp.iloc[0].to_dict()
        max_mean = max_row['mean']
        if  max_mean >= min_threshold:
            pos_sfct = max_row['pos_sfct']
            max_row.update({
                'sim_fct': similarity_functions[pos_sfct]
            })
            property_mapping.append(max_row)

    logging.info('found property mapping=%s', property_mapping)
    return property_mapping
