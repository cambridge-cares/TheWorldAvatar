import logging

import numpy as np
import pandas as pd
from tqdm import tqdm

import scoring


class InstanceMatcherWithAutoCalibration():

    def __init__(self):
        self.score_manager = None

    #TODO-AE remove attribute prop_prop_sim_tuples (was just for testing). replace it completely by params_mapping
    def start(self, srconto, tgtonto, params_blocking, params_mapping=None, prop_prop_sim_tuples=None):

        self.score_manager = scoring.create_score_manager(srconto, tgtonto, params_blocking)

        # TODO-AE: We start with automatic property mapping
        # --> configurable, also: fixed property mapping (e.g. geo coordinates)
        # TODO-AE: symmetrical mapping (with max value for entities of dataset 2)

        if prop_prop_sim_tuples:
            mode = 'test'
        else:
            mode = params_mapping['mode']

        logging.info('starting InstanceMatcherWithAutoCalibrationAgent with mode=%s', mode)

        if mode == 'auto':
            # TODO-AE 211028 auto maybe moved, e.g. as extra step in coordinator
            params_sim_fcts = params_mapping['similarity_functions']
            sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)
            property_mapping = scoring.find_property_mapping(self.score_manager, sim_fcts)

        elif mode in ['fixed', 'test']:

            if mode == 'fixed':
                prop_prop_sim_tuples = scoring.create_prop_prop_sim_triples_from_params(params_mapping)
                logging.info('created prop_prop_sim_tuples from params_mapping')

            logging.info('prop_prop_sim_tuples=%s', prop_prop_sim_tuples)

            property_mapping = []
            for pos, t in enumerate(prop_prop_sim_tuples):
                prop1, prop2, sim_fct = t
                self.score_manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)
                row = {
                    'key': str(pos) + '_max',
                    'prop1': prop1,
                    'prop2': prop2,
                    'score_fct': sim_fct
                }
                property_mapping.append(row)
            logging.info('added prop_prop_sim_tuples to score manager, number=%s', len(self.score_manager.get_prop_prop_fct_tuples()))

            self.score_manager.calculate_similarities_between_datasets()
            self.score_manager.calculate_maximum_scores()

        else:
            raise RuntimeError('unknown mode', mode)

        df_scores = self.score_manager.get_scores()
        #TODO-AE asymmetry
        df_max_scores = self.score_manager.get_max_scores_1()
        df_total_scores, df_total_best_scores = self.calculate_auto_calibrated_total_scores(df_scores, df_max_scores, property_mapping)
        return df_total_scores, df_total_best_scores

    def calculate_auto_calibrated_total_scores_for_index(self, df_scores, df_max_scores, property_mapping, idx_1, skip_column_number = 1):
        best_score = 0
        best_pos = None
        total_score_rows = []

        for pos, (idx_2, row) in enumerate(df_scores.loc[idx_1].iterrows()):
            score = 0
            number_columns = len(property_mapping)
            prop_score = {}
            for propmap  in property_mapping:

                c_max = propmap['key']
                c = int(c_max.split('_')[0])
                value = row[c]

                #TODO-AE changed at 210926
                #if (not value is None) and (type(value) is float and not np.isnan(value)):
                if not (value is None or type(value) is str or np.isnan(value)):
                    # TODO-AE check: <= in line 1 and 3 leads to worse results than <
                    # TODO-AE experimental idea: problem with just a few 'discrete values' (e.g. 0 and 1 for match and mismatch fuel, or 0, 1, 2 edit distance)
                    # is: matches are penalized when using <= (around 0) but when using < instead nonmatches benefit (around 1)
                    # idea: use <= around 0 and < around 1 and "interpolate" in between
                    # this idea should not have much effect if there are many 'discrete values' and there is no lumping on values around 0
                    mask = (df_scores[c] > value)
                    count_m_plus_n = len(df_scores[mask])
                    mask = (df_max_scores[c_max] > value)
                    count_m = len(df_max_scores[mask])
                    if count_m_plus_n == 0:
                        column_score = 1
                    else:

                        #TODO-AE URGENT 211022
                        column_score = count_m / count_m_plus_n

                        '''
                        mask = (df_scores[c] == value)
                        count_m_plus_n_equal = len(df_scores[mask])
                        mask = (df_scores[c] < value)
                        count_m_plus_n_greater = len(df_scores[mask])
                        if count_m_plus_n_equal == 1:
                            denom = count_m_plus_n
                        else:
                            denom = count_m_plus_n + count_m_plus_n_equal * (count_m_plus_n / (count_m_plus_n + count_m_plus_n_greater))


                        mask = (df_max_scores[c_max] == value)
                        count_m_equal = len(df_max_scores[mask])
                        mask = (df_max_scores[c_max] < value)
                        count_m_greater = len(df_max_scores[mask])
                        if count_m_equal == 1:
                            nom = count_m
                        else:
                            #TODO-AE: better (count_m_equal - 1)
                            nom = count_m + count_m_equal * (count_m / (count_m + count_m_greater))
                        column_score = nom / denom


                        column_score = count_m / denom
                        '''
                    '''
                    if log:
                        if count_m_plus_n == 0:
                            print(c, value, 'ZERO', column_score)
                        else:
                            print(c, value, count_m, count_m_plus_n, 'orginal score=', count_m / count_m_plus_n, 'new score=', column_score)
                            print('\t', count_m, count_m_equal, count_m_greater, 'nom=', nom)
                            print('\t', count_m_plus_n, count_m_plus_n_equal, count_m_plus_n_greater, 'denom=', denom)
                    '''
                    score += column_score


                    #TODO-AE 211015 calibrated score for each prop
                    prop_score.update({c: column_score})


                else:
                    #TODO-AE how to score missing data?
                    number_columns = number_columns - 1


            # TODO-AE 211026 replace by get_total_score function
            if number_columns <= skip_column_number:
                score = 0.
                print('score = 0 since number columns=', number_columns, idx_1, idx_2)
            else:
                score = score / number_columns

            total_score_row = {
                'idx_1': idx_1,
                'idx_2': idx_2,
                'score': score,
                'best': False,
                'pos_1': row['pos_1'],
                'pos_2': row['pos_2'],
            }

            total_score_row.update(prop_score)

            total_score_rows.append(total_score_row)

            # TODO-AE what about equality?
            if score > best_score or best_pos is None:
                best_score = score
                best_pos = pos

        total_score_rows[best_pos]['best'] = True

        return total_score_rows

    def calculate_auto_calibrated_total_scores(self, df_scores, df_max_scores, property_mapping):

        logging.info('calculating auto calibrated total scores')

        df_scores['score'] = 0.

        rows = []
        for idx_1, _ in tqdm(df_max_scores.iterrows()):
            #TODO-AE URGENT
            skip_column_number = 1
            total_score_rows = self.calculate_auto_calibrated_total_scores_for_index(df_scores, df_max_scores, property_mapping, idx_1, skip_column_number = skip_column_number)
            rows.extend(total_score_rows)

        df_total_scores = pd.DataFrame(rows)
        df_total_scores.set_index(['idx_1', 'idx_2'], inplace=True)
        mask = (df_total_scores['best'] == True)
        df_total_best_scores = df_total_scores[mask]

        logging.info('calculated auto calibrated total scores')

        return df_total_scores, df_total_best_scores


class InstanceMatcherWithScoringWeights():

    def __init__(self):
        self.score_manager = None

    def start(self, srconto, tgtonto, params_blocking, params_mapping=None, prop_prop_sim_tuples=None):

        logging.info('starting InstanceMatcherWithScoringWeightsAgent')

        self.score_manager = scoring.create_score_manager(srconto, tgtonto, params_blocking)

        if params_mapping:
            prop_prop_sim_tuples = scoring.create_prop_prop_sim_triples_from_params(params_mapping)
            logging.info('created from params_mapping prop_prop_sim_tuples=%s', len(prop_prop_sim_tuples))
        else:
            logging.info('prop_prop_sim_tuples=%s', len(prop_prop_sim_tuples))

        property_mapping = []
        for pos, t in enumerate(prop_prop_sim_tuples):
            prop1, prop2, sim_fct = t
            self.score_manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)
            row = {
                'key': str(pos) + '_max',
                'prop1': prop1,
                'prop2': prop2,
                'score_fct': sim_fct
            }
            property_mapping.append(row)
        logging.info('added prop_prop_sim_tuples, number=%s', len(self.score_manager.get_prop_prop_fct_tuples()))

        self.score_manager.calculate_similarities_between_datasets()
        #scores = self.get_scores()

    def get_scores(self):
        return self.score_manager.get_scores()

    def calculate_total_score(self, scores):
        pass

def add_total_scores(df_scores, props, scoring_weights=None, missing_score=None, aggregation_mode='sum', average_min_prop_count=2):

    if scoring_weights is None:
        scoring_weights = [1.] * len(props)
    elif len(props) != len(scoring_weights):
        raise RuntimeError('props and scoring weights have to be of same dimension', props, scoring_weights)

    for i, row in df_scores.iterrows():
        total_score = get_total_score_for_row(row[props], scoring_weights, missing_score, aggregation_mode, average_min_prop_count)
        if total_score is None or np.isnan(total_score):
            logging.info('MY OOPS 2')
        df_scores.at[i, 'score'] = total_score

def get_total_score_for_row(prop_scores, scoring_weights=None, missing_score=None, aggregation_mode='sum', average_min_prop_count=2):
    scores = []
    prop_count = len(prop_scores)
    for i, score in enumerate(prop_scores):
        new_score = score if score is not None else missing_score
        if new_score is None:
            prop_count = prop_count - 1
        else:
            if scoring_weights:
                new_score = scoring_weights[i] * new_score
            if not (new_score is None or np.isnan(new_score)):
                scores.append(new_score)

    if aggregation_mode == 'sum':
        return sum(scores)
    elif aggregation_mode == 'mean':
        if prop_count < average_min_prop_count:
            return 0.
        else:
            return sum(scores) / prop_count
    else:
        raise RuntimeError('unknown aggregation mode=', aggregation_mode)
