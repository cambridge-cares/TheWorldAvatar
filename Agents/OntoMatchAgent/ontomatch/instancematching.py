import logging
from operator import index
import os
import os.path
import time

import numpy as np
import pandas as pd
from tqdm import tqdm

import ontomatch.hpo
import ontomatch.matchManager
import ontomatch.scoring
import ontomatch.utils.blackboard
import ontomatch.utils.util

class InstanceMatcherBase():

    def __init__(self):
        self.score_manager = None

    def start_base(self, srconto, tgtonto, params_blocking, params_mapping):

        self.score_manager = ontomatch.scoring.create_score_manager(srconto, tgtonto, params_blocking, params_mapping)

        #TODO-AE: start with automatic property mapping
        # --> configurable, also: fixed property mapping (e.g. geo coordinates)
        mode = params_mapping['mode']

        logging.info('starting InstanceMatcherBase with mode=%s', mode)

        if mode == 'auto':
            #TODO-AE 211028 auto maybe moved, e.g. as extra step in coordinator
            params_sim_fcts = params_mapping['similarity_functions']
            sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)
            property_mapping = ontomatch.scoring.find_property_mapping(self.score_manager, sim_fcts)

        elif mode == 'fixed':
            prop_prop_sim_tuples = ontomatch.scoring.create_prop_prop_sim_triples_from_params(params_mapping)
            logging.info('created prop_prop_sim_tuples from params_mapping =%s', prop_prop_sim_tuples)

            property_mapping = []
            for t in prop_prop_sim_tuples:
                prop1, prop2, sim_fct, pos = t
                self.score_manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct, pos)
                row = {
                    'key': str(pos) + '_max',
                    'prop1': prop1,
                    'prop2': prop2,
                    'score_fct': sim_fct
                }
                property_mapping.append(row)
            logging.info('added prop_prop_sim_tuples to score manager, number=%s', len(self.score_manager.get_prop_prop_fct_tuples()))

            self.score_manager.calculate_similarities_between_datasets()

        else:
            raise RuntimeError('unknown mode', mode)

        return property_mapping

    def get_scores(self):
        return self.score_manager.df_scores

    def set_scores(self, df_scores):
        self.score_manager.df_scores = df_scores

class InstanceMatcherClassifier(InstanceMatcherBase):

    def start(self, config_handle, src_graph_handle, tgt_graph_handle, http:bool=False):
        config_json = ontomatch.utils.util.call_agent_blackboard_for_reading(config_handle, http)
        params = ontomatch.utils.util.convert_json_to_dict(config_json)

        srconto = ontomatch.utils.util.load_ontology(src_graph_handle)
        tgtonto = ontomatch.utils.util.load_ontology(tgt_graph_handle)

        self.start_internal(srconto, tgtonto, params)

    def start_internal(self, srconto, tgtonto, params):
        logging.info('starting InstanceMatcherClassifier')

        # first, check existence and consistency of train_file
        params_training = params['training']
        m_train_size = params_training['match_train_size']
        if not isinstance(m_train_size, list):
            m_train_size = [m_train_size]
        nm_ratio = params_training['nonmatch_ratio']
        if not isinstance(nm_ratio, list):
            nm_ratio = [nm_ratio]

        # perform blocking and create similarity vectors
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        params_post_processing = params['post_processing']
        self.start_base(srconto, tgtonto, params_blocking, params_mapping)
        df_scores = self.score_manager.get_scores()

        params_classification = params['classification']

        params_impution = params_training.get('impution')
        evaluation_file = params_post_processing['evaluation_file']
        index_set_matches = ontomatch.evaluate.read_match_file_as_index_set(evaluation_file, linktypes = [1, 2, 3, 4, 5])

        cross_validation = params_training['cross_validation']
        logging.info('classifying similarity vectors for combinations of train_size=%s, ratio=%s', m_train_size, nm_ratio)

        prop_columns = ontomatch.utils.util.get_prop_columns(df_scores)
        for train_size in m_train_size:
            for ratio in nm_ratio:
                #TODO-AE 211123: just a few matches might not be contained in df_scores
                # thus we skip them here, since there are just a few, training XGB would not improve much if they are considered
                # but then we have to calculate their similarity vectors
                logging.info('selecting training samples for train_size=%s, ratio=%s', train_size, ratio)
                intersection = index_set_matches.intersection(df_scores.index)
                len_diff = len(index_set_matches) - len(intersection)
                logging.info('evaluation file=%s, intersection with scores=%s, diff=%s', len(index_set_matches), len(intersection), len_diff)
                df_matches = df_scores.loc[intersection]
                x_train, y_train, x_test, y_test = ontomatch.utils.util.generate_training_set(
                    df_matches, df_scores, train_size, ratio, prop_columns=prop_columns)
                column_name = 'ml_phase_' + str(train_size) + '_' + str(ratio)
                # reuse df_scores for storing train-test-split
                df_scores[column_name] = 'test'
                df_scores.at[x_train.index, column_name] = 'train'
                #TODO-AE 211127 add train / test split and

                logging.info('classifying for train_size=%s, ratio=%s', train_size, ratio)
                self.score_manager.df_scores = ontomatch.hpo.start(params_classification, cross_validation, params_impution,
                        x_train, y_train, x_test, y_test, df_scores, prop_columns)

                if params_post_processing:
                    postprocess(params_post_processing, self, minus_train_set=x_train)

        dump = params_post_processing.get('dump')
        if dump:
            dir_name = dump + '_train_test_' + str(time.time())
            logging.info('dumping train test split to %s', dir_name)
            os.makedirs(dir_name, exist_ok=True)
            df_tmp = df_scores.drop(columns='score')
            df_tmp.to_csv(dir_name + '/train_test.csv')

class InstanceMatcherWithAutoCalibration(InstanceMatcherBase):

    def __init__(self):
        super().__init__()
        self.df_total_scores = None
        self.df_total_best_scores = None
        self.df_total_best_scores_1 = None
        self.df_total_best_scores_2 = None

    def get_scores(self):
        return self.df_total_best_scores

    def get_total_best_scores_1(self):
        return self.df_total_best_scores_1

    def get_total_best_scores_2(self):
        return self.df_total_best_scores_2

    def start(self, config_handle, src_graph_handle, tgt_graph_handle, http:bool=False):
        config_json = ontomatch.utils.util.call_agent_blackboard_for_reading(config_handle, http)
        params = ontomatch.utils.util.convert_json_to_dict(config_json)
        params_model_specific = params['matching']['model_specific']
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        params_post_processing = params['post_processing']

        srconto = ontomatch.utils.util.load_ontology(src_graph_handle)
        tgtonto = ontomatch.utils.util.load_ontology(tgt_graph_handle)

        self.start_internal(srconto, tgtonto, params_model_specific, params_blocking, params_post_processing, params_mapping)

    def start_internal(self, srconto, tgtonto, params_model_specific, params_blocking, params_post_processing=None, params_mapping=None):

        logging.info('starting InstanceMatcherWithAutoCalibration, params=%s', params_model_specific)
        symmetric = params_model_specific['symmetric']
        delta = params_model_specific.get('delta')
        if delta is None:
            delta = 0.025

        property_mapping = self.start_base(srconto, tgtonto, params_blocking, params_mapping)

        self.score_manager.calculate_maximum_scores(symmetric=symmetric)

        df_scores = self.score_manager.get_scores()
        df_max_scores = self.score_manager.get_max_scores_1()
        self.df_total_scores, self.df_total_best_scores = self.calculate_auto_calibrated_total_scores(df_scores, df_max_scores, property_mapping, delta)
        self.df_total_best_scores_1 = self.df_total_best_scores

        if symmetric:
            # df_max_scores_2 has multi index of form (idx_2, idx_1)
            df_max_scores_2 = self.score_manager.get_max_scores_2()
            # change the order of idx_1 and idx_2 (score functions are symmetric. Thus, the score value are the same)
            #df-_scores_2 = self.score_manager.get_scores().reorder_levels(['idx_2', 'idx_1'])
            df_scores_2 = self.score_manager.get_scores()
            _, df_total_best_scores_2 = self.calculate_auto_calibrated_total_scores(df_scores_2, df_max_scores_2, property_mapping, delta, dataset_id=2)
            self.df_total_best_scores_2 = df_total_best_scores_2
            logging.debug('first row of df_total_best_score_2 before reordering index=%s, row=%s', df_total_best_scores_2.index[0], df_total_best_scores_2.iloc[0])
            # 1. method calculate_auto_calibrated_total_scores store idx_2 in index with name 'idx_1. Thus, we have to change the name
            #df_total_best_scores_2 = df_total_best_scores_2.rename(columns={'idx_1': 'idx_2', 'idx_2': 'idx_1'})
            # 2. revert the above change of the index order
            #df_total_best_scores_2 = df_total_best_scores_2.reorder_levels(['idx_1', 'idx_2'])
            logging.debug('first row of df_total_best_score_2 after reordering index=%s, row=%s', df_total_best_scores_2.index[0], df_total_best_scores_2.iloc[0])

            # combine best score pairs from df_total_best_scores and df_total_best_scores_2
            self.df_total_best_scores = self.combine_total_best_scores(self.df_total_best_scores, df_total_best_scores_2)

        if params_post_processing:
            postprocess(params_post_processing, self)

        return self.df_total_scores, self.df_total_best_scores

    def combine_total_best_scores(self, df_total_1, df_total_2):
        df_combined = df_total_1[['score']].copy()
        logging.info('combining total best scores, total_1=%s, total_2=%s', len(df_total_1), len(df_total_2))

        index_1 = df_combined.index

        #TODO-AE 211215 remove, related to flipping test test_auto_calibration_with_geo_coordinates
        # just for debugging:
        index_diff = df_total_2.index.difference(df_total_1.index)
        index_diff_test = df_total_2.index.difference(index_1)
        count_virtual_1 = 0
        count_virtual_2 = 0
        for idx1, idx2 in index_1:
            if idx2 == 'virtual':
                count_virtual_1 += 1
        for idx1, idx2 in df_total_2.index:
            if idx1 == 'virtual':
                count_virtual_2 += 1
        logging.debug('combined init=%s, index init=%s, diff=%s, diff_test=%s, v1=%s, v2=%s',
                len(df_combined), len(index_1), len(index_diff), len(index_diff_test), count_virtual_1, count_virtual_2)

        count = 0
        rows = []

        for idx, row in tqdm(df_total_2.iterrows()):
            score_2 = row['score']
            if idx in index_1:
                count += 1
                score_1 = df_combined.loc[idx]['score']
                df_combined.at[idx, 'score'] = max(score_1, score_2)
            else:
                rows.append({'idx_1': idx[0], 'idx_2': idx[1], 'score': score_2})

        df_diff = pd.DataFrame(rows)
        df_diff.set_index(['idx_1', 'idx_2'], inplace=True)
        df_combined = pd.concat([df_combined, df_diff])
        logging.debug('combined total best scores, count=%s, rows=%s, df_combined=%s', count, len(rows), len(df_combined))
        return df_combined

    def calculate_auto_calibrated_total_scores_for_index(self, df_scores, sliding_counts, property_mapping, idx_1, skip_column_number = 1):
        best_score = 0
        best_pos = None
        total_score_rows = []

        for pos, (idx_2, row) in enumerate(df_scores.loc[idx_1].iterrows()):
            score = 0
            orig_score = 0
            number_columns = len(property_mapping)
            prop_score = {}
            for propmap in property_mapping:

                c_max = propmap['key']
                # TODO-AE 211215 change from int to str for column names
                #c = int(c_max.split('_')[0])
                c = c_max.split('_')[0]
                value = row[c]

                #TODO-AE changed at 210926
                #if (not value is None) and (type(value) is float and not np.isnan(value)):
                if not (value is None or type(value) is str or np.isnan(value)):
                    # TODO-AE check: <= in line 1 and 3 leads to worse results than <
                    # TODO-AE experimental idea: problem with just a few 'discrete values' (e.g. 0 and 1 for match and mismatch fuel, or 0, 1, 2 edit distance)
                    # is: matches are penalized when using <= (around 0) but when using < instead nonmatches benefit (around 1)
                    # idea: use <= around 0 and < around 1 and "interpolate" in between
                    # this idea should not have much effect if there are many 'discrete values' and there is no lumping on values around 0
                    '''
                    mask = (df_scores[c] >= value)
                    count_m_plus_n = len(df_scores[mask])
                    mask = (df_max_scores[c_max] >= value)
                    count_m = len(df_max_scores[mask])
                    '''
                    count_m = sliding_counts[c_max](value)
                    count_m_plus_n = sliding_counts[c](value)
                    if count_m == 0:
                        column_score = 0
                    elif count_m_plus_n == 0:
                        column_score = 1
                    else:

                        #TODO-AE URGENT 211022
                        column_score = count_m / count_m_plus_n

                        #TODO-AE URGENT 211211

                    orig_score += column_score
                    if column_score > 1:
                        # this can happen due to small deviations, in particular if using embedding cosine distance
                        #raise ValueError('column score exceeds 1', score, idx_1, idx_2, c_max)
                        #TODO-AE URGENT 211215
                        logging.debug('column score exceeds 1, %s', column_score)
                        column_score = 1.

                    score += column_score


                    #TODO-AE 211015 calibrated score for each prop
                    prop_score.update({c: column_score})


                else:
                    #TODO-AE how to score missing data?
                    number_columns = number_columns - 1


            # TODO-AE 211026 replace by get_total_score function
            if number_columns <= skip_column_number:
                score = 0.
                logging.debug('score = 0 since number columns=%s, idx_1=%s, idx_2=%s', number_columns, idx_1, idx_2)
            else:
                score = score / number_columns

                #TODO-AE URGENT 211211
                if score > 1:
                    logging.debug('score exceeds 1')
                orig_score = orig_score / number_columns
                if orig_score > 1:
                    logging.debug('orig score exceeds 1')

            total_score_row = {
                'idx_1': idx_1,
                'idx_2': idx_2,
                'score': score,
                'best': False,
            }

            total_score_row.update(prop_score)

            total_score_rows.append(total_score_row)

            # TODO-AE what about equality?
            if score > best_score or best_pos is None:
                best_score = score
                best_pos = pos

        total_score_rows[best_pos]['best'] = True

        return total_score_rows

    def calculate_auto_calibrated_total_scores(self, df_scores_orig, df_max_scores, property_mapping, delta, dataset_id=1):

        logging.info('calculating auto calibrated total scores, dataset_id=%s', dataset_id)

        if dataset_id == 1:
            df_scores = df_scores_orig.copy()
        else:
            df_scores = df_scores_orig.reorder_levels(['idx_2', 'idx_1'])
            logging.info('reordered levels of df_scores since dataset_id=%s', dataset_id)

        df_scores['score'] = 0.

        sliding_counts = {}
        for propmap  in property_mapping:
            c_max = propmap['key']
            series = df_max_scores[c_max]
            # TODO-A URGENT 211211
            #scount = InstanceMatcherWithAutoCalibration.sliding_count(series, delta)
            scount = InstanceMatcherWithAutoCalibration.sliding_count_fast(c_max, series, delta)
            sliding_counts[c_max] = scount

            # TODO-AE 211215 change from int to str for column names
            #c = int(c_max.split('_')[0])
            c = c_max.split('_')[0]
            series = df_scores[c]
            # TODO-A URGENT 211211
            #scount = InstanceMatcherWithAutoCalibration.sliding_count(series, delta)
            scount = InstanceMatcherWithAutoCalibration.sliding_count_fast(c, series, delta)
            sliding_counts[c] = scount

        rows = []
        for idx, _ in tqdm(df_max_scores.iterrows()):
            idx_1 = idx[0]
            #TODO-AE URGENT
            #  skip_column_number = 1
            #TODO-AE 211102 URGENT restaurant, for phone prop only, set skip_column_number = 0
            skip_column_number = 0
            total_score_rows = self.calculate_auto_calibrated_total_scores_for_index(df_scores, sliding_counts, property_mapping, idx_1, skip_column_number = skip_column_number)
            rows.extend(total_score_rows)

        df_total_scores = pd.DataFrame(rows)

        if dataset_id == 2:
            #df_total_scores = df_total_scores.rename(columns={'idx_1': 'idx_2', 'idx_2': 'idx_1', 'pos_1': 'pos_2', 'pos_2': 'pos_1'})
            df_total_scores = df_total_scores.rename(columns={'idx_1': 'idx_2', 'idx_2': 'idx_1'})
            logging.info('reordered index and position names of df_total scores since dataset_id=%s', dataset_id)

        df_total_scores.set_index(['idx_1', 'idx_2'], inplace=True)
        mask = (df_total_scores['best'] == True)
        df_total_best_scores = df_total_scores[mask]

        logging.info('calculated auto calibrated total scores, dataset_id=%s', dataset_id)

        return df_total_scores, df_total_best_scores

    @classmethod
    def sliding_count(cls, series, delta):
        def sliding_count_internal(x):
            mask = (sorted_series >= x - delta) & (sorted_series <= x + delta)
            df_tmp = sorted_series[mask]
            return len(df_tmp)

        sorted_series = series.sort_values(ascending=True).copy()
        return sliding_count_internal

    @classmethod
    def sliding_count_fast(cls, column, series, delta):
        def sliding_count_internal(x):
            pos = round(x / (2*delta))
            return counts[pos]

        counts = []
        x_list = np.arange(0, 1.00001, 2*delta)
        for x in x_list:
            mask = (series >= x - delta) & (series <= x + delta)
            count = len(series[mask])
            counts.append(count)
        logging.debug('counts for column=%s, x=%s, counts=%s', column, x_list, counts)

        return sliding_count_internal

class InstanceMatcherWithScoringWeights(InstanceMatcherBase):

    def __init__(self):
        super().__init__()

    def start(self, config_handle, src_graph_handle, tgt_graph_handle, http:bool=False):
        config_json = ontomatch.utils.util.call_agent_blackboard_for_reading(config_handle, http)
        params = ontomatch.utils.util.convert_json_to_dict(config_json)
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        params_post_processing = params['post_processing']
        scoring_weights = params['matching']['model_specific']['weights']

        srconto = ontomatch.utils.util.load_ontology(src_graph_handle)
        tgtonto = ontomatch.utils.util.load_ontology(tgt_graph_handle)

        return self.start_internal(srconto, tgtonto, params_blocking, scoring_weights, params_post_processing, params_mapping)

    def start_internal(self, srconto, tgtonto, params_blocking, scoring_weights, params_post_processing=None, params_mapping=None):

        logging.info('starting InstanceMatcherWithScoringWeightsAgent')

        property_mapping = self.start_base(srconto, tgtonto, params_blocking, params_mapping)
        prop_column_names = [ c for c in range(len(property_mapping)) ]
        df_scores = self.get_scores()

        ontomatch.instancematching.add_total_scores(df_scores, props=prop_column_names, scoring_weights=scoring_weights,
                        missing_score=None, aggregation_mode='sum', average_min_prop_count=2)

        if params_post_processing:
            postprocess(params_post_processing, self)

        return df_scores

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

def postprocess(params_post_processing, matcher, minus_train_set=None):

    logging.info('post processing')
    dump = params_post_processing.get('dump')
    if dump:
        dir_name = dump + '_' + str(time.time())
        logging.info('dumping results to %s', dir_name)
        os.makedirs(dir_name, exist_ok=True)

        if isinstance(matcher, ontomatch.matchManager.matchManager) or isinstance(matcher, ontomatch.instancematching.InstanceMatcherClassifier):
            matcher.get_scores().to_csv(dir_name + '/total_scores.csv')
        else:
            sm = matcher.score_manager
            sm.data1.to_csv(dir_name + '/data1.csv')
            sm.data2.to_csv(dir_name + '/data2.csv')

            if isinstance(matcher, ontomatch.instancematching.InstanceMatcherWithScoringWeights):
                sm.df_scores.to_csv(dir_name + '/total_scores.csv')
            elif isinstance(matcher, ontomatch.instancematching.InstanceMatcherWithAutoCalibration):
                if sm.get_max_scores_1() is not None:
                    sm.get_max_scores_1().to_csv(dir_name + '/max_scores_1.csv')
                if sm.get_max_scores_2() is not None:
                    sm.get_max_scores_2().to_csv(dir_name + '/max_scores_2.csv')
                matcher.df_total_scores.to_csv(dir_name + '/total_scores_1.csv')
                matcher.df_total_best_scores.to_csv(dir_name + '/total_best_scores.csv')
                sm.df_scores.to_csv(dir_name + '/scores.csv')
            else:
                raise RuntimeError('unsupported type of matcher', type(matcher))

    matchfile = params_post_processing['evaluation_file']
    index_set_matches = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])

    if isinstance(matcher, ontomatch.instancematching.InstanceMatcherWithAutoCalibration):
        df_scores = matcher.get_total_best_scores_1()
        if df_scores is not None:
            logging.info('asym 1 --> 2, diff=%s, remaining matches=%s, remaining candidates=%s', 0, len(index_set_matches), len(df_scores))
            ontomatch.evaluate.evaluate(df_scores, index_set_matches)
        df_scores = matcher.get_total_best_scores_2()
        if df_scores is not None:
            logging.info('asym 2 --> 1, diff=%s, remaining matches=%s, remaining candidates=%s', 0, len(index_set_matches), len(df_scores))
            ontomatch.evaluate.evaluate(df_scores, index_set_matches)

    df_scores = matcher.get_scores()
    logging.info('training set IS NOT EXCLUDED for evaluation, diff=%s, remaining matches=%s, remaining candidates=%s', 0, len(index_set_matches), len(df_scores))
    result = ontomatch.evaluate.evaluate(df_scores, index_set_matches)

    if minus_train_set is not None:

        # evaluate on train set only
        # TODO-AE 211216 add the matches outside candidate set to have a fair comparison concerning overfitting
        df_scores_train = df_scores.loc[minus_train_set.index].copy()
        index_set_matches_train = minus_train_set.index.intersection(index_set_matches)
        logging.info('evaluation on TRAINING SET ONLY, train set=%s, train set matches=%s', len(minus_train_set), len(index_set_matches_train))
        result = ontomatch.evaluate.evaluate(df_scores_train, index_set_matches_train)

        # evaluate on test set
        diff = df_scores.index.difference(minus_train_set.index)
        df_scores = df_scores.loc[diff].copy()
        len_matches = len(index_set_matches)
        index_set_matches_diff = index_set_matches.difference(minus_train_set.index)
        len_diff = len_matches - len(index_set_matches_diff)
        logging.info('training set IS EXCLUDED for evaluation, diff=%s, remaining matches=%s, remaining candidates=%s', len_diff, len(index_set_matches_diff), len(df_scores))
        result = ontomatch.evaluate.evaluate(df_scores, index_set_matches_diff)

    logging.info('post processing finished')
    return result
