import logging
import os
import os.path
import time

import numpy as np
import pandas as pd
from sklearn.metrics.pairwise import laplacian_kernel
from tqdm import tqdm

import ontomatch.converter
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
        mode = params_mapping['mode']

        logging.info('starting InstanceMatcherBase with mode=%s', mode)

        if mode == 'auto':
            #TODO-AE 211028 auto maybe moved, e.g. as extra step in coordinator, configurable
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

        # perform blocking and create similarity vectors
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        params_post_processing = params['post_processing']
        self.start_base(srconto, tgtonto, params_blocking, params_mapping)
        df_scores = self.score_manager.get_scores()

        params_training = params['training']
        params_impution = params_training.get('impution')
        cross_validation = params_training['cross_validation']
        m_train_size = params_training['match_train_size']
        train_file = params_training['train_file']
        logging.info('training classifier for train_size=%s, train_file=%s', m_train_size, train_file)
        prop_columns = ontomatch.utils.util.get_prop_columns(df_scores)
        if train_file:
            df_split = ontomatch.utils.util.read_csv(train_file)
            if isinstance(m_train_size, str):
                split_column = m_train_size
            else:
                split_column = 'ml_phase_' + str(m_train_size)
            x_train, x_test, y_train, y_test = ontomatch.utils.util.split_df(df_split, df_scores, columns_x=prop_columns, column_ml_phase=split_column)
        else:
            evaluation_file = params_post_processing['evaluation_file']
            index_set_matches = ontomatch.evaluate.read_match_file_as_index_set(evaluation_file, linktypes = [1, 2, 3, 4, 5])
            _, x_train, x_test, y_train, y_test = ontomatch.utils.util.train_test_split(
                    df_scores, index_set_matches, train_size=m_train_size, columns_x=prop_columns)

        params_classification = params['classification']
        self.score_manager.df_scores = ontomatch.hpo.start(params_classification, cross_validation, params_impution,
                x_train, y_train, x_test, y_test, df_scores, prop_columns)

        if params_post_processing:
            hint = 'train_size=' + str(m_train_size)
            postprocess(params_post_processing, self, train_set=x_train, hint=hint)

class InstanceMatcherWithAutoCalibration(InstanceMatcherBase):

    def __init__(self):
        super().__init__()
        self.df_total_scores = None
        self.df_total_best_scores = None
        self.df_total_best_scores_1 = None
        self.df_total_best_scores_2 = None

    def get_scores(self):
        return self.df_total_scores

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
        perfect = params_model_specific.get('perfect')
        symmetric = params_model_specific['symmetric']
        delta = params_model_specific.get('delta')
        if delta is None:
            delta = 0.025
        purge_alpha = params_model_specific.get('purge_alpha')
        purge_majority = params_model_specific.get('purge_majority')
        if purge_majority is None:
            purge_majority = 0.
        threshold_ratio = params_model_specific.get('threshold_ratio')
        if threshold_ratio is None:
            threshold_ratio = 1.

        property_mapping = self.start_base(srconto, tgtonto, params_blocking, params_mapping)

        manager = self.score_manager

        match_file = None
        if params_post_processing:
            match_file = params_post_processing.get('evaluation_file')
            if match_file:
                matches_minus_fn = self.read_matches_minus_fn(match_file)

        if perfect:
            self.calculate_perfect_maximum_scores(matches_minus_fn)
        else:
            manager.calculate_maximum_scores(symmetric=symmetric)
            if symmetric and purge_alpha:
                # purging works only if both directions are considered for calculating max sim vectors
                manager.df_max_scores_1, manager.df_max_scores_2 = self.purge_low_max_scores(
                        manager.df_max_scores_1, manager.df_max_scores_2, manager.df_scores, purge_alpha, purge_majority)

        if match_file:
            self.evaluate_maximum_scores(matches_minus_fn)

        df_scores = manager.get_scores()
        df_max_scores = manager.get_max_scores_1()
        self.df_total_scores, self.df_total_best_scores_1 = self.calculate_auto_calibrated_total_scores(df_scores, df_max_scores, property_mapping, delta)
        self.df_total_best_scores = self.df_total_best_scores_1

        if symmetric:
            df_max_scores_2 = manager.get_max_scores_2()
            df_scores_2 = manager.get_scores()

            df_total_scores_2, self.df_total_best_scores_2 = self.calculate_auto_calibrated_total_scores(df_scores_2, df_max_scores_2, property_mapping, delta, dataset_id=2)
            self.df_total_scores = self.combine(self.df_total_scores, df_total_scores_2)
            mask = (self.df_total_scores['best'])
            self.df_total_best_scores = self.df_total_scores[mask].copy()

        logging.debug('total scores=%s, best total scores=%s', len(self.df_total_scores), len(self.df_total_best_scores))

        estimated_threshold = self.estimate_best_threshold(self.df_total_scores, delta, threshold_ratio, log=True)

        mask = ~(self.df_total_scores['best'])
        self.df_total_scores.at[mask, 'score'] = 0

        if params_post_processing:
            postprocess(params_post_processing, self, estimated_threshold=estimated_threshold)

        return self.df_total_scores, self.df_total_best_scores

    def combine(self, df_total_1, df_total_2):

        best_total_1 = df_total_1[df_total_1['best']].index
        best_total_2 = df_total_2[df_total_2['best']].index
        logging.info('combining total scores, total_1=%s, total_2=%s, best_total_1=%s, best_total_2=%s', len(df_total_1), len(df_total_2), len(best_total_1), len(best_total_2))

        df_combined = df_total_1[['score', 'best']].copy()
        for idx, row in tqdm(df_total_2.iterrows()):
            score_1 =  df_total_1.loc[idx, 'score']
            best_1 = df_total_1.loc[idx, 'best']
            #best_1 = (idx in best_total_1)
            score_2 = row['score']
            best_2 = row['best']
            #best_2 = (idx in best_total_2)
            df_combined.at[idx, 'score'] = max(score_1, score_2)
            df_combined.at[idx, 'best'] = (best_1 | best_2)

        return df_combined


    def read_matches_minus_fn(self, match_file):
        index_matches = ontomatch.evaluate.read_match_file_as_index_set(match_file, linktypes = [1, 2, 3, 4, 5])
        df_scores = self.score_manager.get_scores()
        return df_scores.index.intersection(index_matches)

    def calculate_perfect_maximum_scores(self, matches_minus_fn):
        df_tmp = self.score_manager.get_scores().loc[matches_minus_fn].copy()
        logging.warning('calculating PERFECT maximum scores=%s', len(df_tmp))
        columns_dict = {}
        for c in [ str(c) for c in df_tmp.columns]:
            columns_dict.update({ c: c + '_max'})
        logging.debug('columns before=%s', [ str(c) for c in df_tmp.columns])
        df_tmp.rename(columns=columns_dict, inplace=True)
        logging.debug('columns after=%s', [ str(c) for c in df_tmp.columns])
        self.score_manager.df_max_scores_1 = df_tmp.copy()
        logging.warning('PERFECT maximum scores statistics 1: \n%s', self.score_manager.df_max_scores_1.describe())

        logging.debug('max sim 2 index before=%s', df_tmp.index[0])
        df_tmp = df_tmp.reorder_levels(order=['idx_2', 'idx_1'])
        logging.debug('max sim 2 index after=%s', df_tmp.index[0])
        self.score_manager.df_max_scores_2 = df_tmp.copy()
        logging.warning('PERFECT maximum scores statistics 2: \n%s', self.score_manager.df_max_scores_2.describe())

    def evaluate_maximum_scores(self, matches_minus_fn):
        df_max_1 = self.score_manager.df_max_scores_1
        df_max_2 = self.score_manager.df_max_scores_2

        if df_max_2 is None:
            logging.warning('evaluate_maximum_scores skipped because symmetric=False')
            return

        logging.info('evaluating maximum scores: df_max_1=%s, df_max=2=%s', len(df_max_1), len(df_max_2))

        df_max_2 = df_max_2.reorder_levels(order=['idx_1', 'idx_2'])

        tuples_1_non_virtual = []
        for i, _ in df_max_1.iterrows():
            if i[1] != 'virtual':
                tuples_1_non_virtual.append((i[0], i[1]))
        index_1_non_virtual = pd.MultiIndex.from_tuples(tuples_1_non_virtual, names=['idx_1', 'idx_2'])

        tuples_2_non_virtual = []
        for i, _ in df_max_2.iterrows():
            if i[0] != 'virtual':
                tuples_2_non_virtual.append((i[0], i[1]))
        index_2_non_virtual = pd.MultiIndex.from_tuples(tuples_2_non_virtual, names=['idx_1', 'idx_2'])

        index_max_1_2 = df_max_1.index.union(df_max_2.index)
        index_non_virtual = index_1_non_virtual.union(index_2_non_virtual)
        index_virtual = index_max_1_2.difference(index_non_virtual)
        rate = None
        if len(index_virtual) > 0:
            rate = round(len(index_non_virtual) / len(index_virtual), 2)
        logging.info('non-virtual 1=%s, non-virtual 2=%s, all 1+2=%s, non_virtual 1+2=%s, virtual 1+2=%s, rate=%s',
                len(index_1_non_virtual), len(index_2_non_virtual), len(index_max_1_2), len(index_non_virtual), len(index_virtual), rate)

        TP = len(index_non_virtual.intersection(matches_minus_fn))
        pred_matches = len(index_non_virtual)
        FP = pred_matches - TP
        FN = len(matches_minus_fn.difference(index_non_virtual))

        precision = round(TP / (TP + FP), 5)
        recall = round(TP / (TP + FN), 5)
        if precision == 0 or recall == 0:
            f1 = 0
        else:
            f1 = round(2 * precision * recall / (precision + recall), 5)
        logging.info('evaluated maximum scores: MAXSIM f1=%s, p=%s, r=%s, TP=%s, FP=%s, FN=%s', f1, precision, recall, TP, FP, FN)

    def purge_low_max_scores(self, df_max_1, df_max_2, df_scores, purge_alpha, purge_majority, dataset_id=1):
        logging.info('purging low max scores vectors, original max scores 1=%s, original max scores 2=%s', len(df_max_1), len(df_max_2))
        c_idx_1 = 'idx_1'
        c_idx_2 = 'idx_2'
        if dataset_id != 1:
            c_idx_1 = 'idx_2'
            c_idx_2 = 'idx_1'
            df_scores = df_scores.reorder_levels(order=['idx_2', 'idx_1'])

        columns = ontomatch.utils.util.get_prop_columns(df_max_1)
        logging.info('purging for columns=%s', columns)

        df_max_1 = df_max_1.reset_index(c_idx_2)
        df_max_2 = df_max_2.reset_index(c_idx_1)

        df_max_1['purge'] = False
        df_max_2['purge'] = False
        idx_1_unique = df_max_1.index.unique()

        logging.debug('idx_1_unique=%s', len(idx_1_unique))

        for idx_1 in tqdm(idx_1_unique):
            row_1 = df_max_1.loc[idx_1]

            for idx_2 in df_scores.loc[idx_1].index:
                try:
                    row_2 = df_max_2.loc[idx_2]
                except KeyError:
                    continue

                count_comparisons = 0
                count_purge_1 = 0
                count_purge_2 = 0
                for col in columns:
                    max_score_1 = row_1.loc[col]
                    max_score_2 = row_2.loc[col]
                    if max_score_1 is None or max_score_2 is None:
                        continue
                    count_comparisons += 1
                    if max_score_1 <= max_score_2:
                        if max_score_1 <= purge_alpha * max_score_2:
                            #df_max_1.at[idx_1, 'purge'] = True
                            count_purge_1 += 1
                    else:
                        if max_score_2 <= purge_alpha * max_score_1:
                            #df_max_2.at[idx_2, 'purge'] = True
                            count_purge_2 += 1

                if (count_purge_1 > count_purge_2) and (count_purge_1 >= purge_majority * count_comparisons):
                    df_max_1.at[idx_1, 'purge'] = True
                elif (count_purge_2 > count_purge_1) and (count_purge_2 >= purge_majority * count_comparisons):
                    df_max_2.at[idx_2, 'purge'] = True

        logging.info('marked for purging from max scores 1=%s, from max scores 2=%s', len(df_max_1[df_max_1['purge']]), len(df_max_2[df_max_2['purge']]))

        df_max_1 = df_max_1[~df_max_1['purge']].copy()
        df_max_1 = df_max_1.reset_index(c_idx_1)
        df_max_1.set_index([c_idx_1, c_idx_2], inplace=True)
        df_max_1.drop(columns='purge', inplace=True)
        logging.info('maximum scores statistics after purging, max scores 1: \n%s', df_max_1.describe())

        df_max_2 = df_max_2[~df_max_2['purge']].copy()
        df_max_2 = df_max_2.reset_index(c_idx_2)
        df_max_2.set_index([c_idx_2, c_idx_1], inplace=True)
        df_max_2.drop(columns='purge', inplace=True)
        logging.info('maximum scores statistics after purging, max scores 2: \n%s', df_max_2.describe())

        if dataset_id != 1:
            df_scores = df_scores.reorder_levels(order=['idx_1', 'idx_2'])

        logging.info('purging finished, max scores 1=%s, max scores 2=%s', len(df_max_1), len(df_max_2))

        return df_max_1, df_max_2

    def calculate_auto_calibrated_total_scores_for_index(self, df_scores, sliding_counts, property_mapping, idx_1, skip_column_number = 1):
        best_score = 0
        best_pos = None
        total_score_rows = []

        for pos, (idx_2, row) in enumerate(df_scores.loc[idx_1].iterrows()):
            score = 0
            number_columns = len(property_mapping)
            prop_score = {}
            for propmap in property_mapping:
                c_max = propmap['key']
                c = c_max.split('_')[0]
                value = row[c]
                if not (value is None or type(value) is str or np.isnan(value)):
                    count_m = sliding_counts[c_max](value)
                    count_m_plus_n = sliding_counts[c](value)
                    if count_m == 0:
                        column_score = 0
                    elif count_m_plus_n == 0:
                        column_score = 1
                    else:
                        column_score = count_m / count_m_plus_n
                        if column_score > 1:
                            # this can happen due to small deviations, in particular if using embedding cosine distance
                            logging.debug('column score exceeds 1, %s', column_score)
                            column_score = 1.

                    score += column_score
                    prop_score.update({c: column_score})

                else:
                    number_columns = number_columns - 1

            if number_columns <= skip_column_number:
                score = 0.
                logging.debug('score = 0 since number columns=%s, idx_1=%s, idx_2=%s', number_columns, idx_1, idx_2)
            else:
                score = score / number_columns

            total_score_row = {
                'idx_1': idx_1,
                'idx_2': idx_2,
                'score': score,
                'best': False,
            }

            total_score_row.update(prop_score)

            total_score_rows.append(total_score_row)

            if score > best_score or best_pos is None:
                best_score = score
                best_pos = [pos]
            elif score == best_score and (best_pos is not None):
                best_pos.append(pos)

        for pos in best_pos:
            total_score_rows[pos]['best'] = True

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
            scount = InstanceMatcherWithAutoCalibration.sliding_count_fast(c_max, series, delta)
            sliding_counts[c_max] = scount

            c = c_max.split('_')[0]
            series = df_scores[c]
            scount = InstanceMatcherWithAutoCalibration.sliding_count_fast(c, series, delta)
            sliding_counts[c] = scount

        rows = []

        # configuring 'purging' and 'perfect' reduces the index pairs in df_max_scores
        # thus, don't iterate over df_max_scores but use unique first level values of df_scores instead
        #for idx, _ in tqdm(df_max_scores.iterrows()):
        #    idx_1 = idx[0]
        first_level_values = df_scores.index.get_level_values(0)
        logging.debug('first level values=%s, unique=%s', len(first_level_values), len(first_level_values.unique()))
        for idx_1 in tqdm(first_level_values.unique()):
            skip_column_number = 0
            total_score_rows = self.calculate_auto_calibrated_total_scores_for_index(df_scores, sliding_counts, property_mapping, idx_1, skip_column_number = skip_column_number)
            rows.extend(total_score_rows)

        df_total_scores = pd.DataFrame(rows)

        if dataset_id == 2:
            df_total_scores = df_total_scores.rename(columns={'idx_1': 'idx_2', 'idx_2': 'idx_1'})
            logging.info('reordered index and position names of df_total scores since dataset_id=%s', dataset_id)

        df_total_scores.set_index(['idx_1', 'idx_2'], inplace=True)
        mask = (df_total_scores['best'] == True)
        df_total_best_scores = df_total_scores[mask]

        logging.info('calculated auto calibrated total scores, dataset_id=%s, total_scores=%s, total best scores=%s',
                dataset_id, len(df_total_scores), len(df_total_best_scores))

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

    def estimate_best_threshold(self, df_total, delta, threshold_ratio, log=False):

        logging.info('estimating best threshold')

        mask = ~df_total['best']
        df_nonbest_scores = df_total[mask]['score']
        mask = df_total['best']
        df_best_scores = df_total[mask]['score'].sort_values(ascending=False)

        x_list = [ round(x, 5) for x in np.arange(0, 1.0001, delta)]
        ratios = []
        counts_best = []
        counts_nonbest = []

        # we use a sliding count here
        # sometimes there is only a poor statistics for thresholds from x_list close to zero
        # thus, a threshold is accepted only if three subsequent ratios are above 1.
        best_pos = -1
        count_above_one = 0
        delta = delta / 2
        for i, x in enumerate(x_list):
            mask = (df_best_scores >= x - delta) & (df_best_scores <= x + delta)
            count_best = len(df_best_scores[mask])
            mask = (df_nonbest_scores >= x - delta) & (df_nonbest_scores <= x + delta)
            count_nonbest = len(df_nonbest_scores[mask])
            if count_best == 0:
                ratio = 0
            elif count_nonbest == 0:
                ratio = count_best
            else:
                ratio = count_best / count_nonbest
            ratios.append(ratio)
            counts_best.append(count_best)
            counts_nonbest.append(count_nonbest)
            if count_above_one < 3:
                if ratio < threshold_ratio:
                    count_above_one = 0
                    best_pos = -1
                else:
                    count_above_one += 1
                    if best_pos == -1:
                        best_pos = i

        index_threshold = min(len(x_list) - 1, best_pos)
        best_threshold = x_list[index_threshold]

        if log:
            logging.debug('x_list=%s', x_list)
            logging.debug('ratios=%s', ratios)
            logging.debug('counts_best=%s', counts_best)
            logging.debug('counts_nonbest=%s', counts_nonbest)

        logging.info('estimated best threshold=%s, best pos=%s', best_threshold, best_pos)

        return best_threshold

class InstanceMatcherWithScoringWeights(InstanceMatcherBase):

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
            logging.error('total_score is missing, index=%s, row=%s', i, row)
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

def dump(dump_dir, matcher):
    dir_name = dump_dir + '_' + str(time.time())
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

def evaluate(match_file, test_file, matcher, train_set=None, hint='', estimated_threshold=None):
    index_matches = ontomatch.evaluate.read_match_file_as_index_set(match_file, linktypes = [1, 2, 3, 4, 5])
    df_scores = matcher.get_scores()
    if isinstance(matcher, ontomatch.instancematching.InstanceMatcherWithAutoCalibration):

        logging.info('test_file=%s', test_file)
        if test_file:
            df_split = ontomatch.utils.util.read_csv(test_file)
            prop_columns = ontomatch.utils.util.get_prop_columns(df_scores)
            for c in df_split.columns:
                c = str(c)
                if c.startswith('ml_phase') and not 'ratio' in c:
                #if c.startswith('ml_phase_0.1'):
                    x_train, x_test, _, _ = ontomatch.utils.util.split_df(df_split, df_scores, prop_columns, column_ml_phase=c)
                    index_train_set = x_train.index if x_train is not None else None
                    index_test_set = x_test.index
                    hint = c
                    ontomatch.evaluate.evaluate_on_train_test_split(df_scores, index_train_set, index_test_set, index_matches, hint, estimated_threshold)
        else:
            hint = 'test on entire candidate set'
            index_test_set = df_scores.index
            ontomatch.evaluate.evaluate_on_train_test_split(df_scores, None, index_test_set, index_matches, hint, estimated_threshold)
    else:
        if train_set is None:
            index_train_set = None
            index_test_set = df_scores.index
        else:
            index_train_set = train_set.index
            index_test_set = df_scores.index.difference(index_train_set)
        ontomatch.evaluate.evaluate_on_train_test_split(df_scores, index_train_set, index_test_set, index_matches, hint)

def link(link_file, matcher, estimated_threshold):
    logging.info('linking predicted matches')
    df_src = matcher.score_manager.get_data1()
    df_tgt = matcher.score_manager.get_data2()
    df_scores = matcher.get_scores()
    mask = (df_scores['score'] >= estimated_threshold)
    predicted_matches =  df_scores[mask].index
    iri_pairs = []
    for idx_1, idx_2 in predicted_matches:
        iri_src = df_src.loc[idx_1]['iri']
        iri_tgt = df_tgt.loc[idx_2]['iri']
        iri_pairs.append((iri_src, iri_tgt))
    ontomatch.converter.link_entity_pairs(link_file, iri_pairs)
    logging.info('linked predicted matches=%s, link_file=%s', len(iri_pairs), link_file)

def postprocess(params_post_processing, matcher, train_set=None, hint='', estimated_threshold=None):
    logging.info('post processing')
    dump_dir = params_post_processing.get('dump')
    if dump_dir:
        dump(dump_dir, matcher)
    match_file = params_post_processing.get('evaluation_file')
    if match_file:
        test_file = params_post_processing.get('test_file')
        evaluate(match_file, test_file, matcher, train_set, hint, estimated_threshold)
    link_file = params_post_processing.get('link_file')
    if link_file:
        if not isinstance(matcher, ontomatch.instancematching.InstanceMatcherWithAutoCalibration):
            logging.warn('linking entities is only possible for instance matching with AutoCal, link_file=%s', link_file)
        else:
            link(link_file, matcher, estimated_threshold)

    logging.info('post processing finished')
