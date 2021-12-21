import collections
import logging
import math
import os
import os.path
import pathlib
import random
import time

import nltk
from nltk.metrics.distance import jaro_similarity, edit_distance, jaro_winkler_similarity
import numpy as np
import pandas as pd
import sentence_transformers
from tqdm import tqdm

import ontomatch.blocking
import ontomatch.utils.util

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
    if v1 == 0. and v2 == 0.:
        return 0.
    return abs(v1 - v2) / max(abs(v1), abs(v2))

def dist_equal(v1, v2):
    if v1 is None or v2 is None:
        return
    return (0 if v1 == v2 else 1)

def create_dist_cosine_with_tfidf(n_max_idf):

    def dist_cosine_with_tfidf_internal(v1, v2):
        return dist_cosine_with_tfidf(v1, v2, n_max_idf)

    return dist_cosine_with_tfidf_internal

def dist_cosine_with_tfidf(v1, v2, n_max_idf=100):
    if not check_str(v1, v2):
        return None
    #df_index_tokens = ontomatch.blocking.TokenBasedPairIterator.df_index_tokens
    df_index_tokens = ontomatch.blocking.TokenBasedPairIterator.df_index_tokens_unpruned
    if df_index_tokens is None:
        raise ValueError('df_index_tokens was not created yet')
    return compare_strings_with_tfidf(v1, v2, n_max_idf, df_index_tokens, log=False)

def dist_cosine_binary(v1, v2):
    if not check_str(v1, v2):
        return None
    return compare_strings_binary(v1, v2)

def dist_cosine_embedding(v1, v2):
    if not check_str(v1, v2):
        return None
    embeddings1 = SentenceTransformerWrapper.model.encode(v1, convert_to_tensor=True, show_progress_bar=False)
    embeddings2 = SentenceTransformerWrapper.model.encode(v2, convert_to_tensor=True, show_progress_bar=False)
    pytorch_tensor = sentence_transformers.util.pytorch_cos_sim(embeddings1, embeddings2)
    # pytorch returns sometimes values such as 1.0000002
    # use min to avoid negative cosine distance
    cosine_sim = min(1, pytorch_tensor[0].numpy()[0])
    # negative cosine sim is mapped to 0
    cosine_distance = 1 - max(0, cosine_sim)
    return cosine_distance

class SentenceTransformerWrapper():
    model = sentence_transformers.SentenceTransformer('all-MiniLM-L6-v2')

def similarity_from_dist_fct(dist_fct, cut_off_mode='fixed', cut_off_value=1, decrease = 'linear'):
    # parameters to describe a monotone decreasing conversion fct c:[0, oo) --> [0,1] with c(0) = 1
    # cut_off_mode could also be 'max' or 'quantile' (with cut_off_value = percentage)
    # monotonicity could also by 1 - 1/x for (discrete values 1, 2, 3) or 1/exponential or ...
    # (fixed, 1, linear) --> score(v1, v2) = 1 - dist_fct(v1, v2)
    def similarity_from_dist_fct_internal(v1, v2):
        dist = dist_fct(v1, v2)
        if dist is not None:
            return 1 - (min(dist, cut_off_value) / cut_off_value)
        return None

    return similarity_from_dist_fct_internal

def sim_jaro_winkler(v1, v2):
    if not check_str(v1, v2):
        return None
    return jaro_winkler_similarity(v1, v2)

def create_similarity_functions_from_params(params_sim_fcts):

    sim_fcts = []

    for s_fct in params_sim_fcts:
        name = s_fct['name']
        maxidf = s_fct.get('maxidf')
        if (name == 'dist_cosine_with_tfidf') and (maxidf is not None):
            dist_fct = create_dist_cosine_with_tfidf(maxidf)
        else:
            dist_fct = globals()[name]

        cut_off_mode = s_fct.get('cut_off_mode')
        if cut_off_mode:

            cut_off_value = s_fct.get('cut_off_value', 1)
            decrease = s_fct.get('decrease', 'linear')
            sim_fct = similarity_from_dist_fct(dist_fct, cut_off_mode, cut_off_value, decrease)
        else:
            raise RuntimeError('configured similarity or distance function is not implemented', name)

        sim_fcts.append(sim_fct)

    return sim_fcts

def create_prop_prop_sim_triples_from_params(params_mapping):

    params_sim_fcts = params_mapping['similarity_functions']
    sim_fcts = create_similarity_functions_from_params(params_sim_fcts)

    triples = []
    params_triples = params_mapping['triples']
    for i, p in enumerate(params_triples):
        sim_fct_number = p['sim']
        pos = p.get('pos')
        if pos is None:
            pos = i
        triple = (p['prop1'], p['prop2'], sim_fcts[sim_fct_number], pos)
        triples.append(triple)

    return triples

def create_distance_functions_from_params(params_sim_fcts):
    dist_fcts = []
    for s_fct in params_sim_fcts:
         # name refers to a distance function not to a similarity function
        name = s_fct['name']
        dist_fct = globals()[name]
        dist_fcts.append(dist_fct)
    return dist_fcts

class ScoreManager():

    def __init__(self, data1: pd.DataFrame, data2: pd.DataFrame, pair_iterator, similarity_file):
        self.data1 = data1
        self.data2 = data2
        self.pair_iterator = pair_iterator
        self.similarity_file = similarity_file
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

    def get_column_names(self):
        columns = []
        for _, _, _, i in self.prop_prop_fct_tuples:
            sim_column = str(i)
            columns.append(sim_column)
        return columns

    def get_scores(self):
        return self.df_scores

    def get_max_scores_1(self):
        return self.df_max_scores_1

    def get_max_scores_2(self):
        return self.df_max_scores_2

    def add_prop_prop_fct_tuples_by_params(self, params_mapping):
        prop_prop_sim_tuples = create_prop_prop_sim_triples_from_params(params_mapping)
        for prop1, prop2, sim_fct, pos in prop_prop_sim_tuples:
            self.add_prop_prop_fct_tuples(prop1, prop2, sim_fct, pos)

    def add_prop_prop_fct_tuples(self, property1: str, property2: str, fct, pos=None):
        if property1 and (not property1 in self.data1.columns):
            raise RuntimeError('property1 not found in dataframe columns', property1)
        if property2 and (not property2 in self.data2.columns):
            raise RuntimeError('property2 not found in dataframe columns', property2)
        if pos is None:
            pos = len(self.prop_prop_fct_tuples)
        self.prop_prop_fct_tuples.append((property1, property2, fct, pos))

    @staticmethod
    def calculate_between_entities(entity1, entity2, prop_prop_fct_tuples):

        result = []
        for prop1, prop2, fct, _ in prop_prop_fct_tuples:
            v1 = entity1[prop1]
            v2 = entity2[prop2]
            value = fct(v1, v2)
            assert value is None or (value >= 0 and value <= 1)
            result.append(value)

        return result

    def calculate_similarities_between_datasets_for_pairs(self, candidate_pairs):
        logging.info('calculating similarity vectors, number of pairs=%s', len(candidate_pairs))
        count = 0
        rows = []
        for idx1, idx2 in tqdm(candidate_pairs):
            count += 1
            row1 = self.data1.loc[idx1]
            row2 = self.data2.loc[idx2]
            row = [idx1, idx2]
            scores = ScoreManager.calculate_between_entities(row1.to_dict(), row2.to_dict(), self.prop_prop_fct_tuples)
            row.extend(scores)
            rows.append(row)

        columns = ['idx_1', 'idx_2']
        columns.extend(self.get_column_names())

        result = pd.DataFrame(data=rows, columns=columns)
        result.set_index(['idx_1', 'idx_2'], inplace=True)
        logging.info('calculated similarity vectors=%s, score columns=%s', len(result), columns)
        return result

    def save_similarity_vectors(self, df_scores, file):
        path = os.path.dirname(file)
        if not os.path.exists(path):
            os.makedirs(path)
        new_file = file[:-4] + '_NEW_' + '_' + str(time.time()) + '.csv'
        logging.info('saving all similarity vectors to %s', new_file)
        df_scores.to_csv(new_file)

    def calculate_similarities_between_datasets(self):

        logging.info('similarity vector file=%s', self.similarity_file)
        candidate_pairs = self.pair_iterator.candidate_matching_pairs
        if self.similarity_file is None :
            # calculate similarities from scratch for all candidate pairs
            self.df_scores = self.calculate_similarities_between_datasets_for_pairs(candidate_pairs)
        elif not pathlib.Path(self.similarity_file).exists():
            # calculate similarities from scratch for all candidate pairs and store the result
            self.df_scores = self.calculate_similarities_between_datasets_for_pairs(candidate_pairs)
            self.save_similarity_vectors(self.df_scores, self.similarity_file)
        else:
            df_loaded = ontomatch.utils.util.read_csv(self.similarity_file)
            if 'pos_1' in [ str(c) for c in df_loaded.columns]:
                df_loaded.drop(columns=['pos_1', 'pos_2'], inplace=True)
                logging.info('dropped columns pos_1 and pos_2 from loaded similarity vectors')
            index_intersection = candidate_pairs.intersection(df_loaded.index)
            df_intersection = df_loaded.loc[index_intersection]
            logging.info('loaded similarity vectors, all=%s, intersection=%s', len(df_loaded), len(df_intersection))
            index_diff = candidate_pairs.difference(df_loaded.index)
            if len(index_diff) > 0:
                df_diff = self.calculate_similarities_between_datasets_for_pairs(candidate_pairs=index_diff)
                df_scores_tmp = pd.concat([df_intersection, df_diff])
                df_all = pd.concat([df_loaded, df_diff])
                logging.info('number of similarity vectors, all=%s, new=%s, df_scores=%s', len(df_all), len(df_diff), len(df_scores_tmp))
                self.save_similarity_vectors(df_all, self.similarity_file)
            else:
                df_scores_tmp = df_intersection.copy()
                logging.info('no new similarity vectors, all=%s, df_scores=%s', len(df_loaded), len(df_scores_tmp))
                columns = self.get_column_names()
                df_scores_tmp = df_scores_tmp[columns]

            self.df_scores = df_scores_tmp

        return self.df_scores

    def calculate_maximum_scores(self, symmetric=False, switch_to_min_of_distance=False):
        self.df_max_scores_1 = self.__calculate_maximum_scores(1, switch_to_min_of_distance)
        if symmetric:
            self.df_max_scores_2 = self.__calculate_maximum_scores(2, switch_to_min_of_distance)

    def __calculate_maximum_scores(self, dataset_id, switch_to_min_of_distance=False):

        logging.info('calculating maximum scores, dataset_id=%s, number of scores=%s', dataset_id, len(self.prop_prop_fct_tuples))

        if dataset_id == 1:
            idx_values = self.df_scores.index.get_level_values(0).unique()
            df_scores_tmp = self.df_scores
            index_column_name = 'idx_1'
            other_index_column_name = 'idx_2'
        else:
            idx_values = self.df_scores.index.get_level_values(1).unique()
            df_scores_tmp = self.df_scores.reorder_levels(['idx_2', 'idx_1'])
            index_column_name = 'idx_2'
            other_index_column_name = 'idx_1'

        logging.info('number of entities=%s', len(idx_values))

        columns = []
        str_column_prop = ''
        for prop1, prop2, _, pos in self.prop_prop_fct_tuples:
            columns.append(str(pos))
            str_column_prop += '\n' + str(pos) + ':' + prop1 + ' vs. ' + prop2

        result_rows = []
        count = 0

        for idx in tqdm(idx_values):
            result_row = {}
            cand = df_scores_tmp.loc[idx]
            count += len(cand)

            for c in columns:
                max_score = None
                for _, row in cand.iterrows():
                    score = row[c]
                    if not ((score is None) or np.isnan(score)):
                        if not switch_to_min_of_distance:
                            if max_score is None or score > max_score:
                                max_score = score
                        else:
                            if max_score is None or score < max_score:
                                max_score = score
                result_row.update({str(c) + '_max': max_score})

            result_row.update({index_column_name: idx})

            # use idx_2 as second index if all its column scores are maximum; otherwise use 'virtual'
            max_idx_2 = None
            for idx_2, row in cand.iterrows():
                max_idx_2 = idx_2
                for c in columns:
                    v1 = row[c]
                    v2 = result_row[str(c) + '_max']
                    if not ((pd.isna(v1) and pd.isna(v2)) or (v1 == v2)):
                        max_idx_2 = None
                        break
                if max_idx_2:
                    break
            if not max_idx_2:
                max_idx_2 = 'virtual'
            result_row.update({other_index_column_name: max_idx_2})

            result_rows.append(result_row)

        df_result = pd.DataFrame(result_rows)
        df_result.set_index([index_column_name, other_index_column_name], inplace=True)

        logging.info('calculated maximum scores, number of entities=%s, number of pairs=%s', len(df_result), count)
        logging.info('maximum scores statistics: %s\n%s', str_column_prop, df_result.describe())
        return df_result

def create_score_manager(srconto, tgtonto, params_blocking, params_mapping=None):
    it = ontomatch.blocking.create_iterator(srconto, tgtonto, params_blocking, use_position=False)
    dframe1 = ontomatch.blocking.TokenBasedPairIterator.df_src
    dframe2 = ontomatch.blocking.TokenBasedPairIterator.df_tgt
    if params_mapping:
        similarity_file = params_mapping.get('similarity_file')
    else:
        similarity_file = None
    manager = ScoreManager(dframe1, dframe2, it, similarity_file)
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
                manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)

    logging.info('added prop prop sim tuples, number=%s', len(manager.get_prop_prop_fct_tuples()))

    manager.calculate_similarities_between_datasets()

    manager.calculate_maximum_scores()
    df_max_scores_1 = manager.get_max_scores_1()

    logging.info('\nmax scores for dataset 1:\n------------------\n%s', df_max_scores_1)

    means = df_max_scores_1.describe().loc['mean']

    pos_sim_fcts = {}
    for i, s in enumerate(similarity_functions):
        pos_sim_fcts.update({s:i})

    rows = []
    for prop_prop_sim in manager.get_prop_prop_fct_tuples():
        prop1, prop2, s, pos = prop_prop_sim
        pos_sfct = pos_sim_fcts[s]
        key = str(pos) + '_max'
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

def get_tfidf(frequencies, n):
    tfidf = {}
    for t, freq in frequencies.items():
        max_inverse = max(1, n / freq)
        tfidf[t] = math.log(max_inverse)
    return tfidf

def calculate_dot_product(v1, v2):
    dot = 0
    tokens = set(v1.keys()).intersection(set(v2.keys()))
    for t in tokens:
        dot += v1[t] * v2[t]
    return dot

def calculate_norm(a):
    norm = 0
    for _, v in a.items():
        norm += math.pow(v, 2)
    return math.pow(norm, 0.5)

def get_frequencies(tokens, df_index_tokens):
    frequencies = {}
    for t in tokens:
        freq_sum = 0
        for c in ['count_1', 'count_2']:
            try:
                row = df_index_tokens.loc[t]
            except KeyError:
                continue

            freq = row[c]
            if freq and not np.isnan(freq):
                freq_sum += freq
        if freq_sum > 0:
            frequencies.update({t: freq_sum})
    return frequencies

def compare_strings_with_tfidf(s1, s2, n_max_idf, df_index_tokens, log=True):

    if not s1 and not s2:
        return 0
    if not s1 or not s2:
        return 1

    tokens1 = ontomatch.blocking.tokenize(s1)
    tokens2 = ontomatch.blocking.tokenize(s2)

    for t1 in tokens1.copy():
        for t2 in tokens2:
            if len(t1) > 3 and len(t2) > 3:
                edit_dist = nltk.edit_distance(t1, t2)
                if edit_dist == 1:
                    tokens1.remove(t1)
                    tokens1.append(t2)
                    break

    if df_index_tokens is not None:
        freq1 = get_frequencies(tokens1, df_index_tokens)
        freq2 = get_frequencies(tokens2, df_index_tokens)
        v1 = get_tfidf(freq1, n_max_idf)
        v2 = get_tfidf(freq2, n_max_idf)
    else:
        # binary frequency i.e. 1 is token occurs one ore more times
        v1 = { t:1 for t in tokens1 }
        v2 = { t:1 for t in tokens2 }

    if log:
        print(freq1)
        print(freq2)

    dot = calculate_dot_product(v1, v2)
    if dot == 0:
        return 1 - 0

    norm1 = calculate_norm(v1)
    norm2 = calculate_norm(v2)

    cosine_distance = 1 - round(dot / (norm1 * norm2), 4)

    if log:
        print(v1, v2, dot, norm1, norm2, cosine_distance)

    return cosine_distance

def compare_strings_binary(s1, s2):

    if not s1 and not s2:
        return 0
    if not s1 or not s2:
        return 1

    tokens1 = ontomatch.blocking.tokenize(s1)
    tokens2 = ontomatch.blocking.tokenize(s2)

    tokens1 = set(tokens1)
    tokens2 = set(tokens2)
    count_t1_t2_intersection = 0
    for t1 in tokens1.copy():
        for t2 in tokens2.copy():
            edit_dist = nltk.edit_distance(t1, t2)
            if (edit_dist == 0) or (len(t1) > 3 and len(t2) > 3 and edit_dist == 1):
                count_t1_t2_intersection += 1
                tokens1.remove(t1)
                tokens2.remove(t2)
                break

    norm1 = math.pow(len(tokens1) + count_t1_t2_intersection, 0.5)
    norm2 = math.pow(len(tokens2) + count_t1_t2_intersection, 0.5)
    cosine_distance = 1 - round( count_t1_t2_intersection / (norm1 * norm2), 4)
    return cosine_distance

class ScoringWeightIterator(collections.Iterable, collections.Sized):

    def __init__(self, number_weights, max_weight=10, sample_count=None):
        self.number_weights = number_weights
        self.max_weight = max_weight
        self.all_weight_arrays = []

        self.collect_weights([])

        if sample_count:
            self.weight_arrays = random.sample(self.all_weight_arrays, sample_count)
        else:
            self.weight_arrays = self.all_weight_arrays

        logging.info('finished preparing scoring weight iterator, weights=%s (out of %s)', len(self.weight_arrays), len(self.all_weight_arrays))


    def collect_weights(self, current_weight_array):

        current_sum = sum(current_weight_array)

        if len(current_weight_array) == self.number_weights - 1:
            w = self.max_weight - current_sum
            current_weight_array.append(w)
            normalized = [ cw/self.max_weight for cw in current_weight_array]
            self.all_weight_arrays.append(normalized)
            return

        for w in range(self.max_weight + 1 - current_sum):
            extended_copy = current_weight_array.copy()
            extended_copy.append(w)
            self.collect_weights(extended_copy)

    def __iter__(self):
        return iter(self.weight_arrays)

    def __len__(self):
        return len(self.weight_arrays)
