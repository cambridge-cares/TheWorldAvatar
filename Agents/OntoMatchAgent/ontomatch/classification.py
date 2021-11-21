
import logging
import math
import random
import sys

import numpy as np
import pandas as pd
import sklearn
import sklearn.ensemble
import sklearn.metrics.pairwise
import sklearn.model_selection
import sklearn.svm
from tqdm import tqdm

import ontomatch.blocking
import ontomatch.coordinator
import ontomatch.scoring
import ontomatch.utils.util

class TrainTestGenerator():

    @staticmethod
    def generate_sim_vectors_for_matches_and_split(params, tgt_file, train_sizes):
        match_file = params['post_processing']['evaluation_file']
        #dframe = pd.read_csv(match_file, index_col=['idx_1', 'idx_2'])
        dframe = ontomatch.utils.util.read_csv(match_file)

        # split into train and test set for different sizes
        for train_size in train_sizes:
            ml_phase_column = 'ml_phase_' + str(train_size)
            dframe = Utils.create_train_test_split(dframe, train_size, save_to=None, ml_phase_column=ml_phase_column)

        # read data
        df1, df2, srconto, tgtonto = Utils.create_dataframes(params)
        # create iterator and TFIDF weights
        params_blocking = params['blocking']
        ontomatch.blocking.create_iterator(srconto, tgtonto, params_blocking)
        # add similarity vectors
        params_mapping = params['mapping']
        index_set = dframe.index
        df_scores, _, _ = calculate_scores_for_index_set(df1, df2, params_mapping, index_set)
        df_matches = pd.concat([dframe, df_scores], axis = 1)

        # set label y to 1 for class 'match' (0 means 'nonmatch')
        df_matches['y'] = 1
        if tgt_file:
            df_matches.to_csv(tgt_file)
        return df_matches

    @staticmethod
    def random_nonmatches_from_file_and_split(scores_file, tgt_file, match_train_test_file, nonmatch_match_ratio_for_training=1):

        df_scores = pd.read_csv(scores_file, index_col=['idx_1', 'idx_2'])
        df_matches = pd.read_csv(match_train_test_file, index_col=['idx_1', 'idx_2'])

        # create nonmatches from all blocked candidate pairs in df_scores minus df_matches
        diff = df_scores.index.difference(df_matches.index)
        df_nonmatches = df_scores.loc[diff].copy()
        # set label y to 0 for class 'nonmatch' (1 means 'match')
        df_nonmatches['y'] = 0

        # for each train-test-split ration in df_matches, choose nonmatches for training randomly from all nonmatches
        # all other nonmatches are considered for testing
        split_columns = []
        for c in df_matches.columns:
            if c.startswith('ml_phase'):
                split_columns.append(str(c))

        for c in split_columns:
            df_nonmatches[c] = 'test'
            len_train = len(df_matches[df_matches[c] == 'train'])
            train_size = int(nonmatch_match_ratio_for_training * len_train)
            nonmatches_train = Utils.sample_without_replacement(diff, number=train_size)
            df_nonmatches.loc[nonmatches_train, c] = 'train'

        assert len(df_nonmatches.index.intersection(df_matches.index)) == 0

        if tgt_file:
            df_nonmatches.to_csv(tgt_file)

        return df_nonmatches

    @staticmethod
    def train_test_split(match_file, nonmatch_file, column_ml_phase, prop_columns=None):
        logging.info('splitting, match=%s, nonmatch=%s, ml_phase=%s, columns=%s',
            match_file, nonmatch_file, column_ml_phase, prop_columns)

        keep_columns = prop_columns.copy()
        keep_columns.extend(['y', column_ml_phase])

        df_matches = pd.read_csv(match_file, index_col=['idx_1', 'idx_2'])
        df_matches = df_matches[keep_columns].copy()

        df_nonmatches = pd.read_csv(nonmatch_file, index_col=['idx_1', 'idx_2'])
        df_nonmatches = df_nonmatches[keep_columns].copy()

        dframe = pd.concat([df_matches, df_nonmatches])
        mask = (dframe[column_ml_phase] == 'train')
        x_train = dframe[mask][prop_columns].copy()
        y_train = dframe[mask]['y'].copy()
        mask = (dframe[column_ml_phase] == 'test')
        x_test = dframe[mask][prop_columns].copy()
        y_test = dframe[mask]['y'].copy()

        logging.info('x_train=%s, y_train=%s, x_test=%s, y_test=%s', len(x_train), len(y_train), len(x_test), len(y_test))
        return x_train, x_test, y_train, y_test

class Utils():

    @staticmethod
    def create_dataframes(params):
        srcaddr = params['dataset']['src'],
        tgtaddr = params['dataset']['tgt'],
        srconto = ontomatch.utils.util.load_ontology(srcaddr, blackboard=False)
        tgtonto = ontomatch.utils.util.load_ontology(tgtaddr, blackboard=False)

        df1 = ontomatch.blocking.create_dataframe_from_ontology(srconto)
        df2 = ontomatch.blocking.create_dataframe_from_ontology(tgtonto)
        return df1, df2, srconto, tgtonto

    @staticmethod
    def sample_without_replacement(multi_index_set, number):
        array = multi_index_set.to_list()
        result = random.sample(array, number)
        logging.info('number of samples=%s', len(result))
        return result

    @staticmethod
    def get_random_nonmatches(df1, df2, match_index, nonmatch_number):

        result = set()

        idx1_set = df1.index.get_level_values(0).unique()
        len1 = len(idx1_set)
        idx2_set = df2.index.get_level_values(0).unique()
        len2 = len(idx2_set)

        while len(result) < nonmatch_number:
            r1 = random.randint(0, len1-1)
            idx1 = idx1_set[r1]
            r2 = random.randint(0, len2-1)
            idx2 = idx2_set[r2]
            new_index = (idx1, idx2)
            if not new_index in match_index:
                result.add(new_index)

        logging.info('number of random non-matches=%s', len(result))

        return result

    @staticmethod
    def create_train_test_split(dframe, train_size, stratify_y_column=None, save_to=None, ml_phase_column='ml_phase'):
        if isinstance(dframe,str):
            logging.info('reading data from %s', dframe)
            dframe = pd.read_csv(dframe, index_col=['idx_1', 'idx_2'])
        else:
            dframe = dframe.copy()
        if stratify_y_column:
            y = dframe[stratify_y_column]
            df_train, df_test = sklearn.model_selection.train_test_split(dframe, train_size=train_size, shuffle=True, stratify=y)
        else:
            df_train, df_test = sklearn.model_selection.train_test_split(dframe, train_size=train_size, shuffle=True)
        dframe[ml_phase_column] = ''
        dframe.at[df_train.index, ml_phase_column] = 'train'
        dframe.at[df_test.index, ml_phase_column] = 'test'
        if save_to:
            dframe.to_csv(save_to, index=True)
        return dframe

def get_pair_iterator_with_position(index_set, df1, df2):
    pair_positions = set()
    for idx_1, idx_2 in tqdm(index_set):
        pos1 = df1.loc[idx_1]['pos']
        pos2 = df2.loc[idx_2]['pos']
        pair_positions.add((pos1, pos2))
    return pair_positions

def calculate_scores_for_index_set(df1, df2, params_mapping, index_set):
    pair_iterator = get_pair_iterator_with_position(index_set, df1, df2)
    score_manager = ontomatch.scoring.ScoreManager(df1, df2, pair_iterator=pair_iterator)
    score_manager.add_prop_prop_fct_tuples_by_params(params_mapping)
    df_scores = score_manager.calculate_similarities_between_datasets()
    df_max_scores_1, df_max_scores_2 = score_manager.calculate_maximum_scores()
    return df_scores, df_max_scores_1, df_max_scores_2

def p_norm(a, p):
    norm = 0
    for v in a:
        norm += math.pow(v, p)
    return math.pow(norm, 1/p)

def filter_scores_for_index_set(df_scores, number=None, rate=None, mode='random', center=0, p=2, radius=1):
    # mode='random' or 'max' or 'min'
    if number is None:
        number = int(len(df_scores) * rate)
    columns = ontomatch.utils.util.get_prop_columns(df_scores)
    if center == 0:
        if p == 'max':
            dist_fct = lambda x : np.abs(x[columns]).max()
        else:
            dist_fct = lambda x : p_norm(x[columns], p)
    else:
        center_point = np.array([center]*len(columns))
        if p == 'max':
            dist_fct = lambda x : np.abs(np.array(x[columns]) - center_point).max()
        else:
            dist_fct = lambda x : p_norm(np.array(x[columns]) - center_point, p)

    df_scores['distance'] = -1
    df_scores['distance'] = df_scores.apply(dist_fct, axis='columns')

    before = len(df_scores)
    if radius:
        mask = (df_scores['distance'] <= radius)
        df_scores = df_scores[mask]
    if mode == 'random':
        index_set = get_random_index_set(df_scores, number)
        df_scores = df_scores.loc[index_set]
    elif mode == 'max':
        df_scores = df_scores.nlargest(number, 'distance')
    elif mode == 'min':
        df_scores = df_scores.nsmallest(number, 'distance')

    #df_scores.drop(columns='distance', inplace=True)

    logging.info('filtered samples, before=%s, now=%s', before, len(df_scores))
    return df_scores

def get_random_index_set(dframe, number):
    result = set()
    size = len(dframe)
    if number > size/2:
        raise ValueError('number too large for selection from index set', number, size)
    while len(result) < number:
        rint = random.randint(0, size-1)
        idx = dframe.index[rint]
        result.add(idx)
    return result



def select_seeds_for_ground_truth(df1, df2, match_index, params_mapping, split=[.8, .2], missing_value=None):

    # calculate similarities for matches
    df_scores_matches, _, _ = calculate_scores_for_index_set(df1, df2, params_mapping, match_index)

    if missing_value is None:
        # remove matching pairs with any missing score
        df_scores_matches.dropna(axis='index', inplace=True)

    split_sum = sum(split)
    if split_sum < 1.:
        df_scores_matches, _ = sklearn.model_selection.train_test_split(df_scores_matches, train_size=split_sum, shuffle=True)

    # select as many random nonmatches as remaining matches
    if missing_value is None:
        # don't choose random pairs of entities with missing values
        df1_tmp = df1.dropna(axis='index')
        df2_tmp = df2.dropna(axis='index')
    else:
        df1_tmp = df1
        df2_tmp = df2
    nonmatch_index = Utils.get_random_nonmatches(df1_tmp, df2_tmp, match_index=match_index, nonmatch_number=len(df_scores_matches))

    # calculate similarities for nonmatches
    df_scores_nonmatches, _, _ = calculate_scores_for_index_set(df1, df2, params_mapping, nonmatch_index)

    # join matches and nonmatches
    # label = 0 / 1 denotes nonmatch / match
    df_scores_matches['label'] = 1
    df_scores_nonmatches['label'] = 0
    df_joint = pd.concat([df_scores_matches, df_scores_nonmatches])

    # split joint matches into training and test set
    split = [ s/split_sum for s in split]
    labels = df_joint['label']
    df_joint.drop(labels='label', axis='columns', inplace=True)
    logging.debug('matches=%s, nonmatches=%s, total=%s', len(df_scores_matches), len(df_scores_nonmatches), len(df_joint))
    df_train, df_test, labels_train, labels_test = sklearn.model_selection.train_test_split(df_joint, labels, train_size=split[0], shuffle=True, stratify=labels)
    logging.debug('splitted into X train=%s, X test=%s, y train=%s, y test=%s according to split=%s',
            len(df_train), len(df_test), len(labels_train), len(labels_test), split)

    return df_train, df_test, labels_train, labels_test

def clean(dframe):
    df_cleaned = dframe.copy()
    frame_columns = [ str(c) for c in df_cleaned.columns]
    if 'pos_1' in frame_columns:
        df_cleaned.drop(columns=['pos_1', 'pos_2'], inplace=True)

    columns_dict = {}
    frame_columns = [ c for c in df_cleaned.columns]
    for c in frame_columns:
        c_new = c
        if isinstance(c, int):
            # TODO-AE 211110 change from int to str for column names - HACK
            c_new = str(c)
        elif c.endswith('_max'):
            c_new = c.split('_')[0]
        columns_dict.update({c: c_new})
    df_cleaned.rename(columns=columns_dict, inplace=True)

    #TODO-AE 2111010 fill nan values with average values or 0. or ...
    before = len(dframe)
    df_cleaned.dropna(inplace=True)
    logging.info('dropping nan columns, before=%s, now=%s', before, len(df_cleaned))
    return df_cleaned

def select_seeds_for_max_scores(df_max_scores, df_scores):

    df_max_scores.dropna(inplace=True)
    df_max_scores['label'] = 1 # match

    diff = df_scores.index.difference(df_max_scores.index)
    df_scores = df_scores.loc[diff]
    df_scores.dropna(inplace=True)
    size = len(df_max_scores)
    df_scores, _ = sklearn.model_selection.train_test_split(df_scores, train_size=size, shuffle=True)
    df_scores = df_scores.copy()
    df_scores['label'] = 0 # nonmatch

    log_columns = [ (str(c), type(c)) for c in df_max_scores.columns]
    logging.info('columns for max scores=%s', log_columns)
    log_columns = [ (str(c), type(c)) for c in df_scores.columns]
    logging.info('columns for scores=%s', log_columns)

    df_samples = pd.concat([df_max_scores, df_scores])
    labels = df_samples['label']
    df_samples.drop(labels='label', axis='columns', inplace=True)
    logging.debug('max scores=%s, non max scores=%s, total=%s', len(df_max_scores), len(df_scores), len(df_samples))

    df_train, df_test, labels_train, labels_test = sklearn.model_selection.train_test_split(df_samples, labels, train_size=0.8, shuffle=True, stratify=labels)

    logging.debug('X train=%s, X test=%s, y train=%s, y test=%s', len(df_train), len(df_test), len(labels_train), len(labels_test))
    return df_train, df_test, labels_train, labels_test

def store_nonmatch_file(df1, df2, params_mapping, match_index, nonmatch_file, nonmatch_number):
    nonmatch_index = Utils.get_random_nonmatches(df1, df2, match_index, nonmatch_number)
    df_scores_nonmatches, _, _ = calculate_scores_for_index_set(df1, df2, params_mapping, nonmatch_index)
    df_scores_nonmatches.to_csv(nonmatch_file, index=True)

def select_samples(df1, df2, params_mapping, match_index, nonmatch_file=None, nonmatch_number=-1):
    if nonmatch_file:
        df_scores_nonmatches = pd.read_csv(nonmatch_file, index_col=['idx_1', 'idx_2'])
    else:
        nonmatch_index = Utils.get_random_nonmatches(df1, df2, match_index, nonmatch_number)
        df_scores_nonmatches, _, _ = calculate_scores_for_index_set(df1, df2, params_mapping, nonmatch_index)

    df_scores_nonmatches = clean(df_scores_nonmatches)
    df_scores_nonmatches_filtered = filter_scores_for_index_set(df_scores_nonmatches, number=10, mode='random', center=0, p=2, radius=1)


def hpo_svm(x, y, params_hpo, crossvalidation):

    # set probability=True to get confidence scores after training when using SVC for prediction
    model = sklearn.svm.SVC( probability=True)

    hpo_model = sklearn.model_selection.GridSearchCV(model, params_hpo, cv = crossvalidation)
    logging.info('tuning hyperparameters for SVM')
    hpo_model.fit(x, y)
    logging.info('tuned hyperparameters for SVM')

    return hpo_model

def calculate_confidence_scores_from_hpo_svm(params_classification, df_train, labels_train, df_test, labels_test, df_scores_cleaned):

    params_hpo = {}
    cross_validation = None
    for key, value in params_classification.items():
        if key == 'cross_validation':
            cross_validation = value
            continue
        if isinstance(value, list):
            params_hpo[key] = value
        else:
            params_hpo[key] = [value]
    logging.info('cross_validation=%s, params_hpo=%s', cross_validation, params_hpo)

    model  = hpo_svm(df_train, labels_train, params_hpo, crossvalidation=cross_validation)
    train_score = model.score(df_train, labels_train)
    test_score = model.score(df_test, labels_test)
    logging.info('SVC mean acurray: train_score=%s, test_score=%s', train_score, test_score)

    logging.info('predicting confidence scores')
    # http://scikit-learn.sourceforge.net/stable/modules/generated/sklearn.svm.SVC.html
    # Returns the probability of the sample for each class in the model.
    # The columns correspond to the classes in sorted order, as they appear in the attribute classes_.
    pos_match = 1 if model.classes_[1] == 1 else 0
    logging.info('label order for confidence scores=%s (0=nonmatch, 1=match), pos_match=%s', model.classes_, pos_match)
    assert pos_match == 1
    confidence_scores = model.predict_proba(df_scores_cleaned)
    logging.info('len df_scores_cleaned=%s, confidence_scores=%s', len(df_scores_cleaned), len(confidence_scores))
    df_scores_cleaned['score'] = 0.
    for row_pos, (idx, _) in enumerate(df_scores_cleaned.iterrows()):
        score = confidence_scores[row_pos][pos_match]
        df_scores_cleaned.at[idx, 'score'] = score

    return df_scores_cleaned

def start_M():

    # KWL
    #tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_DEU_M_ground_truth_tfidf.csv'
    #config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/power_plant_DEU/conf_power_plant_DEU_xgb_geo.json'
    # DUKES
    #tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_GBR_M_ground_truth_tfidf.csv'
    #config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/power_plant_GBR/conf_power_plant_GBR_xgb_geo.json'
    # bibliography
    tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/bibliography_M_ground_truth_tfidf.csv'
    config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/bibliography/conf_bibliography_xgb_1.json'
    # product
    #tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/product_M_ground_truth_tfidf.csv'
    #config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/product/conf_product_xgb_1.json'

    sys.argv.extend(['--config', config_file])
    params, _ = ontomatch.utils.util.init()

    train_sizes = [0.01, 0.02, 0.05, 0.1, 0.2]
    TrainTestGenerator.generate_sim_vectors_for_matches_and_split(params, tgt_file, train_sizes=train_sizes)

def start_N_from_blocks():

    # KWL
    scores_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/power_plant_DEU/scores_geo/scores.csv'
    tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_DEU_N_random_blocking_tfidf.csv'
    match_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_DEU_M_ground_truth_tfidf.csv'
    config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/power_plant_DEU/conf_power_plant_DEU_xgb_geo.json'
    # DUKES
    #scores_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/power_plant_GBR/scores_geo/scores.csv'
    #tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_GBR_N_random_blocking_tfidf.csv'
    #match_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_GBR_M_ground_truth_tfidf.csv'
    #config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/power_plant_GBR/conf_power_plant_GBR_xgb_geo.json'
    # bibliography
    #scores_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/bibliography/scores_1/scores.csv'
    #tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/bibliography_N_random_blocking_tfidf.csv'
    #match_file = 'C:/my/tmp/ontomatch/20211118_tmp/bibliography_M_ground_truth_tfidf.csv'
    #config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/bibliography/conf_bibliography_xgb_1.json'
    # product
    #scores_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/product/scores_1/scores.csv'
    #tgt_file = 'C:/my/tmp/ontomatch/20211118_tmp/product_N_random_blocking_tfidf.csv'
    #match_file = 'C:/my/tmp/ontomatch/20211118_tmp/product_M_ground_truth_tfidf.csv'
    #config_file = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB/product/conf_product_xgb_1.json'

    sys.argv.extend(['--config', config_file])
    ontomatch.utils.util.init()
    nonmatch_match_ratio = 5
    tgt_file = tgt_file[:-4] + '_ratio_' + str(nonmatch_match_ratio) + '.csv'
    TrainTestGenerator.random_nonmatches_from_file_and_split(scores_file, tgt_file, match_file, nonmatch_match_ratio)

if __name__ == '__main__':
    #start_M()
    start_N_from_blocks()
