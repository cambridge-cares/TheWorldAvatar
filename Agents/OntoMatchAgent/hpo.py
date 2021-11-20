import logging

import numpy as np
import pandas as pd
import sklearn
import sklearn.ensemble
import sklearn.metrics.pairwise
import sklearn.model_selection
import sklearn.svm
import xgboost

import classification
import evaluate
import util

def create_classifier_RF():
    return sklearn.ensemble.RandomForestClassifier(n_jobs=1, verbose=0, class_weight='balanced')

def create_classifier_XGB():
    return xgboost.XGBClassifier()

def start_hpo(params_classification, x, y):

    cross_validation = params_classification['cross_validation']
    params_hpo = {}
    for key, value in params_classification['model_specific'].items():
        if not value:
            continue
        elif isinstance(value, list):
            params_hpo[key] = value
        else:
            params_hpo[key] = [value]
    logging.info('cross_validation=%s, params_hpo=%s', cross_validation, params_hpo)

    logging.info('y value counts=%s', y.value_counts())
    count_nonmatch = y.value_counts().loc[0]
    count_match = y.value_counts().loc[1]
    scale_pos_weight = count_nonmatch / count_match
    logging.info('unbalanced classification with nonmatches=%s, matches=%s, ratio=%s', count_nonmatch, count_match, scale_pos_weight)

    name = params_classification['name']
    if name == 'RF':
        model = create_classifier_RF()
    elif name == 'XGB':
        #params_hpo['scale_pos_weight'] = [scale_pos_weight]
        model = create_classifier_XGB()
        # 'use_label_encoder' is no HPO parameter. If it is not set as parameter for GridSearchCV,
        # then the following log message appears for each CV-fold training:
        # UserWarning: The use of label encoder in XGBClassifier is deprecated and will be removed in a future release.
        # To remove this warning, do the following: 1) Pass option use_label_encoder=False when constructing XGBClassifier
        # object; and 2) Encode your labels (y) as integers starting with 0, i.e. 0, 1, 2, ..., [num_class - 1].
        #params_hpo.update({'use_label_encoder': [True]})

    #hpo_model = sklearn.model_selection.GridSearchCV(model, param_grid=params_hpo, cv = cross_validation, verbose=3)
    #TODO-AE 211119: scoring function for XGB
    hpo_model = sklearn.model_selection.GridSearchCV(model, param_grid=params_hpo, cv = cross_validation, verbose=3, scoring = 'f1')
    logging.info('training model with name=%s', name)
    hpo_model.fit(x, y)
    logging.info('trained model=%s', hpo_model)
    logging.info('best_score=%s, best_params=%s', hpo_model.best_score_, hpo_model.best_params_)

    score = hpo_model.score(x, y)
    logging.info('score on entire training set=%s, len=%s', score, len(x))

    return hpo_model

def evaluate_with_pred_proba(model, x, y, number_of_thresholds=41):

    score = model.score(x, y)
    logging.info('score on entire test set=%s, len=%s', score, len(x))

    #y_pred = model.predict(x)
    #evaluate.evaluate_y_pred(y, y)

    y_pred_proba = model.predict_proba(x)
    result = evaluate.evaluate_y_pred_proba(y, y_pred_proba, number_of_thresholds)
    evaluate.log_result(result)
    return result

def get_train_set_from_auto_scores(total_scores_file, scores_file, lower_threshold, upper_threshold, prop_columns, nonmatch_match_ratio = 1):

    logging.info('get train set from total scores=%s, scores=%s, lower=%s, upper=%s, columns%s, ratio=%s',
            total_scores_file, scores_file, lower_threshold, upper_threshold, prop_columns, nonmatch_match_ratio)

    #assert lower_threshold <= upper_threshold

    dframe = util.read_csv(total_scores_file)
    mask = ((dframe['best'] == True) & (dframe['score'] >= upper_threshold ))
    index_matches = dframe[mask].index

    mask = ((dframe['best'] == False) & (dframe['score'] < lower_threshold ))
    index_nonmatches_lower = dframe[mask].index.difference(index_matches)

    number_nonmatches = len(index_matches) * nonmatch_match_ratio
    assert number_nonmatches <= len(index_nonmatches_lower) / 1.5
    index_nonmatches = classification.Utils.sample_without_replacement(index_nonmatches_lower, number_nonmatches)

    logging.info('matches upper=%s, all nonmatches lower=%s, sampled nonmatches=%s',
            len(index_matches), len(index_nonmatches_lower), len(index_nonmatches))

    if scores_file is None:
        # use auto-calibrated scores
        df_scores = util.read_csv(total_scores_file)
    else:
        # use the original similarity scores
        df_scores = util.read_csv(scores_file)

    index_union = index_matches.union(index_nonmatches)
    df_scores = df_scores.loc[index_union].copy()
    df_scores['y'] = 0
    df_scores.loc[index_matches, 'y'] = 1

    x_train = df_scores[prop_columns].copy()
    y_train = df_scores['y'].copy()

    logging.info('x_train=%s, y_train=%s', len(x_train), len(y_train))
    return x_train, y_train

def start_from_console():

    params = util.init()
    params_classification = params['classification']
    # KWL
    match_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_DEU_M_ground_truth_tfidf.csv'
    nonmatch_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_DEU_N_random_blocking_tfidf.csv'
    scores_dir = 'C:/my/repos/ontomatch_20210924/experiments/211118_XGB_ratio_2/power_plant_DEU/scores_no_geo'
    total_scores_file = scores_dir + '/total_scores.csv'
    scores_file = scores_dir + '/scores.csv'
    # DUKES
    #match_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_GBR_M_ground_truth_tfidf.csv'
    #nonmatch_file = 'C:/my/tmp/ontomatch/20211118_tmp/power_plant_GBR_N_random_blocking_tfidf.csv'
    # bibliography
    #match_file = 'C:/my/tmp/ontomatch/20211118_tmp/bibliography_M_ground_truth_tfidf.csv'
    #nonmatch_file = 'C:/my/tmp/ontomatch/20211118_tmp/bibliography_N_random_blocking_tfidf.csv'
    # product
    #match_file = 'C:/my/tmp/ontomatch/20211118_tmp/product_M_ground_truth_tfidf.csv'
    #nonmatch_file = 'C:/my/tmp/ontomatch/20211118_tmp/product_N_random_blocking_tfidf.csv'

    nonmatch_match_ratio = 2
    nonmatch_file = nonmatch_file[:-4] + '_ratio_' + str(nonmatch_match_ratio) + '.csv'

    train_size = 0.2
    column_ml_phase = 'ml_phase_' + str(train_size)
    prop_columns=['0', '1', '2', '3', '4']
    x_train, x_test, y_train, y_test = classification.TrainTestGenerator.train_test_split(
        match_file, nonmatch_file, column_ml_phase, prop_columns)

    if True:
        logging.info('classifying similarity vectors')
        model = start_hpo(params_classification, x_train, y_train)
        result = evaluate_with_pred_proba(model, x_test, y_test)

    if False:
        logging.info('classifying similarity vectors selected by means of autocalibrated total scores')
        lower_threshold = 0.2
        upper_threshold = 0.5
        x_train, y_train = get_train_set_from_auto_scores(total_scores_file, scores_file, lower_threshold, upper_threshold, prop_columns, nonmatch_match_ratio)
        model = start_hpo(params_classification, x_train, y_train)
        #TODO-AE 211119 evaluate on the entire ground truth and all matches?
        # evaluate on the same test set as above
        result = evaluate_with_pred_proba(model, x_test, y_test)

    if False:
        logging.info('classifying auto sim vectors')
        lower_threshold = 0.5
        upper_threshold = 0.5
        scores_file = None
        x_train, y_train = get_train_set_from_auto_scores(total_scores_file, scores_file, lower_threshold, upper_threshold, prop_columns, nonmatch_match_ratio)
        model = start_hpo(params_classification, x_train, y_train)
        #TODO-AE 211119 evaluate on the entire ground truth and all matches?
        # evaluate on the same test set as above
        result = evaluate_with_pred_proba(model, x_test, y_test)

if __name__ == '__main__':
    start_from_console()
