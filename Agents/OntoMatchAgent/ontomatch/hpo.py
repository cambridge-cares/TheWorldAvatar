import logging
import time

import pandas as pd
import sklearn
import sklearn.ensemble
import sklearn.impute
import sklearn.metrics
import sklearn.metrics.pairwise
import sklearn.model_selection
import sklearn.neural_network
import sklearn.pipeline
import xgboost

def create_classifier_XGB():
    return xgboost.XGBClassifier()

def create_classifier_MLP(params_hpo, params_impution):
    if params_impution['name'] == 'sklearn.impute.KNNImputer':
        imputer = sklearn.impute.KNNImputer()
    else:
        raise ValueError('unknown imputer with name', params_impution['name'])
    classifier = sklearn.neural_network.MLPClassifier(solver='adam') #, max_iter='400')
    estimator = sklearn.pipeline.Pipeline([('imputer', imputer), ('mlp', classifier)])

    #logging.debug('available parameter keys for MLP=%s', estimator.get_params().keys())

    all_params = params_impution["model_specific"].copy()
    all_params.update(params_hpo)
    params_hpo_mlp = {}
    for key, value in all_params.items():

        if key == 'hidden_layer_sizes':
            # e.g. ['10', '5', '10,5,3']
            current_value = value.copy()
            value = []
            for v in current_value:
                t = tuple(map(int, v.split(',')))
                value.append(t)

        if key in ['n_neighbors', 'weights']:
            # add prefix 'imputer__' because estimator is a pipeline
            params_hpo_mlp['imputer__' + key] = value
        elif key in ['hidden_layer_sizes', 'learning_rate', 'learning_rate_init', 'alpha', 'beta_1', 'beta_2']:
            # add prefix 'mlp__' because estimator is a pipeline
            params_hpo_mlp['mlp__' + key] = value
        else:
            params_hpo_mlp[key] = value

    return estimator, params_hpo_mlp

def get_params_for_hpo(params_model_specific):
    params_hpo = {}
    for key, value in params_model_specific.items():
        if not value:
            continue
        elif isinstance(value, list):
            params_hpo[key] = value
        else:
            params_hpo[key] = [value]
    return params_hpo

def start_hpo(params_classification, cross_validation, params_impution, x_train, y_train, x_test, y_test):

    name = params_classification['name']
    logging.info('name=%s, cross_validation=%s', name, cross_validation)
    params_hpo = get_params_for_hpo(params_classification['model_specific'])
    count_nonmatch = y_train.value_counts().loc[0]
    count_match = y_train.value_counts().loc[1]
    scale_pos_weight = count_nonmatch / count_match
    logging.info('training classifier with nonmatches=%s, matches=%s, N-M-ratio=%s', count_nonmatch, count_match, scale_pos_weight)

    if name == 'XGB':
        #params_hpo['scale_pos_weight'] = [scale_pos_weight]
        model = create_classifier_XGB()
        # 'use_label_encoder' is no HPO parameter. If it is not set as parameter for GridSearchCV,
        # then the following log message appears for each CV-fold training:
        # UserWarning: The use of label encoder in XGBClassifier is deprecated and will be removed in a future release.
        # To remove this warning, do the following: 1) Pass option use_label_encoder=False when constructing XGBClassifier
        # object; and 2) Encode your labels (y) as integers starting with 0, i.e. 0, 1, 2, ..., [num_class - 1].
        #params_hpo.update({'use_label_encoder': [True]})
    elif name == 'MLP':
        model, params_hpo = create_classifier_MLP(params_hpo, params_impution)

    logging.info('params_hpo=%s', params_hpo)

    scoring = 'f1'
    # you can set return_train_score to False and remove dumping cv_results_ below
    # if you are not going to analyse HPO params
    return_train_score=True
    hpo_model = sklearn.model_selection.GridSearchCV(model, param_grid=params_hpo, cv = cross_validation, verbose=3, scoring=scoring, return_train_score=return_train_score)
    logging.info('training model with name=%s', name)
    hpo_model.fit(x_train, y_train)
    logging.info('trained model=%s', hpo_model)
    # for each grid point and for each of the k(=5) folds, compute the score, i.e. evaluate the trained classifier wrt to the metric choosen for param scoring above
    # for each grid point, the score is the average score over the scores of all k folds
    # the best score is the highest average
    logging.info('best_score (split test average over k folds)=%s, best_params=%s', hpo_model.best_score_, hpo_model.best_params_)

    df_cv_results = pd.DataFrame(hpo_model.cv_results_)
    df_cv_results.to_csv('./cv_result_' + str(time.time()) + '.csv')

    score = hpo_model.score(x_train, y_train)
    logging.info('(score on entire training set=%s, len=%s)', score, len(x_train))
    if x_test is not None:
        score = hpo_model.score(x_test, y_test)
        # if test score is close to best_score then no overfitting
        # don't compare it with the score on the entire training set
        logging.info('score on entire test set=%s, len=%s', score, len(x_test))

    return hpo_model

def start(params_classification, cross_validation, params_impution, x_train, y_train, x_test, y_test, df_scores, prop_columns):
    model = start_hpo(params_classification, cross_validation, params_impution, x_train, y_train, x_test, y_test)
    logging.info('predicting probability of match or nonmatch')
    y_pred_proba = model.predict_proba(df_scores[prop_columns])
    y_pred_proba_match = [ ymatch for (_, ymatch) in y_pred_proba]
    df_scores['score'] = y_pred_proba_match

    return df_scores
