import logging

import numpy as np
import pandas as pd

def evaluate_y_pred(y_test, y_pred):
    tp, tn, fp, fn = 0, 0, 0, 0
    for i, y in enumerate(y_test):
        y_p = int(y_pred[i])
        if y == 0:
            if y_p == 0:
                tn += 1
            elif y_p == 1:
                fp += 1
            else:
                raise ValueError()
        elif y == 1:
            if y_p == 0:
                fn += 1
            elif y_p == 1:
                tp += 1
            else:
                raise ValueError()
        else:
            raise ValueError()
    precision, recall, f1score = calculate_precision_recall_intern(tp, fp, fn)
    return tp, fp, fn, precision, recall, f1score

def evaluate_y_pred_proba(y_test, y_pred_proba, number_of_thresholds=41):
    result = []
    thresholds = np.linspace(1, 0, num=number_of_thresholds, endpoint=True)
    for t in thresholds:
        # y_pred_proba is an array of tuples (probability nonmatch, probability match)
        # both probabilities sum up to 1
        # the second probability (index = 1) is the 'confidence' for a match
        map_fct = lambda x: 1 if x[1] > t else 0
        y_pred = [ y for y in map(map_fct, y_pred_proba) ]
        tp, fp, fn, precision, recall, f1score = evaluate_y_pred(y_test, y_pred)
        entry = [t, precision, recall, tp, fp, fn, f1score]
        result.append(entry)
    return result

def evaluate_with_pred_proba(model, x, y, number_of_thresholds=41):

    score = model.score(x, y)
    logging.info('score on entire test set=%s, len=%s', score, len(x))

    #y_pred = model.predict(x)
    #ontomatch.evaluate.evaluate_y_pred(y, y)

    y_pred_proba = model.predict_proba(x)
    result = evaluate_y_pred_proba(y, y_pred_proba, number_of_thresholds)
    log_result(result)
    return result

def getID(iri):
    if iri.startswith('http://dbpedia.org/resource/'):
        return iri.replace('http://dbpedia.org/resource/', 'dbr:')
    if iri.startswith('http://www.google.com/base/feeds/snippets/'):
        return iri
    i = iri.rfind('/')
    strs = iri[i+1:].split('_')
    return strs[0]

def read_match_file_as_index_set(filename, linktypes):
    dframe = pd.read_csv(filename)
    dframe['idx_1'] = dframe['idx_1'].astype(str)
    dframe['idx_2'] = dframe['idx_2'].astype(str)
    fct = lambda s : s.replace('http://www.google.com/base/feeds/snippets/', '')
    dframe['idx_2'] = dframe['idx_2'].apply(fct)
    dframe.set_index(['idx_1', 'idx_2'], inplace=True)
    #idx0 = dframe.index.levels[0].astype(str)
    #idx1 = dframe.index.levels[1].astype(str)
    #dframe.index = dframe.index.set_levels([idx0, idx1])

    mask = (dframe['link'] >= 0) & False
    for t in linktypes:
        mask = (mask | (dframe['link'] == t))

    mi_match_pairs = dframe[mask].index
    #remove duplicates
    mi_match_pairs = mi_match_pairs.intersection(mi_match_pairs)
    logging.info('loaded evaluation file=%s, link types=%s, number of matches=%s', filename, linktypes, len(mi_match_pairs))
    return mi_match_pairs

def calculate_precision_recall_intern(tm, fm, fn):
    precision = 1.
    recall = 0.
    f1score = 0.
    if tm > 0:
        precision = tm / (tm + fm)
        recall = tm / (tm + fn)
        f1score = 2 * precision * recall / (precision + recall)
    return precision, recall, f1score

def calculate_precision_recall(predicted_matches, matches):

    true_matches = matches.intersection(predicted_matches)
    false_matches = predicted_matches.difference(matches)
    false_nonmatches = matches.difference(predicted_matches)
    tm = len(true_matches)
    fm = len(false_matches)
    fn = len(false_nonmatches)

    precision, recall, f1score = calculate_precision_recall_intern(tm, fm, fn)
    return true_matches, false_matches, false_nonmatches, precision, recall, f1score

def get_max_f1score(result):
    max_f1score = 0.
    max_t = 0
    precision = 0.
    recall = 0.
    for r in result:
        if r[6] >= max_f1score:
            max_f1score = r[6]
            max_t = r[0]
            precision = r[1]
            recall = r[2]
    return max_t, max_f1score, precision, recall

def get_area_under_curve(result):
    area_under_curve = 0.
    last_recall = 0.

    for r in result:
        current_precision = r[1]
        current_recall = r[2]
        assert last_recall <= current_recall
        area_under_curve += (current_recall - last_recall) * current_precision
        last_recall = current_recall

    return area_under_curve

def log_result(result, hint='evaluation result', count_matches=None, estimated_threshold=None):
    max_t, max_f1score, precision, recall = get_max_f1score(result)
    area_under_curve = get_area_under_curve(result)
    logging.info('%s: max f1=%s for t=%s, p=%s, r=%s, area under curve=%s', hint, max_f1score, max_t, precision, recall, area_under_curve)
    res = get_f1_by_threshold(result, threshold=0.5)
    logging.info('%s - by threshold 0.5: max f1=%s for t=%s, p=%s, r=%s, TP=%s, FP=%s, FN=%s',
                hint, res[6], res[0], res[1], res[2], res[3], res[4], res[5])

    if count_matches is not None:
        res = get_f1_by_predicted_match_count(result, count_matches)
        if res:
            logging.info('%s - by match count: max f1=%s for t=%s, p=%s, r=%s, TP=%s, FP=%s, FN=%s',
                hint, res[6], res[0], res[1], res[2], res[3], res[4], res[5])
        else:
            logging.warning('%s - by match count: not found', hint)
    if estimated_threshold is not None:
        res = get_f1_by_threshold(result, estimated_threshold)
        if res:
            logging.info('%s - by estimated threshold: max f1=%s for t=%s (estimated=%s), p=%s, r=%s, TP=%s, FP=%s, FN=%s',
                hint, res[6], res[0], round(estimated_threshold, 5), res[1], res[2], res[3], res[4], res[5])
        else:
            logging.warning('%s - by estimated threshold: not found', hint)

    logging.info('threshold, precision, recall, TP, FP, FN, f1score:\n%s', result)

def evaluate(df_scores, index_matches, number_of_thresholds=201, hint='evaluation result', estimated_threshold=None):

    result = []
    thresholds = [ round(t,5) for t in np.linspace(1, 0, num=number_of_thresholds, endpoint=True)]
    for t in thresholds:
        mask = (df_scores['score'] >= t)
        predicted_matches =  df_scores[mask].index
        true_matches, false_matches, false_nonmatches, precision, recall, f1score = calculate_precision_recall(
            predicted_matches, index_matches)
        entry = [t, round(precision, 5), round(recall, 5), len(true_matches), len(false_matches), len(false_nonmatches), round(f1score, 5)]
        result.append(entry)

    log_result(result, hint, len(index_matches), estimated_threshold)
    return result

def evaluate_on_train_test_split(df_scores, index_train_set, index_test_set, index_matches, hint:str, estimated_threshold=None):

    index_matches_fn = index_matches.difference(df_scores.index)
    len_train_set = len(index_train_set) if index_train_set is not None else 0
    len_test_set = len(index_test_set) if index_test_set is not None else 0
    logging.info('evaluation result for scores=%s, train_set=%s, test_set=%s, all matches=%s, FN=%s',
        len(df_scores), len_train_set, len_test_set, len(index_matches), len(index_matches_fn))
    if len_train_set > 0:
        df_scores_tmp = df_scores.loc[index_train_set]
        hint_long = 'evaluation result on training set minus FN, ' + hint
        index_matches_minus_fn = index_matches.intersection(index_train_set)
        evaluate(df_scores_tmp, index_matches_minus_fn, hint=hint_long, estimated_threshold=estimated_threshold)
        hint_long = 'evaluation result on training set incl. FN, ' + hint
        index_matches_incl_fn = index_matches_minus_fn.union(index_matches_fn)
        evaluate(df_scores_tmp, index_matches_incl_fn, hint=hint_long, estimated_threshold=estimated_threshold)

    if len_test_set > 0:
        df_scores_tmp = df_scores.loc[index_test_set]
        hint_long = 'evaluation result on test set minus FN, ' + hint
        index_matches_minus_fn = index_matches.intersection(index_test_set)
        evaluate(df_scores_tmp, index_matches_minus_fn, hint=hint_long, estimated_threshold=estimated_threshold)
        hint_long = 'evaluation result on test set incl. FN, ' + hint
        index_matches_incl_fn = index_matches_minus_fn.union(index_matches_fn)
        evaluate(df_scores_tmp, index_matches_incl_fn, hint=hint_long, estimated_threshold=estimated_threshold)

def get_f1_by_predicted_match_count(result, count_matches):
    for res in result:
        # threshold, precision, recall, TP, FP, FN, f1score
        pred_matches = res[3] + res[4]
        if pred_matches > count_matches:
            return res

def get_f1_by_threshold(result, threshold):
    for res in result:
        # threshold, precision, recall, TP, FP, FN, f1score
        if res[0] <= threshold:
            return res
