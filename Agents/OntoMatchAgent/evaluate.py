import logging

import numpy as np
import pandas as pd

import readAlignment
import util

def read_alignment_file_as_dataframe(filename):
    reader = readAlignment.AReader(filename)
    reader.readAlignment(-1.)

    rows = []
    for iri1, iri2, score in reader.a.map:
        idx1 = getID(iri1)
        idx2 = getID(iri2)
        row = {'idx_1': idx1, 'idx_2': idx2, 'score': score}
        rows.append(row)

    dframe = pd.DataFrame(rows)
    dframe.set_index(['idx_1', 'idx_2'], inplace=True)
    return dframe

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

def calculate_precision_recall(predicted_matches, matches):

    true_matches = matches.intersection(predicted_matches)
    false_matches = predicted_matches.difference(matches)
    false_nonmatches = matches.difference(predicted_matches)
    tm = len(true_matches)
    fm = len(false_matches)
    fn = len(false_nonmatches)

    precision = 1.
    recall = 0.
    f1score = 0.
    if tm > 0:
        precision = tm / (tm + fm)
        recall = tm / (tm + fn)
        f1score = 2 * precision * recall / (precision + recall)

    #logging.debug('number predicted matches=%s', len(predicted_matches))
    #logging.debug('TP=%s, FP=%s, FN=%s', tm, fm, fn)
    #logging.debug('precision=%s, recall=%s', precision, recall)

    return true_matches, false_matches, false_nonmatches, precision, recall, f1score

def get_max_f1score(result):
    max_f1score = 0.
    max_t = 0
    for r in result:
        if r[6] >= max_f1score:
            max_f1score = r[6]
            max_t = r[0]
    return max_t, max_f1score

def get_area_under_curve(result):
    area_under_curve = 0.
    last_recall = 0.

    for r in result:
        current_precision = r[1]
        current_recall = r[2]
        assert last_recall <= current_recall
        #TODO-AE URGENT use (last precision and current_precision) / 2
        area_under_curve += (current_recall - last_recall) * current_precision
        last_recall = current_recall

    return area_under_curve


def evaluate(df_alignment, matches, number_of_thresholds=41):

    result = []
    thresholds = np.linspace(1, 0, num=number_of_thresholds, endpoint=True)
    for t in thresholds:
        mask = (df_alignment['score'] >= t)
        predicted_matches =  df_alignment[mask].index
        true_matches, false_matches, false_nonmatches, precision, recall, f1score = calculate_precision_recall(
            predicted_matches, matches)
        entry = [t, precision, recall, len(true_matches), len(false_matches), len(false_nonmatches), f1score]
        result.append(entry)

    logging.info('evaluation result (threshold, precision, recall, TP, FP, FN, f1score):\n%s', result)
    max_t, max_f1score = get_max_f1score(result)
    area_under_curve = get_area_under_curve(result)
    logging.info('max f1-score=%s for threshold t=%s', max_f1score, max_t)
    logging.info('area under curve=%s', area_under_curve)

    return result


if __name__ == '__main__':

    util.init_logging()

    alignmentfile = './2109xx.owl'
    df_alignment = read_alignment_file_as_dataframe(alignmentfile)
    matchfile = 'C:/my/tmp/ontomatch/scores_kwl_20210720_8.csv'
    #matchfile = 'C:/my/tmp/ontomatch/20210914_scores_dukes_gppd_v3.csv'
    #matchfile = 'C:/my/tmp/ontomatch/scores_dbp_DEU_v2.csv'
    index_set_matches = read_match_file_as_index_set(matchfile, linktypes = [1, 3, 4, 5])
    logging.info('length of alignment file=%s, ground truth matches=%s', len(df_alignment), len(index_set_matches))
    evaluate(df_alignment, index_set_matches)
