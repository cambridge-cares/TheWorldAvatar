import collections
import json
import logging
import pickle
import time

import coordinator
import evaluate
import instancematching
import scoring
import util

def hpo_matchmanager(params):

    logging.info('starting HPO for MatchManager')

    alignmentfile = './2109xx.owl'
    matchfile = 'C:/my/tmp/ontomatch/scores_kwl_20210720_8.csv'
    #matchfile = 'C:/my/tmp/ontomatch/20210914_scores_dukes_gppd_v3.csv'
    #matchfile = 'C:/my/tmp/ontomatch/scores_dbp_DEU_v2.csv'
    index_set_matches = evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])

    # grid search for space of scoring weights
    count = 0

    best_f1score = -1
    best_threshold = -1
    best_scoring_weights = None
    best_result = None

    max_weight = 10
    it = scoring.ScoringWeightIterator(3, max_weight)
    logging.info('total number of iterations=%s', len(it))
    for scoring_weights in it:

        count += 1
        assert abs(1. - sum(scoring_weights)) <= 0.0001
        params_ms = params['matching']['model_specific']
        params_ms['weights'] = scoring_weights

        logging.info('hpo iteration=%s, scoring_weights=%s', count, scoring_weights)

        starttime = time.time()
        agent = coordinator.Agent()
        agent.start(params)
        timenow = time.time()-starttime
        logging.info('elapsed time in seconds=%s', timenow)

        df_alignment = evaluate.read_alignment_file_as_dataframe(alignmentfile)

        logging.info('lenght of alignment file=%s, ground truth matches=%s', len(df_alignment), len(index_set_matches))
        result = evaluate.evaluate(df_alignment, index_set_matches)
        threshold, f1score = evaluate.get_max_f1score(result)
        if f1score > best_f1score:
            best_f1score = f1score
            best_threshold = threshold
            best_scoring_weights = scoring_weights
            best_result = result

    logging.info('result with best max f1score=\n%s', best_result)
    logging.info('best_f1score=%s, best_threshold=%s, best_scoring_weights=%s', best_f1score, best_threshold, best_scoring_weights)
    logging.info('finished HPO')


def hpo_instancematcherwithscoringweights(params):

    logging.info('starting HPO for InstanceMatcherWithScoringWeights')

    src_addr = params['dataset']['src']
    tgt_addr = params['dataset']['tgt']
    with open(src_addr,'rb') as file:
        src_onto = pickle.load(file)
    with open(tgt_addr,'rb') as file:
        tgt_onto = pickle.load(file)

    matchfile = 'C:/my/tmp/ontomatch/scores_kwl_20210720_8.csv'
    #matchfile = 'C:/my/tmp/ontomatch/20210914_scores_dukes_gppd_v3.csv'
    #matchfile = 'C:/my/tmp/ontomatch/scores_dbp_DEU_v2.csv'
    index_set_matches = evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])


    params_blocking = params['blocking']

    #prop_prop_sim_triples = get_prop_prop_sim_tuples_without_geo_coordinates()
    params_mapping = params['mapping']
    prop_prop_sim_triples = scoring.create_prop_prop_sim_triples_from_params(params_mapping)

    matcher = instancematching.InstanceMatcherWithScoringWeights()
    matcher.start(src_onto, tgt_onto, params_blocking, None, prop_prop_sim_triples)
    df_scores = matcher.get_scores()
    logging.debug('number=%s', len(df_scores))
    logging.debug('columns=%s', [ str(c) for c in df_scores.columns])

    # grid search for space of scoring weights
    count = 0

    best_f1score = -1
    best_threshold = -1
    best_scoring_weights = None
    best_missing_score = -1
    best_result = None

    best_auc = - 1
    best_auc_scoring_weights = None
    best_auc_missing_score = -1
    best_auc_result = None

    number_weights = len(prop_prop_sim_triples)
    max_weight = 10
    #sample_count = 200
    sample_count = 100
    it = scoring.ScoringWeightIterator(number_weights, max_weight, sample_count)
    #it = [[.4, 0.2, 0.1, 0.1, 0.2]]
    #it = [[0.6, 0.1, 0.1, 0.0, 0.2]]
    #missing_scores = [None, 0.1, 0.2, 0.3, 0.4, 0.5]
    #missing_scores = [None, 0.2, 0.4]
    missing_scores = [None]

    logging.info('total number of iterations=%s', len(it) * len(missing_scores))
    for missing_score in missing_scores:
        logging.info('iteration missing score=%s', missing_score)
        for scoring_weights in it:

            count += 1
            assert abs(1. - sum(scoring_weights)) <= 0.0001

            logging.info('hpo iteration=%s, scoring_weights=%s', count, scoring_weights)

            starttime = time.time()
            instancematching.add_total_scores(df_scores, props=[0, 1, 2, 3, 4], scoring_weights=scoring_weights,
                    missing_score=missing_score, aggregation_mode='sum', average_min_prop_count=2)
            timenow = time.time()-starttime

            logging.info('elapsed time in seconds=%s', timenow)

            logging.info('lenght of alignment file=%s, ground truth matches=%s', len(df_scores), len(index_set_matches))
            result = evaluate.evaluate(df_scores, index_set_matches)
            threshold, f1score = evaluate.get_max_f1score(result)
            if f1score > best_f1score:
                best_f1score = f1score
                best_threshold = threshold
                best_scoring_weights = scoring_weights
                best_missing_score = missing_score
                best_result = result

            area_under_curve = evaluate.get_area_under_curve(result)
            if area_under_curve > best_auc:
                best_auc = area_under_curve
                best_auc_scoring_weights = scoring_weights
                best_auc_missing_score = missing_score
                best_auc_result = result

    logging.info('result with best max f1score=\n%s', best_result)
    logging.info('best_f1score=%s, best_threshold=%s, best_scoring_weights=%s, best_missing_score=%s',
                best_f1score, best_threshold, best_scoring_weights, best_missing_score)
    logging.info('result with best area under curve=\n%s', best_auc_result)
    logging.info('best_auc=%s, best_auc_scoring_weights=%s, best_auc_missing_score=%s',
                best_auc, best_auc_scoring_weights, best_auc_missing_score)
    logging.info('finished HPO')

if __name__ == '__main__':

    #config_file = './conf/conf_value_matcher.json'
    #config_file = './tests/conf/conf_scoring_weight_matcher_kwl_gppd.json'
    config_file = 'C:/my/repos/ontomatch_20210924/experiments/211028_tmp/conf_tmp.json'

    with open(config_file) as json_config:
        params = json.load(json_config, object_pairs_hook=collections.OrderedDict)

    util.init_logging('.', '..')

    #hpo_matchmanager(params)
    hpo_instancematcherwithscoringweights(params)
