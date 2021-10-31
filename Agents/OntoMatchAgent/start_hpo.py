import collections
import json
import logging
import os
import pickle
import time

import coordinator
import evaluate
import instancematching
import scoring
import util

def hpo(params, matchfile):

    matcher_name = params['matching']['name']
    logging.info('starting HPO for %s', matcher_name)

    logging.info('creating property similarity scores')
    starttime = time.time()
    agent = coordinator.Agent()
    df_scores = agent.start(params)
    logging.info('created property similarity scores')
    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)

    # grid search for space of scoring weights
    missing_scores = [None]
    if matcher_name == 'matchManager.matchManager':
        prop_column_names = params['matching']['model_specific']['steps']
        number_weights = len(prop_column_names)
        max_weight = 10
        it = scoring.ScoringWeightIterator(number_weights, max_weight)
        #it = [[0.7, 0.3, 0.0]]
    elif matcher_name == 'instancematching.InstanceMatcherWithScoringWeights':
        params_mapping = params['mapping']
        prop_prop_sim_triples = scoring.create_prop_prop_sim_triples_from_params(params_mapping)
        prop_column_names = [ c for c in range(len(prop_prop_sim_triples)) ]
        number_weights = len(prop_prop_sim_triples)
        max_weight = 10
        sample_count = 500
        #sample_count = 100
        #sample_count = None
        it = scoring.ScoringWeightIterator(number_weights, max_weight, sample_count)
        #it = [[0.8, 0.0, 0.2, 0.0]]
        #it = [[0.6, 0.1, 0.1, 0.0, 0.2]]
        #it = [[0.4, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]]
        #missing_scores = [None, 0.1, 0.2, 0.3, 0.4, 0.5]
        #missing_scores = [None, 0.2, 0.4]

    logging.info('total number of iterations=%s', len(it) )

    best_f1_dict = {'f1': - 1}
    best_auc_dict = {'auc': - 1}
    index_set_matches = evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])

    for missing_score in missing_scores:
        for count, scoring_weights in enumerate(it):
            assert abs(1. - sum(scoring_weights)) <= 0.0001

            logging.info('hpo iteration=%s, scoring_weights=%s, missing_score=%s', count, scoring_weights, missing_score)
            instancematching.add_total_scores(df_scores, props=prop_column_names, scoring_weights=scoring_weights,
                        missing_score=missing_score, aggregation_mode='sum', average_min_prop_count=2)

            result = evaluate.evaluate(df_scores, index_set_matches)
            threshold, f1_score = evaluate.get_max_f1score(result)
            area_under_curve = evaluate.get_area_under_curve(result)

            if f1_score > best_f1_dict['f1']:
                best_f1_dict = current_state_to_dict(f1_score, threshold, area_under_curve, scoring_weights, missing_score, result)
            if area_under_curve > best_auc_dict['auc']:
                best_auc_dict = current_state_to_dict(f1_score, threshold, area_under_curve, scoring_weights, missing_score, result)

    logging.info('best_f1_dict=%s', best_f1_dict)
    logging.info('best_auc_dict=%s', best_auc_dict)
    logging.info('finished HPO')
    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)


def current_state_to_dict(f1_score, threshold, area_under_curve, scoring_weights, missing_score, result):
    return {
        'f1': f1_score,
        'threshold': threshold,
        'auc': area_under_curve,
        'scoring_weights': scoring_weights,
        'missing_score': missing_score,
        'result': result
    }

if __name__ == '__main__':

    #config_file = './conf/conf_value_matcher.json'
    config_file = './tests/conf/conf_scoring_weight_matcher_kwl_gppd.json'
    #config_file = './tests/conf/conf_scoring_weight_matcher_kwl_gppd_geo.json'
    #config_file = 'C:/my/repos/ontomatch_20210924/experiments/211028_tmp/conf_tmp.json'
    #config_file = "./conf/conf_scoring_weight_matcher_dukes_gppd.json"
    #config_file = "./conf/conf_scoring_weight_matcher_dukes_gppd_geo.json"

    with open(config_file) as json_config:
        params = json.load(json_config, object_pairs_hook=collections.OrderedDict)

    util.init_logging('.', '..')
    logging.info('current working directory=%s', os.getcwd())
    logging.info('config=%s', params)

    matchfile = 'C:/my/tmp/ontomatch/scores_kwl_20210720_8.csv'
    #matchfile = 'C:/my/tmp/ontomatch/20210914_scores_dukes_gppd_v3.csv'
    #matchfile = 'C:/my/tmp/ontomatch/scores_dbp_DEU_v2.csv'

    hpo(params, matchfile)
