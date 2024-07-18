import json
import logging
import pickle
import random
import sys

import pandas as pd
import sklearn.model_selection

import ontomatch.coordinator
import ontomatch.evaluate
import ontomatch.instancematching
import ontomatch.scoring
import ontomatch.utils.blackboard
import ontomatch.utils.util

def start_pickle_dump():
    file = './data/bibl_DBLP_Scholar/scholar.ttl'
    tgt_file = ontomatch.utils.blackboard.LOCAL_BLACKBOARD_DIR + '/scholar.pkl'
    onto = ontomatch.utils.util.load_ontology(file, blackboard=False)
    ontomatch.utils.util.pickle_dump(tgt_file, onto)

def create_train_valid_test_split_files():

    config_file = './experiments/autocal_mtf_50/conf_kg_auto_50.json'
    #config_file = './experiments/autocal_mtf_50/conf_dg_auto_50.json'
    #mtf = 100
    
    sys.argv.extend(['--config', config_file])
    params, _ = ontomatch.utils.util.init()

    src_file =  params['dataset']['src']
    src_onto = ontomatch.utils.util.read_csv_table_ditto_format(src_file)
    tgt_file = params['dataset']['tgt']
    tgt_onto = ontomatch.utils.util.read_csv_table_ditto_format(tgt_file)

    matchfile = params['post_processing']['evaluation_file']
    index_matches = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
    logging.info('ground truth matches=%s', len(index_matches))

    params_blocking = params['blocking']
    #params_blocking['model_specific']['max_token_occurrences_src'] = mtf
    #params_blocking['model_specific']['max_token_occurrences_tgt'] = mtf
    #logging.debug('changed params_blocking=%s', params_blocking)

    iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params_blocking)
    index_pairs = iterator.candidate_matching_pairs
    intersection_matches = index_pairs.intersection(index_matches)
    false_negatives = index_matches.difference(intersection_matches)
    logging.info('number of matches in candidate set=%s, false negatives=%s', len(intersection_matches), len(false_negatives))

    df_candidates = pd.DataFrame(index=index_pairs)
    df_candidates.index.names = ['ltable_id','rtable_id']
    df_candidates['label'] = 0 # nonmatch
    df_candidates.at[intersection_matches, 'label'] = 1 # match
    #df_candidates.rename(index={'idx_1':'ltable_id', 'idx_2':'rtable_id'}, inplace=True)
    logging.info('number of candidate pairs in data frame=%s, matches=%s', len(df_candidates), len(df_candidates[df_candidates['label'] == 1]))

    # create split
    train_size = 0.6
    train, rest = sklearn.model_selection.train_test_split(df_candidates, train_size=train_size, shuffle=True, stratify=df_candidates['label'])
    val_size = 0.2
    rest_size = val_size / (1 - train_size)
    val, test = sklearn.model_selection.train_test_split(rest, train_size=rest_size, shuffle=True, stratify=rest['label'])
    logging.info('train / val / test = %s / %s / %s', len(train), len(val), len(test))
    logging.info('matches = %s / %s / %s', len(train[train['label'] == 1]), len(val[val['label'] == 1]), len(test[test['label'] == 1]))

    train.to_csv('../tmp/train.csv', index=True)
    val.to_csv('../tmp/valid.csv', index=True)
    test.to_csv('../tmp/test.csv', index=True)

def start_coordinate():
    # http = False: agent call each other by direct Python function calls instead of HTTP requests
    # http = True: HTTP requests are used for calling agents
    http = False

    #use this config file to test the complete pipeline including knowledge enrichment
    config_file = './tests/conf/conf_power_plant_DEU_auto_no_pickl.json'
    #use this config file to test instance matching with the pickled RDF graph including geocoordinates
    #config_file = './tests/conf/conf_power_plant_DEU_auto.json'

    sys.argv.extend(['--config', config_file])
    params, _ = ontomatch.utils.util.init()

    # set matching name to None to finish without instance matching
    # this option is for testing knowledge enrichment only
    #params['matching']['name'] = None

    # write the config params to the blackboard
    params_str = json.dumps(params)
    config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(config_file, params_str, http)

    call_agent_coordinator(config_handle, http)

def call_agent_coordinator(config_handle, http=False):
    logging.info('calling ontomatch.coordinator.Agent, config_handle=%s, http=%s', config_handle, http)
    if http:
        raise NotImplementedError()
    else:
        ontomatch.coordinator.Agent().start(config_handle, http=False)
    logging.info('called ontomatch.coordinator.Agent')

if __name__ == '__main__':

    #start_pickle_dump()

    create_train_valid_test_split_files()

    #start_coordinate()
