import json
import logging
import pickle
import sys

import pandas as pd

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

def create_train_test_split_files():

    #config_file = './conf/power_plant_DEU/conf_power_plant_DEU_auto_huge.json'
    #save_file = '../tmp/split_kg_mtf_200.csv'
    #config_file = './conf/power_plant_GBR/conf_power_plant_GBR_auto_huge.json'
    #save_file = '../tmp/split_dg_mtf_200.csv'
    #config_file = './conf/bibliography/conf_bibda_auto_huge.json'
    #save_file = '../tmp/split_da_mtf_200.csv'
    #config_file = './conf/bibl_DBLP_Scholar/conf_bibds_auto_huge.json'
    #save_file = '../tmp/split_ds_mtf_200.csv'
    config_file = './conf/product/conf_product_auto_huge.json'
    save_file = '../tmp/split_ag_mtf_100.csv'

    mtf = 200
    train_sizes = [0.0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.8]

    sys.argv.extend(['--config', config_file])
    params, _ = ontomatch.utils.util.init()

    src_file = params['dataset']['src']
    with open(src_file,'rb') as file:
        src_onto = pickle.load(file)
    tgt_file = params['dataset']['tgt']
    with open(tgt_file,'rb') as file:
        tgt_onto = pickle.load(file)

    matchfile = params['post_processing']['evaluation_file']
    index_matches = ontomatch.evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
    logging.info('ground truth matches=%s', len(index_matches))

    params_blocking = params['blocking']
    params_blocking['model_specific']['max_token_occurrences_src'] = mtf
    params_blocking['model_specific']['max_token_occurrences_tgt'] = mtf
    logging.debug('changed params_blocking=%s', params_blocking)

    iterator = ontomatch.blocking.create_iterator(src_onto, tgt_onto, params_blocking)
    index_pairs = iterator.candidate_matching_pairs
    logging.debug('number of candidate pairs=%s', len(index_pairs))

    # create empty DataFrame
    df_pairs = pd.DataFrame(index=index_pairs)
    logging.debug('number of candidate pairs in data frame=%s', len(df_pairs))

    # create splits
    columns_x = []
    for train_size in train_sizes:
        column_ml_phase = 'ml_phase_' + str(train_size)
        df_candidate_pairs, _, _, _, _ = ontomatch.utils.util.train_test_split(
                df_pairs, index_matches, train_size, columns_x, column_ml_phase=column_ml_phase)

    df_candidate_pairs.to_csv(save_file)

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

    create_train_test_split_files()

    #start_coordinate()
