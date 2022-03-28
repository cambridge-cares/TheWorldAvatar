import json
import logging
import pickle
import random
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
    #config_file = './conf/restaurant/conf_restaurant_auto_huge.json'
    #save_file = '../tmp/split_fz_mtf_20.csv'
    #config_file = './conf/bibliography/conf_bibda_auto_huge.json'
    #save_file = '../tmp/split_da_mtf_200.csv'
    #config_file = './conf/bibl_DBLP_Scholar/conf_bibds_auto_huge.json'
    #save_file = '../tmp/split_ds_mtf_200.csv'
    config_file = './conf/product/conf_product_auto_huge.json'
    save_file = '../tmp/split_ag_mtf_200.csv'

    #mtf = 20
    #train_sizes = [0.0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.8]
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

def create_train_test_split_file_for_ratios():

    #split_file = './conf/power_plant_DEU/split_kg_mtf_200.csv'
    #save_file = '../tmp/split_kg_mtf_200_ratios.csv'
    # KG with MTF=200 --> N-M-ratio = 42.61
    #ratios = [1, 2, 5, 10, 20, 43]

    #split_file = './conf/power_plant_GBR/split_dg_mtf_200.csv'
    #save_file = '../tmp/split_dg_mtf_200.csv'
    # DG with MTF=200 --> N-M-ratio = N-M-ratio=87.21
    #ratios = [1, 2, 5, 10, 20, 50, 87]

    #split_file = './conf/restaurant/split_fz_mtf_20.csv'
    #save_file = '../tmp/split_fz_mtf_20_ratios.csv'
    # DS with MTF=20 --> N-M-ratio = 22.64
    #ratios = [1, 2, 5, 10, 20, 23]
    # DS with MTF=200 --> N-M-ratio = 211.45
    #ratios = [1, 2, 5, 10, 20, 23, 50, 100, 200, 211]

    split_file = './conf/bibl_DBLP_Scholar/split_ds_mtf_200.csv'
    save_file = '../tmp/split_ds_mtf_200.csv'
    # DS with MTF=200 --> N-M-ratio = 134.7
    ratios = [1, 2, 5, 10, 20, 50, 100, 135]

    #split_file = './conf/bibliography/split_da_mtf_200.csv'
    #save_file = '../tmp/split_da_mtf_200_ratios.csv'
    # DA with MTF=200 --> N-M-ratio = 219.64
    #ratios = [1, 2, 5, 10, 20, 50, 100, 200, 220]

    #split_file = './conf/product/split_ag_mtf_200.csv'
    #save_file = '../tmp/split_ag_mtf_200.csv'
    # AG with MTF=200 --> N-M-ratio = 189.69
    #ratios = [1, 2, 5, 10, 20, 50, 100, 190]

    split_column = 'ml_phase_0.1'


    df_split = ontomatch.utils.util.read_csv(split_file)

    count_nonmatches = len(df_split[df_split['y'] == 0])
    count_matches = len(df_split[df_split['y'] == 1])
    ratio = count_nonmatches / count_matches
    logging.info('read %s , nonmatches=%s, matches=%s, ratio=%s', split_file, count_nonmatches, count_matches, ratio)

    # use the same matches in the training set as in the stratified split file
    mask = (df_split[split_column] == 'train') & (df_split['y'] == 1)
    index_train_matches = df_split[mask].index

    # sample the nonmatches for training randomly from the set of all nonmatches in the candidate set
    # there are two ways to define the set of all nonmatches
    # 1. exclude all true matches, this was already done for the split file:
    index_nonmatches = df_split[df_split['y'] == 0].index.to_list()
    # 2. exclude only those true matches that are in the training set:
    #index_nonmatches = df_split.index.difference(index_train_matches)
    # We choose option 1 for the following reasons:
    # a) If the candidate set is large and the ratio is relatively small, it is very likely that only a neglectible
    # number of true matches is declared as nonmatches for training.
    # b) Of course, in a real-world project, the set of all true matches is unknown in advance. If the condition in a)
    # is not valid, option 2 gives the classifier an advantage (leaking information). However, we want to study
    # the influence of the ratio on F1 measure here and for this, we exclude possible other factors, such as
    # the influence of declaring matches as false nonmatches.

    for ratio in ratios:
        # sample without replacement
        count_nonmatches = ratio * len(index_train_matches)
        list_index_train_nonmatches = random.sample(index_nonmatches, count_nonmatches)
        index_train_nonmatches = pd.MultiIndex.from_tuples(list_index_train_nonmatches)
        index_train = index_train_matches.union(index_train_nonmatches)
        ratio_column = split_column + '_ratio_' + str(ratio)
        df_split[ratio_column] = 'test'
        df_split.at[index_train, ratio_column] = 'train'
        logging.info('added column=%s for ratio=%s: train matches=%s, train nonmatches=%s',
            ratio_column, ratio, len(index_train_matches), len(index_train_nonmatches))

    df_split.to_csv(save_file)


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

    #create_train_test_split_files()

    create_train_test_split_file_for_ratios()

    #start_coordinate()
