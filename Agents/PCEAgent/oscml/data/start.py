import argparse
import logging
import os

import pandas as pd

import oscml.utils.util
import oscml.data.dataset_cep
import oscml.data.dataset_hopv15

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--src', type=str, default='.')
    parser.add_argument('--dst', type=str, default='.')
    parser.add_argument('--task', type=int, choices=range(1,8), required=True)
    args = parser.parse_args()

    oscml.utils.util.init_logging('.', '.')

    try:
        logging.info('starting task=%s', vars(args))

        if not args.dst == '.':
            dir_name = os.path.dirname(args.dst)
            try:
                os.makedirs(dir_name, exist_ok=True)
            except FileExistsError:
                logging.info('destination dir already exists, dir=%s', dir_name)

        if args.task==1:
            # python ./oscml/data/start.py --task 1 --src ./data/raw/CEPDB.csv --dst ./data/processed/CEPDB_valid_SMILES.csv
            oscml.data.dataset_cep.store_CEP_with_valid_SMILES(args.src, args.dst, numbersamples=-1)
        elif args.task==2:
            # python ./oscml/data/start.py --task 2 --src ./data/processed/CEPDB_valid_SMILES.csv --dst ./data/processed/CEPDB_25000_stratified.csv
            # skip all samples with PCE values <= 0.0001
            oscml.data.dataset_cep.store_CEP_cleaned_and_stratified(
                    args.src, args.dst, number_samples=[15000, 5000, 5000], threshold_skip=0.0001)
        elif args.task==3:
            # python ./oscml/data/start.py --task 2 --src ./data/processed/CEPDB_valid_SMILES.csv --dst ./data/processed/CEPDB_25000_stratified.csv
            # 1. skip all samples with PCE values <= 0.0001
            # 2. remove 30% of all samples with PCE values <= 4.0
            oscml.data.dataset_cep.store_CEP_cleaned_and_stratified(
                    args.src, args.dst, number_samples=[15000, 5000, 5000], threshold_skip=0.0001,
                    threshold_downsampling=4.0, threshold_percentage=0.3)
        elif args.task==4:
            info_cep = oscml.data.dataset_cep.create_dataset_info_for_CEP25000()
            oscml.data.dataset_hopv15.generate_dictionaries(args.src, 'smiles', info_cep)
        elif args.task==5:
            # python ./oscml/data/start.py --task 5 --src ./data/processed/CEPDB_valid_SMILES.csv --dst ./data/processed/CEPDB_25000_random_seed_100.csv
            # skip all samples with PCE values <= 0.0001
            oscml.data.dataset_cep.store_CEP_cleaned_and_random(
                    args.src, args.dst, number_samples=[15000, 5000, 5000], threshold_skip=0.0001, random_state=100)
        elif args.task==6:
            df = pd.read_csv('./data/processed/HOPV_15_revised_2_processed_homo.csv')
            oscml.data.dataset.add_k_fold_columns(df, 5, seed=200, column_name_prefix='ml_phase')
            oscml.data.dataset.store(df, './data/processed/HOPV_15_revised_2_processed_homo_5fold.csv')
        elif args.task==7:
            # python ./oscml/data/start.py --task 7 --src ./data/processed/HOPV_15_revised_2_processed_homo.csv --dst ./data/processed/HOPV_15_fingerprints_1048.csv
            # python ./oscml/data/start.py --task 7 --src ./data/processed/CEPDB_25000.csv --dst ./data/processed/CEPDB_25000_fingerprints_1048.csv
            df = pd.read_csv(args.src)
            #df = oscml.data.dataset.add_fingerprint_columns(df, 'smiles', 1048, 2)
            df = oscml.data.dataset.add_fingerprint_columns(df, 'SMILES_str', 1048, 2)
            oscml.data.dataset.store(df, args.dst)


    except Exception as exc:
        logging.exception('task failed', exc_info=True)
        raise exc
    else:
        logging.info('tasked finished successfully')