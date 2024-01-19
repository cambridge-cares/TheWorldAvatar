import logging
import unittest

import torch
from tqdm import tqdm

import oscml.data.dataset
import oscml.data.dataset_cep
import oscml.data.dataset_hopv15
import oscml.models.model_bilstm
import oscml.models.model_gnn
import oscml.utils.util
from oscml.utils.util import smiles2mol

class TestModels(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        print()
        print()
        print('###################################')
        print('#           Models Tests          #')
        print('###################################')
        print()
        print()
        oscml.utils.util.init_logging('./tests', './tests/tests_logs')

    def test_out_of_vocabulary(self):
        """
        This test starts with the fragments of CEPDB (which contains 8 different atom types)
        and allows updating fragments on-the-fly by setting oov=True.
        Run the fragment mapping on HOPV15 and assert that 340 molecules from HOPV15
        contain different fragment types and the number of different atom types
        has increased to 12.
        """
        path = oscml.data.dataset.path_hopv_15()
        df_hopv15 = oscml.data.dataset_hopv15.read(path)

        info_cep = oscml.data.dataset_cep.create_dataset_info_for_CEP25000()
        wf = info_cep.mol2seq.wf
        mol2seq_without_OOV = oscml.models.model_bilstm.Mol2seq(radius=1, oov=False, wf=wf)
        mol2seq_with_OOV = oscml.models.model_bilstm.Mol2seq(radius=1, oov=True, wf=wf)
        logging.info(mol2seq_with_OOV.atom_dict)
        assert len(mol2seq_with_OOV.atom_dict) == 8

        max_index = len(wf['fragment_dict'])-1
        count_larger = 0
        for i in range(len(df_hopv15)):
            smiles = df_hopv15.iloc[i]['smiles']
            m = smiles2mol(smiles)
            #print(smiles, m)
            seq_without_OOV = mol2seq_without_OOV(m)
            seq_with_OOV = mol2seq_with_OOV(m)
            #print(seq_without_OOV)
            #print('MY SEQ WITH OOV', seq_with_OOV)
            assert len(seq_without_OOV) == len(seq_with_OOV)
            larger_max_index = False
            for j in range(len(seq_without_OOV)):
                fragment_index = seq_without_OOV[j]
                if (fragment_index > max_index):
                    larger_max_index = True
                    # check that the index of the fragment was set to -1 (for OOV)
                    assert seq_with_OOV[j] == -1
                else:
                    assert seq_with_OOV[j] == fragment_index
            if larger_max_index:
                count_larger += 1

        logging.info(mol2seq_with_OOV.atom_dict)
        assert len(mol2seq_with_OOV.atom_dict) == 12

        logging.info('number of molecules with at least one new atom type=%s', count_larger)
        assert count_larger == 340

    def test_bilstm_model_forward(self):
        path = oscml.data.dataset.path_cepdb_25000()
        df_train, _, _ = oscml.data.dataset.read_and_split(path)
        transformer = oscml.data.dataset.create_transformer(df_train, column_target='pce', column_x='SMILES_str')

        info_cep = oscml.data.dataset_cep.create_dataset_info_for_CEP25000()
        wf = info_cep.mol2seq.wf
        oscml.models.model_bilstm.Mol2seq(radius=1, oov=True, wf=wf)

        model_params =  {
            'number_of_subgraphs': 60,
            'embedding_dim': 128,
            'lstm_hidden_dim': 128,
            'mlp_units': [256, 32, 32, 32, 1],
            'padding_index': 0,
            'target_mean': transformer.target_mean,
            'target_std': transformer.target_std,
        }

        optimizer = {
            'name': 'Adam',             # Adam, SGD, RMSProp
            'lr': 0.001,
            'momentum': 0,              # SGD and RMSProp only
            'weight_decay': 0,
            'nesterov': False,          # SGD only
        }

        model = oscml.models.model_bilstm.BiLstmForPce(**model_params, optimizer=optimizer)

        batch = torch.LongTensor([[1,2,3], [4,5,6]]) #.to(device)
        output = model(batch)
        print(output)

    def internal_test_bilstm_dataloader(self, type_dict, src, x_column):

        if type_dict == oscml.data.dataset_cep.CEP25000:
            max_sequence_length = 60
            split = 'ml_phase'
        else:
            max_sequence_length = 150
            split = [200,None,36]

        dataset_config = {
            "src": src,
            "z-stand": "False",
            "x_column": [x_column],
            "y_column": ["pce"],
            'split': split
        }

        logging.info('dataset=%s, max sequence length=%s', dataset_config, max_sequence_length)
        df_train, df_val, df_test, transformer = oscml.data.dataset.get_dataframes(dataset=dataset_config)

        train_dl, _, _ = oscml.models.model_bilstm.get_dataloaders(type_dict, df_train, df_val, df_test,
            transformer, batch_size=40, max_sequence_length=max_sequence_length)

        for batch in train_dl:
            self.assertEqual(40, len(batch[0]))
            self.assertEqual(max_sequence_length, len(batch[0][0]))
            self.assertEqual(40, len(batch[1]))
            break

    def test_bilstm_dataloader_for_hopv15(self):
        print()
        print()
        print('------------------------------------------------------')
        print('-       Test: test_bilstm_dataloader_for_hopv15      -')
        print('------------------------------------------------------')
        print()
        print()
        self.internal_test_bilstm_dataloader(type_dict=oscml.data.dataset_hopv15.HOPV15, src='./data/processed/HOPV_15_revised_2_processed_homo_5fold.csv', x_column='smiles')

    def test_bilstm_dataloader_for_cep25000(self):
        print()
        print()
        print('--------------------------------------------------------')
        print('-       Test: test_bilstm_dataloader_for_cep25000      -')
        print('--------------------------------------------------------')
        print()
        print()
        self.internal_test_bilstm_dataloader(type_dict=oscml.data.dataset_cep.CEP25000, src='./data/processed/CEPDB_25000.csv', x_column='SMILES_str')

    def test_profile_gnn_dataloader_for_cep25000(self):
        print()
        print()
        print('--------------------------------------------------------')
        print('-     Test: test_profile_gnn_dataloader_for_cep25000   -')
        print('--------------------------------------------------------')
        print()
        print()
        dataset_config = {
            "src": "./data/processed/CEPDB_25000.csv",
            "z-stand": "False",
            "x_column": ["SMILES_str"],
            "y_column": ["pce"],
            'split': "ml_phase"
        }

        df_train, df_val, df_test, transformer = oscml.data.dataset.get_dataframes(dataset=dataset_config)

        #df_train = df_train[:2500]

        train_dl, _, _ = oscml.models.model_gnn.get_dataloaders(oscml.data.dataset_cep.CEP25000, df_train, df_val, df_test,
            transformer, batch_size=250)

        for batch in tqdm(train_dl):
            pass

        logging.info('finished iteration 1')

        for batch in tqdm(train_dl):
            pass