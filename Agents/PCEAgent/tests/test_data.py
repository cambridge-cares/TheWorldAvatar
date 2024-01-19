import unittest

import pandas as pd
from tqdm import tqdm

import oscml.data.dataset
import oscml.data.dataset_cep
import oscml.data.dataset_hopv15
import oscml.utils.util
from oscml.utils.util import smiles2mol


class TestData(unittest.TestCase):

    def assert_PCE_values(self, df_100, df):
        for i in range(len(df_100)):
            df_100_pce = df_100['id'].iloc[i]
            pce = df['id'].iloc[i]
            self.assertEqual(df_100_pce, pce)

    def assertEqualArray(self, a, b):
        self.assertEqual(len(a), len(b))
        for i in range(len(a)):
            self.assertEqual(a[i],b[i])

    @classmethod
    def setUpClass(cls):
        print()
        print()
        print('###################################')
        print('#           Data Tests            #')
        print('###################################')
        print()
        print()
        oscml.utils.util.init_logging('./tests', './tests/tests_logs')

    def setUp(self):
        self.path_CEPDB = oscml.data.dataset.path_cepdb_valid_smiles()
        self.path_CEPDB_25000 = oscml.data.dataset.path_cepdb_25000()

    def test_dataset_read_cep_25000(self):
        print()
        print()
        print('------------------------------------------------')
        print('-       Test: test_dataset_read_cep_25000      -')
        print('------------------------------------------------')
        print()
        print()
        df_train, df_val, df_test = oscml.data.dataset.read_and_split(self.path_CEPDB_25000)
        assert len(df_train) == 15000
        assert len(df_val) == 5000
        assert len(df_test) == 5000

    def test_dataset_transform_cep_25000(self):
        print()
        print()
        print('------------------------------------------------')
        print('-    Test: test_dataset_transform_cep_25000    -')
        print('------------------------------------------------')
        print()
        print()
        df_train, _, _ = oscml.data.dataset.read_and_split(self.path_CEPDB_25000)
        transformer = oscml.data.dataset.create_transformer(df_train, column_target='pce', column_x='SMILES_str')
        self.assertAlmostEqual(4.120434375131375, transformer.target_mean, 1)
        self.assertAlmostEqual(2.405561853258728, transformer.target_std, 1)

    def test_dataset_update_state(self):
        print()
        print()
        print('------------------------------------------------')
        print('-       Test: test_dataset_update_state        -')
        print('------------------------------------------------')
        print()
        print()
        mol2seq = oscml.features.weisfeilerlehman.Mol2seq_WL(radius=1)
        info = oscml.data.dataset.DatasetInfo(mol2seq=mol2seq)

        smiles = '[SiH2]1C=c2c3cc([se]c3c3cc4ccccc4cc3c2=C1)-c1cncs1'
        mol = smiles2mol(smiles)
        info.update(mol, smiles)
        self.assertEqual(38, info.max_molecule_size)
        self.assertEqual(50, info.max_smiles_length)
        self.assertEqual(16, len(info.mol2seq.fragment_dict))
        self.assertEqual(7, len(info.node_types))

        smiles = '[SiH2]1cc2cccc(-c3ccc(-c4scc5[nH]ccc45)c4nsnc34)c2c1'
        mol = smiles2mol(smiles)
        info.update(mol, smiles)
        self.assertEqual(39, info.max_molecule_size)
        self.assertEqual(52, info.max_smiles_length)
        self.assertEqual(7, len(info.node_types))

    def test_dataset_info_for_cepdb_25000(self):
        print()
        print()
        print('------------------------------------------------')
        print('-   Test: test_dataset_info_for_cepdb_25000    -')
        print('------------------------------------------------')
        print()
        print()
        # check the correct size of dictionaries
        info = oscml.data.dataset_cep.create_dataset_info_for_CEP25000()
        number_node_types = len(info.node_types)
        self.assertEqual(8, number_node_types)
        number_fragment_types = len(info.mol2seq.fragment_dict)
        self.assertEqual(56, number_fragment_types)

        # read subset from CEPDB
        df = pd.read_csv(self.path_CEPDB_25000)
        for i in tqdm(range(len(df))):
            smiles = df.iloc[i]['SMILES_str']
            m = smiles2mol(smiles)
            info.update(m, smiles)

        # check that there are no additional node or fragment types
        number_node_types = len(info.node_types)
        self.assertEqual(8, number_node_types)
        number_fragment_types = len(info.mol2seq.fragment_dict)
        self.assertEqual(56, number_fragment_types)

    def test_dataset_info_for_hopv15(self):
        print()
        print()
        print('------------------------------------------------')
        print('-     Test: test_dataset_info_for_hopv15       -')
        print('------------------------------------------------')
        print()
        print()
        # check the correct size of dictionaries
        info = oscml.data.dataset_hopv15.create_dataset_info_for_HOPV15()
        number_node_types = len(info.node_types)
        self.assertEqual(12, number_node_types)
        number_fragment_types = len(info.mol2seq.fragment_dict)
        self.assertEqual(150, number_fragment_types)

        # the fragments and node type were added to existing ones from CEP DB
        # compare the results when starting from scratich
        path = oscml.data.dataset.path_hopv_15()
        info_from_scratch = oscml.data.dataset_hopv15.generate_dictionaries(path, 'smiles', None)
        number_fragment_types = len(info_from_scratch.mol2seq.fragment_dict)
        self.assertEqual(134, number_fragment_types)
        # that means there are 16 fragments in CEP DB that are not used in HOPV15

    def test_sample_without_replacement(self):
        df = pd.read_csv(self.path_CEPDB)
        df_cleaned = oscml.data.dataset_cep.skip_all_small_pce_values(df.copy(), 0.0001)
        df_train, _ = oscml.data.dataset_cep.sample_without_replacement(df_cleaned, number_samples=1000, step=1.)
        self.assertEqual(1000, len(df_train))

        df_cleaned = oscml.data.dataset_cep.skip_all_small_pce_values(df.copy(), 0.0001)
        df_train, df_val, df_test = oscml.data.dataset_cep.sample_without_replacement(df_cleaned, number_samples=[1000, 200, 300], step=.2)
        self.assertEqual(1000, len(df_train))
        self.assertEqual(200, len(df_val))
        self.assertEqual(300, len(df_test))

    def test_store_CEP_cleaned_and_stratified(self):
        df = oscml.data.dataset_cep.store_CEP_cleaned_and_stratified(
            self.path_CEPDB, dst=None, number_samples=[15000, 5000, 5000], threshold_skip=0.0001)
        self.assertEqual(25000, len(df))
        mask = (df['ml_phase'] == 'train')
        self.assertEqual(15000, len(df[mask]))

    def test_add_k_fold_columns(self):
        file = './data/processed/HOPV_15_revised_2_processed_homo.csv'
        df = pd.read_csv(file)
        k = 5
        oscml.data.dataset.add_k_fold_columns(df, k, seed=200, column_name_prefix='ml_phase')
        size = len(df)
        mask = [False]*size
        for i in range(k):
            column = 'ml_phase_fold_' + str(i)
            mask = (mask | (df[column] == 'test'))
        self.assertTrue(all(mask))

    def test_add_fingerprint_columns(self):
        file = './data/processed/HOPV_15_revised_2_processed_homo.csv'
        df = pd.read_csv(file)[:4]

        print(df['smiles'])

        nBits = 128
        expected_number_columns = len(df.columns) + 128

        df = oscml.data.dataset.add_fingerprint_columns(df, 'smiles', nBits, 2)
        self.assertEqualArray([0,0,0,0], df['fp0'].to_numpy())
        self.assertEqualArray([1,0,1,1], df['fp3'].to_numpy())
        self.assertEqual(expected_number_columns, len(df.columns))


if __name__ == '__main__':
    unittest.main()

    #suite = unittest.TestSuite()
    #suite.addTest(TestData('test_dataset_info_for_cepdb_25000'))
    #suite.addTest(TestData('test_dataset_info_for_hopv15'))
    #suite.addTest(TestData('test_dataset_transform_cep_25000'))
    #suite.addTest(TestData('test_dataset_skip_invalid_smiles'))
    #suite.addTest(TestData('test_add_k_fold_columns'))
    #suite.addTest(TestData('test_add_fingerprint_columns'))
    #runner = unittest.TextTestRunner()
    #runner.run(suite)