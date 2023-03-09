import unittest
import json
from collections import OrderedDict
import functools
import os
import optuna
from oscml.utils.util_config import set_config_param
from time import sleep

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
test_config_file = os.path.join(THIS_DIR,'test_confhpo','config_test.json')

class TestConfigparser(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        print()
        print()
        print('###################################')
        print('#         Config Parser Tests     #')
        print('###################################')
        print()
        print()

    def test_config_parser(self):
        def test_objective(trial, config_params):
            processed_params = {}
            for key, value in config_params['config_parser_test'].items():
                processed_params.update({key: set_config_param(trial=trial,param_name=key,param=value, all_params=processed_params)})

            # param_1
            assert processed_params['param_1'] == 1
            # param_2
            assert processed_params['param_2'] == 4
            # param_3
            assert processed_params['param_3'] == processed_params['param_1']
            # param_4
            assert processed_params['param_4'] == [1,3,4]
            # param_5
            len_p4 = len(processed_params['param_4'])
            len_p5 = len(processed_params['param_5'])
            len_p5_attr = config_params['config_parser_test']['param_5']['length']
            assert len_p5 == len_p5_attr
            assert processed_params['param_5'][0:len_p4] == processed_params['param_4']
            assert processed_params['param_5'][len_p4:len_p5-1] == [processed_params['param_4'][-1]]*(len_p5-len_p4-1)
            # param_6
            assert processed_params['param_6'] == [1]
            # param_7
            assert processed_params['param_7'] == [1,3,4]
            # param_8
            assert processed_params['param_8'] == [1,3,4,4]
            # param_9
            assert processed_params['param_9'][0] == 1
            assert len(processed_params['param_9']) == 4
            i, j = config_params['config_parser_test']['param_9']['low'], config_params['config_parser_test']['param_9']['high']
            assert all(ele >= i and ele <= j for ele in processed_params['param_9']) == True
            # param_10
            assert processed_params['param_10'] == processed_params['param_4']
            # param_11
            assert processed_params['param_11'][0:3] == processed_params['param_4']
            i, j = config_params['config_parser_test']['param_11']['low'], config_params['config_parser_test']['param_11']['high']
            assert processed_params['param_11'][0:3] == processed_params['param_4']
            assert all(ele >= i and ele <= j for ele in processed_params['param_11'][3:]) == True
            # param_12
            i, j = config_params['config_parser_test']['param_12']['low'], config_params['config_parser_test']['param_12']['high']
            p9_2 = processed_params['param_9'][2]
            len_p7 = len(processed_params['param_7'])
            len_p12 = len(processed_params['param_12'])
            assert len_p12 == len_p7
            assert processed_params['param_12'][0] == p9_2
            # param_13
            len_p13 = len(processed_params['param_13'])
            for i in range(1,len_p13):
                assert processed_params['param_13'][i-1] >= processed_params['param_13'][i]
            # param_14
            len_p14 = len(processed_params['param_14'])
            for i in range(1,len_p13):
                assert processed_params['param_14'][i-1] <= processed_params['param_14'][i]
            # param_15
            p_15_0 = processed_params['param_15'][0]
            len_p15 = len(processed_params['param_15'])
            assert processed_params['param_15'] == [p_15_0]*len_p15
            # param_16
            assert processed_params['param_16'] == [1,1,1,1]
            # param_17
            len_p17 = len(processed_params['param_17'])
            p_17_0 = processed_params['param_17'][0]
            assert p_17_0 == 1
            for i in range(1,len_p17):
                assert processed_params['param_17'][i-1] <= processed_params['param_17'][i]
            # param_18
            len_p18 = len(processed_params['param_18'])
            p_18_0 = processed_params['param_18'][0]
            assert p_18_0 == 10
            for i in range(1,len_p18):
                assert processed_params['param_18'][i-1] >= processed_params['param_18'][i]

            assert processed_params['param_19'] == [2,2,2,2]
            assert processed_params['param_20'] == [2,3,5,5]
            #assert (k in trial.params for k in ('param_9_1','param_9_2','param_9_3')) == True

            sleep(1)
            return 1.0

        print()
        print()
        print('-----------------------------------')
        print('-     Test: test_config_parser    -')
        print('-----------------------------------')
        print()
        print()
        #--------------------------------------
        with open(test_config_file) as json_config:
            config_params = json.load(json_config, object_pairs_hook=OrderedDict)

        obj = functools.partial(test_objective, config_params=config_params)

        study = optuna.study.create_study()
        study.optimize(obj, n_trials=1,gc_after_trial=True, timeout=1)

        print('done')