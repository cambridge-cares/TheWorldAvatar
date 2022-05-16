import os
import time
import unittest
from oscml.jobhandling import JobHandler
import oscml.utils.util
import glob
import pandas as pd
import shutil
from parameterized import parameterized

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

class Test_HPO(unittest.TestCase):
    @parameterized.expand([
        ['rf','trials_5_no_cv'],
        ['rf','trials_1_cv_2'],
        ['rf','trials_2_cv_2'],
        ['rf','predict'],
        ['svr','trials_5_no_cv'],
        ['svr','trials_1_cv_2'],
        ['svr','trials_2_cv_2'],
        ['svr','predict'],
        ['simplegnn','trials_5_no_cv'],
        ['simplegnn','trials_1_cv_2'],
        ['simplegnn','trials_2_cv_2'],
        ['simplegnn','transfer'],
        ['simplegnn','predict'],
        ['bilstm','trials_5_no_cv'],
        ['bilstm','trials_1_cv_2'],
        ['bilstm','trials_2_cv_2'],
        ['bilstm','transfer'],
        ['bilstm','predict'],
        ['attentivefp','trials_5_no_cv'],
        ['attentivefp','trials_1_cv_2'],
        ['attentivefp','trials_2_cv_2'],
        ['attentivefp','transfer'],
        ['attentivefp','predict']
    ])
    def test_models(self, model, test):
        print('========================================================')
        print('MODEL: ', model)
        print('TEST: ', test)
        print()
        print()
        test_path = os.path.join(THIS_DIR, 'modelsRegressionTests', model, test)
        input_file = os.path.join(test_path,'input.json')
        clean_test_dir(test_path)
        jobHandler = JobHandler({'<configFile>': input_file})
        jobHandler.runJob()
        compareResults(test_path, model, test)
        print('========================================================')
        print()
        print()

def clean_test_dir(testDir):
    reg_test_dir = os.path.join(testDir,'reg_test')
    reg_test_db = os.path.join(testDir,'reg_test.db')

    if os.path.exists(reg_test_dir):
        shutil.rmtree(reg_test_dir)
    if os.path.isfile(reg_test_db):
        os.remove(reg_test_db)

def compareResults(testDir, model, test):
    msg_head = "model: "+model+" test: "+test+" "
    # read ref data

    refData = _getTestData(os.path.join(testDir,"ref_data"))
    regData = _getTestData(os.path.join(testDir,"reg_test"))

    for key in refData.keys():
        assert key in regData
        pd.testing.assert_frame_equal(refData[key],regData[key], rtol=0.05, atol=1e-5)

def _getTestData(testDir):
    testData = {}
    hpoDir = os.path.join(testDir,'hpo')
    bestTrialRetrainDir = os.path.join(testDir,'best_trial_retrain')
    transferLearningDir = os.path.join(testDir,'transfer_learning')
    modelPredictDir = os.path.join(testDir,'model_predict')
    if os.path.exists(hpoDir):
        df = pd.read_csv(os.path.join(hpoDir,'hpo_result.csv'))
        for col in ['datetime_start', 'datetime_complete','duration']:
            if col in df:
                df = df.drop([col], axis=1)
        testData['hpo'] = df
    if os.path.exists(bestTrialRetrainDir):
        bestTrialResultsDir = glob.glob(os.path.join(bestTrialRetrainDir,'trial_*'))[0]
        bestTrialNr = bestTrialResultsDir.split('trial_')[-1]
        df = pd.read_csv(os.path.join(bestTrialResultsDir,"best_trial_retrain_model.csv"))
        if 'time' in df:
            df = df.drop(['time'], axis=1)
        testData['bestTrialRetrain'+'_trial_'+str(bestTrialNr)] = df
    if os.path.exists(transferLearningDir):
        transferLearningResultsDir = glob.glob(os.path.join(transferLearningDir,'trial_*'))[0]
        transferLearningTrialNr = transferLearningResultsDir.split('trial_')[-1]
        df = pd.read_csv(os.path.join(transferLearningResultsDir,"transfer_learning_model.csv"))
        if 'time' in df:
            df = df.drop(['time'], axis=1)
        testData['transferLearning'+'_trial_'+str(transferLearningTrialNr)] = df
    if os.path.exists(modelPredictDir):
        df = pd.read_csv(os.path.join(modelPredictDir,'model_predict.csv'))
        testData['modelPredict'] = df
    return testData

if __name__ == '__main__':
    unittest.main()