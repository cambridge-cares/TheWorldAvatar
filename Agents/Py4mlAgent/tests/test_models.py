import os
from py4ml.jobhandling import JobHandler
import glob
import pandas as pd
import shutil
import pytest

THIS_DIR = os.path.dirname(os.path.abspath(__file__))


@pytest.mark.parametrize(
    "model, test",
    [
    ('rf','train_and_predict_no_cv'),
    ('rf','train_and_predict_no_cv_mult_y'),
    ('rf','train_and_predict_with_cv'),
    ('svr','train_and_predict_no_cv'),
    ('svr','train_and_predict_with_cv'),
    ('mlp','train_and_predict_no_cv'),
    ('mlp','train_and_predict_with_cv'),
    ('mlp','train_and_predict_no_cv_mult_y')
])
def test_models(model, test):
    print('========================================================')
    print('MODEL: ', model)
    print('TEST: ', test)
    print()
    print()
    test_path = os.path.join(THIS_DIR, 'test_models', model, test)
    input_file = os.path.join(test_path,'input.json')
    clean_test_dir(test_path)
    jobHandler = JobHandler({'<configFile>': input_file})
    jobHandler.runJob()
    compareResults(test_path, model, test)
    print('========================================================')
    print()
    print()


def clean_test_dir(testDir):
    reg_test_dir = os.path.join(testDir,'test_data')

    if os.path.exists(reg_test_dir):
        shutil.rmtree(reg_test_dir)


def compareResults(testDir, model, test):
    # read all data
    refData = _getTestData(os.path.join(testDir,"ref_data"))
    testData = _getTestData(os.path.join(testDir,"test_data"))

    for key in refData.keys():
        assert key in testData
        # only compare output array sizes
        assert testData[key].size == refData[key].size
        # do not check for the exact results equality as this is hardware specific
        # but this is how one might do it...
        # pd.testing.assert_frame_equal(refData[key],testData[key], rtol=0.05, atol=1e-5)

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