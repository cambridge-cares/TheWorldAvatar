import logging
import sklearn.svm
from py4ml.hpo.hpo_utils import preproc_training_params, BL_model_train, sample_params
from py4ml.hpo.hpo_utils import BL_model_train_cross_validate, \
                                BL_bestTrialRetrainDataPreproc, \
                                BL_loadModelFromCheckpoint, \
                                BL_ModelPredict
from py4ml.hpo.objclass import Objective
from py4ml.utils.util_sklearn import train_model_hpo, best_model_retraining
import numpy as np
import pandas as pd

def getObjectiveSVR(
        modelName,
        data,
        config,
        logFile,
        logDir,
        logHead,
        crossValidation,
        bestTrialRetraining=False,
        transferLearning=False,
        modelPredict=False
    ):

    objectiveSVR = Objective(modelName=modelName, data=data, config=config,
                        logFile=logFile, logDir=logDir, logHead=logHead)

    # add goal and model specific settings
    if bestTrialRetraining:
        objectiveSVR = addBestTrialRetrainingSettings(objectiveSVR)
    elif modelPredict:
        objectiveSVR = addModelPredictSettings(objectiveSVR)
    else:
        objectiveSVR = addHpoSettings(objectiveSVR, crossValidation)
    return objectiveSVR

def addBestTrialRetrainingSettings(objective):
    objective.data = BL_bestTrialRetrainDataPreproc(objective.data)
    objective.setModelCreator(funcHandle=model_create)
    objective.setModelTrainer(funcHandle=BL_model_train,extArgs=[data_preproc, best_model_retraining])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    return objective

def addModelPredictSettings(objective):
    objective.setModelCreator(funcHandle=BL_loadModelFromCheckpoint)
    objective.addPostModelCreateTask(objParamsKey='predictDataPreproc', funcHandle=modelPredictDataPrepare)
    objective.addPostModelCreateTask(objParamsKey='predictModel', funcHandle=BL_ModelPredict)
    return objective

def addHpoSettings(objective, crossValidation):
    model_trainer_func = BL_model_train_cross_validate if crossValidation else BL_model_train

    objective.setModelCreator(funcHandle=model_create)
    objective.setModelTrainer(funcHandle=model_trainer_func,extArgs=[data_preproc, train_model_hpo])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    return objective

def model_create(trial, data, objConfig, objParams):
    # set model parameters from the config file
    #--------------------------------------
    """
        'kernel'
        'gamma_structural'
    """

    model_params = sample_params(
        model_params=objConfig['config']['model']['model_specific'],
        trial = trial
    )

    logging.info('model params=%s', model_params)

    model = sklearn.svm.SVR(**model_params)

    return model

def data_preproc(trial, data, objConfig, objParams):
    x_column = data['x_column']
    y_column = data['y_column']

    data_processed = {
        'train': None,
        'val': None,
        'test': None,
        'scaler': None,
        'transformer': data['transformer']
    }

    x, y = _data_preproc(
            data['train'], column_x=x_column, column_y=y_column)
    data_processed['train'] = (x, y)
    data_processed['scaler'] = None

    objParams['model_params'] = {'scaler': None}

    if data['val'] is not None:
        x, y = _data_preproc(
            data['val'], column_x=x_column, column_y=y_column)
        data_processed['val'] = (x,y)

    x, y= _data_preproc(
        data['test'], column_x=x_column, column_y=y_column)
    data_processed['test'] = (x,y)

    data_processed = {**data, **data_processed}
    return data_processed


def _data_preproc(df, column_x, column_y):
    x = np.array(df[column_x])
    y = np.array(df[column_y])

    return x, y

def modelPredictDataPrepare(trial, model, data, objConfig, objParams):
    data = np.array(data)
    model_params = objParams['model_params']
    transformer = model_params.get('transformer')
    if transformer:
        data = transformer.transform_x(pd.DataFrame(data))
    return data