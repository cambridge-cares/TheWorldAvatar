import logging
from sklearn.ensemble import RandomForestRegressor
from py4ml.utils.util_sklearn import train_model_hpo, best_model_retraining
from py4ml.hpo.objclass import Objective
from py4ml.hpo.hpo_utils import preproc_training_params, BL_model_train, sample_params
from py4ml.hpo.hpo_utils import BL_model_train_cross_validate, \
                                BL_bestTrialRetrainDataPreproc, \
                                BL_loadModelFromCheckpoint, BL_ModelPredict
import pandas as pd
import numpy as np


def getObjectiveRF(
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

    objectiveRF = Objective(modelName=modelName, data=data, config=config,
                        logFile=logFile, logDir=logDir, logHead=logHead)

    # add goal and model specific settings
    if bestTrialRetraining:
        objectiveRF = addBestTrialRetrainingSettings(objectiveRF)
    elif modelPredict:
        objectiveRF = addModelPredictSettings(objectiveRF)
    else:
        objectiveRF = addHpoSettings(objectiveRF, crossValidation)
    return objectiveRF

def addBestTrialRetrainingSettings(objective):
    objective.data = BL_bestTrialRetrainDataPreproc(objective.data)
    objective.setModelCreator(funcHandle=model_create)
    objective.setModelTrainer(funcHandle=BL_model_train,extArgs=[data_preproc, best_model_retraining])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    return objective

def addModelPredictSettings(objective):
    objective.setModelCreator(funcHandle=BL_loadModelFromCheckpoint)
    objective.addPostModelCreateTask(objParamsKey='predictDataPreproc', funcHandle=predict_data_preproc)
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
    metric = objParams['training']['metric']

    model_params = sample_params(
        model_params=objConfig['config']['model']['model_specific'],
        trial = trial
    )


    logging.info('model params=%s', model_params)
    model = RandomForestRegressor(**model_params, criterion=metric, n_jobs=1, verbose=0)#, random_state=0)
    return model

def predict_data_preproc(trial, model, data, objConfig, objParams):
    model_params = objParams['model_params']
    transformer = model_params.get('transformer')
    if transformer:
        data = transformer.transform_x(pd.DataFrame(data))
    return data

def data_preproc(trial, data, objConfig, objParams):
    x_column = data['x_column']
    y_column = data['y_column']

    objParams['model_params'] = None

    data_processed = {
        'train': None,
        'val': None,
        'test': None,
        'transformer': data['transformer'],
    }
    data_processed['train'] = get_xy(data['train'], x_column, y_column)
    if data['val'] is not None:
        data_processed['val'] = get_xy(data['val'], x_column, y_column)
    data_processed['test'] = get_xy(data['test'], x_column, y_column)

    data_processed = {**data, **data_processed}
    return data_processed

def get_xy(df, column_x, column_y):
    x = np.array(df[column_x])
    y = np.array(df[column_y])

    return (x, y)