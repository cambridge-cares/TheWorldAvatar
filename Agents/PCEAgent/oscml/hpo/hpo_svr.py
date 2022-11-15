import logging
import oscml.models.model_kernel
from oscml.utils.util_config import set_config_param
from oscml.models.model_kernel import preprocess_data_phys_and_struct
from oscml.hpo.hpo_utils import preproc_training_params, BL_model_train
from oscml.hpo.hpo_utils import BL_model_train_cross_validate, \
                                BL_bestTrialRetrainDataPreproc, \
                                BL_loadModelFromCheckpoint, \
                                BL_ModelPredict
from oscml.hpo.objclass import Objective
from oscml.utils.util_sklearn import train_model, train_model_hpo, best_model_retraining
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
    model_conf = objConfig['config']['model']['model_specific']
    model_params = {}
    for key, value in model_conf.items():
        model_params.update({key: set_config_param(trial=trial,param_name=key,param=value, all_params=model_params)})

    logging.info('model params=%s', model_params)

    model = oscml.models.model_kernel.SVRWrapper(**model_params)

    return model

def data_preproc(trial, data, objConfig, objParams):
    # set fingerprint parameters from the config file
    #--------------------------------------
    fp_conf = objConfig['config']['model']['fingerprint_specific']

    fp_params = {}
    for key, value in objConfig['config']['model']['fingerprint_specific'].items():
        fp_params.update({key: set_config_param(trial=trial,param_name=key,param=value, all_params=fp_params)})

    logging.info('fingerprting params=%s', fp_params)

    x_column = data['x_column'][0]
    y_column = data['y_column'][0]

    data_processed = {
        'train': None,
        'val': None,
        'test': None,
        'scaler': None,
        'transformer': data['transformer']
    }

    x, y, scaler_svr_physical_data= preprocess_data_phys_and_struct(
            data['train'], fp_params, train_size=1, column_smiles=x_column,
            columns_phys=None, column_y=y_column)
    data_processed['train'] = (x, y)
    data_processed['scaler'] = scaler_svr_physical_data

    objParams['model_params'] = {'fp_params': fp_params, 'scaler':scaler_svr_physical_data}

    if data['val'] is not None:
        x, y, _= preprocess_data_phys_and_struct(
            data['val'], fp_params, train_size=1, column_smiles=x_column,
            columns_phys=None, column_y=y_column, scaler_svr_physical_data=scaler_svr_physical_data)
        data_processed['val'] = (x,y)

    x, y, _= preprocess_data_phys_and_struct(
        data['test'], fp_params, train_size=1, column_smiles=x_column,
        columns_phys=None, column_y=y_column, scaler_svr_physical_data=scaler_svr_physical_data)
    data_processed['test'] = (x,y)

    data_processed = {**data, **data_processed}
    return data_processed

def modelPredictDataPrepare(trial, model, data, objConfig, objParams):
    x = []
    params_morgan = objParams['model_params']['fp_params']
    scaler = objParams['model_params']['scaler']

    df = pd.DataFrame(data,columns=['smiles'])
    df.insert(loc=1,column='dummy_y',value=data)

    x, _, _ = preprocess_data_phys_and_struct(df=df, params_fingerprint=params_morgan, train_size=0, column_smiles='smiles',
                                    columns_phys=None, column_y='dummy_y', scaler_svr_physical_data = scaler)
    return x