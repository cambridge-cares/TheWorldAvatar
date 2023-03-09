import logging
import rdkit
import rdkit.Chem
import rdkit.Chem.AllChem
from sklearn.ensemble import RandomForestRegressor
from oscml.utils.util import smiles2mol
from oscml.utils.util_sklearn import train_model_hpo, best_model_retraining
from oscml.hpo.objclass import Objective
from oscml.hpo.hpo_utils import preproc_training_params, BL_model_train
from oscml.hpo.hpo_utils import BL_model_train_cross_validate, \
                                BL_bestTrialRetrainDataPreproc, \
                                BL_loadModelFromCheckpoint, BL_ModelPredict
from oscml.utils.util_config import set_config_param


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
    objective.addPostModelCreateTask(objParamsKey='predictDataPreproc', funcHandle=smilesToMorganFP)
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
    model_conf = objConfig['config']['model']['model_specific']
    metric = objParams['training']['metric']
    model_params = {}
    for key, value in model_conf.items():
        model_params.update({key: set_config_param(trial=trial,param_name=key,param=value, all_params=model_params)})
    logging.info('model params=%s', model_params)
    model = RandomForestRegressor(**model_params, criterion=metric, n_jobs=1, verbose=0)#, random_state=0)
    return model

def data_preproc(trial, data, objConfig, objParams):
    # set fingerprint parameters from the config file
    #--------------------------------------
    fp_conf = objConfig['config']['model']['fingerprint_specific']

    fp_params = {}
    for key, value in objConfig['config']['model']['fingerprint_specific'].items():
        fp_params.update({key: set_config_param(trial=trial,param_name=key,param=value, all_params=fp_params)})

    logging.info('fingerprint params=%s', fp_params)

    x_column = data['x_column'][0]
    y_column = data['y_column'][0]

    objParams['model_params'] = fp_params

    # at the moment the only supported fingerprint choice is morgan
    fp_type = fp_params.pop('type',None)
    if fp_type=='morgan':
        get_fp = dataFrameToMorganFP
    else:
        logging.exception('', exc_info=True)
        raise ValueError("Unknown fingerprint type '"+ fp_type+"'. Only 'morgan' fingerprints supported.")

    data_processed = {
        'train': None,
        'val': None,
        'test': None,
        'transformer': data['transformer'],
    }
    data_processed['train'] = get_fp(data['train'], fp_params, x_column, y_column)
    if data['val'] is not None:
        data_processed['val'] = get_fp(data['val'], fp_params, x_column, y_column)
    data_processed['test'] = get_fp(data['test'], fp_params, x_column, y_column)

    data_processed = {**data, **data_processed}
    return data_processed

def dataFrameToMorganFP(df, params_morgan, columns_smiles, column_y):
    x = []
    y = []
    for i in range(len(df)):
        smiles = df.iloc[i][columns_smiles]
        m = smiles2mol(smiles)
        fingerprint = rdkit.Chem.AllChem.GetMorganFingerprintAsBitVect(m, **params_morgan)
        x.append(fingerprint)
        pce = df.iloc[i][column_y]
        y.append(pce)

    return (x, y)

def smilesToMorganFP(trial, model, data, objConfig, objParams):
    x = []
    params_morgan = objParams['model_params']
    for smiles in data:
        m = smiles2mol(smiles)
        fingerprint = rdkit.Chem.AllChem.GetMorganFingerprintAsBitVect(m, **params_morgan)
        x.append(fingerprint)
    return x