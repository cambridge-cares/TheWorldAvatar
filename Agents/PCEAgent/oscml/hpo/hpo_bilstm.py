import logging
from oscml.utils.util_config import set_config_param
from oscml.models.model_bilstm import get_dataloaders
from oscml.hpo.objclass import Objective
from oscml.hpo.hpo_utils import preproc_training_params
from oscml.utils.util_transfer_learning import BiLstmForPceTransfer
from oscml.models.model_bilstm import BiLstmForPce
from oscml.hpo.hpo_utils import NN_model_train, NN_model_train_cross_validate
from oscml.hpo.hpo_utils import NN_addBestModelRetrainCallback
from oscml.hpo.hpo_utils import NN_logBestTrialRetraining, NN_transferLearningCallback
from oscml.hpo.hpo_utils import NN_logTransferLearning, NN_prepareTransferLearningModel
from oscml.hpo.hpo_utils import NN_valDataCheck, NN_loadModelFromCheckpoint, NN_ModelPredict
from oscml.data.dataset import get_dataset_info
from oscml.utils.util import smiles2mol
import torch
import numpy as np
import pandas as pd

def getObjectiveBilstm(
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
    # create a model agnostic objective instance
    objectiveBilstm = Objective(modelName=modelName, data=data, config=config,
                        logFile=logFile, logDir=logDir, logHead=logHead)

    # add goal and model specific settings
    if bestTrialRetraining:
        objectiveBilstm = addBestTrialRetrainingSettings(objectiveBilstm, config)
    elif transferLearning:
        objectiveBilstm = addTransferLearningSettings(objectiveBilstm, crossValidation, config)
    elif modelPredict:
        objectiveBilstm = addModelPredictSettings(objectiveBilstm)
    else:
        objectiveBilstm = addHpoSettings(objectiveBilstm, crossValidation, config)
    return objectiveBilstm

def addBestTrialRetrainingSettings(objective, config):
    # for not cv job, make sure there is non empty validation set
    # as NN methods require it for training. In case of the best trial
    # retraining, the cross_validation must always be False
    objective.data = NN_valDataCheck(objective.data, config)

    objective.setModelCreator(funcHandle=model_create,extArgs=[BiLstmForPce])
    objective.setModelTrainer(funcHandle=NN_model_train,extArgs=[data_preproc])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    objective.addPostModelCreateTask(objParamsKey='callbackBestTrialRetraining', funcHandle=NN_addBestModelRetrainCallback)
    objective.addPostTrainingTask(objParamsKey='logBestTrialRetrain', funcHandle=NN_logBestTrialRetraining)
    return objective

def addModelPredictSettings(objective):
    objective.addPreModelCreateTask(objParamsKey='ckpt_path', funcHandle=setModelCheckpoint)
    objective.setModelCreator(funcHandle=NN_loadModelFromCheckpoint, extArgs=[BiLstmForPce])
    objective.addPostModelCreateTask(objParamsKey='predictDataPreproc', funcHandle=predictDataPreproc)
    objective.addPostModelCreateTask(objParamsKey='predictModel', funcHandle=NN_ModelPredict)
    return objective

def addTransferLearningSettings(objective, crossValidation, config):
    freeze_and_train = config['transfer_learning']['freeze_and_train']
    modelCreatorClass = BiLstmForPceTransfer if freeze_and_train else BiLstmForPce
    model_trainer_func = NN_model_train_cross_validate if crossValidation else NN_model_train

    # for not cv job, make sure there is non empty validation set
    # as NN methods require it for training
    if not crossValidation:
        objective.data = NN_valDataCheck(objective.data, config, transferLearning=True)

    # this flag, if true, disables model creation in the objclass _createModel step, instead the model is
    # created in the trainer as part of the cross validation loop
    objective.setCrossValidation(crossValidation)
    objective.setModelCreator(funcHandle=model_create,extArgs=[modelCreatorClass])
    objective.setModelTrainer(funcHandle=model_trainer_func,extArgs=[data_preproc])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    objective.addPostModelCreateTask(objParamsKey='callbackTransferLearning', funcHandle=NN_transferLearningCallback)
    objective.addPostModelCreateTask(objParamsKey='transferLearningModel', funcHandle=NN_prepareTransferLearningModel)
    objective.addPostTrainingTask(objParamsKey='logTransferLearning', funcHandle=NN_logTransferLearning)
    return objective

def addHpoSettings(objective, crossValidation, config):
    model_trainer_func = NN_model_train_cross_validate if crossValidation else NN_model_train

    if not crossValidation:
        objective.data = NN_valDataCheck(objective.data, config)
    # this flag disables model creation in the objclass _createModel step, instead the model is
    # created in the trainer as part of the cross validation loop
    objective.setCrossValidation(crossValidation)
    objective.setModelCreator(funcHandle=model_create,extArgs=[BiLstmForPce])
    objective.setModelTrainer(funcHandle=model_trainer_func,extArgs=[data_preproc])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    return objective

def model_create(trial, data, objConfig, objParams, modelCreatorClass):
    transformer = data['transformer']
    type_dict = objConfig['config']['model']['type_dict']
    info = get_dataset_info(type_dict)
    number_subgraphs = info.number_subgraphs()
    max_sequence_length = objConfig['config']['model']['max_sequence_length']

    batch_size = objParams['training']['batch_size']
    optimizer = objParams['training']['optimiser']

    # set model parameters from the config file
    model_params = {}
    for key, value in objConfig['config']['model']['model_specific'].items():
        model_params.update({key: set_config_param(trial=trial,param_name=key,param=value, all_params=model_params)})

    # add output dimension to the mlp_units
    model_params['mlp_units'] = model_params.get('mlp_units', []) + [1]

    # add extra params not defined in the config file
    extra_params =  {
        # additional non-hyperparameter values
        'lstm_hidden_dim': model_params['embedding_dim'],
        'padding_index': 0,
        'target_mean': transformer.target_mean,
        'target_std': transformer.target_std,
        'number_of_subgraphs': number_subgraphs
    }
    model_params.update(extra_params)
    logging.info('model params=%s', model_params)

    model_params.pop('mlp_layers',None) # this is not needed for the model creation
    model = modelCreatorClass(**model_params, optimizer=optimizer)

    return model

def data_preproc(trial, data, objConfig, objParams):
    type_dict = objConfig['config']['model']['type_dict']
    df_train = data['train']
    df_val = data['val']
    df_test = data['test']
    transformer = data['transformer']

    info = get_dataset_info(type_dict)
    number_subgraphs = info.number_subgraphs()
    max_sequence_length = objConfig['config']['model']['max_sequence_length']
    batch_size = objParams['training']['batch_size']

    # dataloaders
    train_dl, val_dl, test_dl = get_dataloaders(type_dict, df_train, df_val, df_test,
        transformer, batch_size=batch_size, max_sequence_length=max_sequence_length)

    processed_data = {
        "train": train_dl,
        "val": val_dl,
        "test": test_dl,
        "transformer": transformer
    }
    processed_data = {**data, **processed_data}
    return processed_data

def setModelCheckpoint(trial, data, objConfig, objParams):
    return objConfig['config']['predict_settings']['ckpt_path']

def predictDataPreproc(trial, model, data, objConfig, objParams):
    type_dict = objConfig['config']['model']['type_dict']
    info = get_dataset_info(type_dict)
    max_sequence_length = objConfig['config']['model']['max_sequence_length']
    mol2seq_obj = info.mol2seq
    padding_index = 0
    padding_sequence = [padding_index]*max_sequence_length
    # index - row index in the dataframe
    x = []
    for smiles in data:
        # smiles to rdkit mol object
        m = smiles2mol(smiles)
        # m to fragments sequence vector in BFS (not padded yet) but it may include oov indices as "-1"
        xx = mol2seq_obj(m)
        # increase all indexes by +1 - this this would shift any oov indices to "0"
        xx = np.array(xx) + 1
        # fill up the sequence with padding index 0
        diff = max_sequence_length-len(xx)
        if diff > 0:
            xx = np.append(xx, padding_sequence[:diff])
        if diff < 0:
            raise RuntimeError('A sequence with length greater the maximum sequence length was generated.',
                    ', length=', len(xx), ', maximum sequence length=', max_sequence_length)

        xx = torch.tensor([xx], dtype = torch.long)
        x.append(xx)
    return x