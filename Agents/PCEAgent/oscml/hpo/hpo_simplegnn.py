import logging

import oscml.data.dataset
import oscml.models.model_gnn
from oscml.utils.util_config import set_config_param
import oscml.utils.util_transfer_learning
from oscml.hpo.hpo_utils import preproc_training_params
from oscml.utils.util_transfer_learning import SimpleGNNTransfer
from oscml.models.model_gnn import SimpleGNN
from oscml.hpo.hpo_utils import NN_model_train, NN_logBestTrialRetraining, NN_valDataCheck
from oscml.hpo.hpo_utils import NN_model_train_cross_validate, \
                                NN_addBestModelRetrainCallback, \
                                NN_prepareTransferLearningModel, \
                                NN_logTransferLearning, \
                                NN_transferLearningCallback, \
                                NN_loadModelFromCheckpoint, \
                                NN_ModelPredict
from oscml.hpo.objclass import Objective
from oscml.data.dataset import get_dataset_info
from oscml.utils.util import smiles2mol
from oscml.models.model_gnn import Mol2seq_simple
import rdkit
import rdkit.Chem
import rdkit.Chem.AllChem
import rdkit.Chem.rdmolops
import networkx
import dgl
import torch

def getObjectiveSimpleGNN(
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

    # create SimpleGNN objective
    objectiveSimpleGNN = Objective(modelName=modelName, data=data, config=config,
                        logFile=logFile, logDir=logDir, logHead=logHead)

    # add goal and model specific settings
    if bestTrialRetraining:
        objectiveSimpleGNN = addBestTrialRetrainingSettings(objectiveSimpleGNN, config)
    elif transferLearning:
        objectiveSimpleGNN = addTransferLearningSettings(objectiveSimpleGNN, crossValidation, config)
    elif modelPredict:
        objectiveSimpleGNN = addModelPredictSettings(objectiveSimpleGNN)
    else:
        objectiveSimpleGNN = addHpoSettings(objectiveSimpleGNN, crossValidation, config)
    return objectiveSimpleGNN

def addBestTrialRetrainingSettings(objective, config):
    # for not cv job, make sure there is non empty validation set
    # as NN methods require it for training. In case of the best trial
    # retraining, the cross_validation must always be False
    objective.data = NN_valDataCheck(objective.data, config)
    objective.setModelCreator(funcHandle=model_create,extArgs=[SimpleGNN])
    objective.setModelTrainer(funcHandle=NN_model_train,extArgs=[data_preproc])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    objective.addPostModelCreateTask(objParamsKey='callbackBestTrialRetraining', funcHandle=NN_addBestModelRetrainCallback)
    objective.addPostTrainingTask(objParamsKey='logBestTrialRetrain', funcHandle=NN_logBestTrialRetraining)
    return objective

def addModelPredictSettings(objective):
    objective.addPreModelCreateTask(objParamsKey='ckpt_path', funcHandle=setModelCheckpoint)
    objective.setModelCreator(funcHandle=NN_loadModelFromCheckpoint, extArgs=[SimpleGNN])
    objective.addPostModelCreateTask(objParamsKey='predictDataPreproc', funcHandle=predictDataPreproc)
    objective.addPostModelCreateTask(objParamsKey='predictModel', funcHandle=NN_ModelPredict)
    return objective

def addTransferLearningSettings(objective, crossValidation, config):
    freeze_and_train = config['transfer_learning']['freeze_and_train']
    modelCreatorClass = SimpleGNNTransfer if freeze_and_train else SimpleGNN
    model_trainer_func = NN_model_train_cross_validate if crossValidation else NN_model_train

    # for not cv job, make sure there is non empty validation set
    # as NN methods require it for training
    if not crossValidation:
        objective.data = NN_valDataCheck(objective.data, config)
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

    # for not cv job, make sure there is non empty validation set
    # as NN methods require it for training
    if not crossValidation:
        objective.data = NN_valDataCheck(objective.data, config, transferLearning=True)
    # this flag, if true, disables model creation in the objclass _createModel step, instead the model is
    # created in the trainer as part of the cross validation loop
    objective.setCrossValidation(crossValidation)
    objective.setModelCreator(funcHandle=model_create,extArgs=[SimpleGNN])
    objective.setModelTrainer(funcHandle=model_trainer_func,extArgs=[data_preproc])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    return objective



def model_create(trial, data, objConfig, objParams, modelCreatorClass):
    transformer = data['transformer']
    type_dict = objConfig['config']['model']['type_dict']
    batch_size = objParams['training']['batch_size']
    optimizer = objParams['training']['optimiser']

    info = oscml.data.dataset.get_dataset_info(type_dict)
    node_type_number = len(info.node_types)

    # set model parameters from the config file
    #--------------------------------------
    model_params = {}
    for key, value in objConfig['config']['model']['model_specific'].items():
        model_params.update({key: set_config_param(trial=trial,param_name=key,param=value, all_params=model_params)})

    # constant state vector size throughout the graph convolutional layers
    embedding_dim = model_params['embedding_dim']
    conv_layers = model_params['conv_layers']
    conv_dims = [embedding_dim]*conv_layers
    model_params['conv_dims'] = conv_dims

    # add output dimension to the mlp_units
    model_params['mlp_units'] = model_params.get('mlp_units', []) + [1]

    # add extra params not defined in the config file
    extra_params =  {
        # additional non-hyperparameter values
        'node_type_number': node_type_number, #len(oscml.data.dataset_hopv15.ATOM_TYPES_HOPV15),
        'padding_index': 0,
        'target_mean': transformer.target_mean,
        'target_std': transformer.target_std,
    }
    model_params.update(extra_params)
    logging.info('model params=%s', model_params)

    model_params.pop('mlp_layers',None) # this is not needed for the model creation
    model_params.pop('conv_layers',None) # this is not needed for the model creation

    model = modelCreatorClass(**model_params, optimizer=optimizer)

    return model

def data_preproc(trial, data, objConfig, objParams):

    type_dict = objConfig['config']['model']['type_dict']
    batch_size = objParams['training']['batch_size']

    # datasets
    df_train= data['train']
    df_val= data['val']
    df_test= data['test']
    transformer= data['transformer']

    train_dl, val_dl, test_dl = oscml.models.model_gnn.get_dataloaders(type_dict, df_train, df_val, df_test,
            data['transformer'], batch_size=batch_size)

    processed_data = {
        "train": train_dl,
        "val": val_dl,
        "test": test_dl,
        "transformer": data['transformer']
    }
    processed_data = {**data, **processed_data}
    return processed_data


def setModelCheckpoint(trial, data, objConfig, objParams):
    return objConfig['config']['predict_settings']['ckpt_path']

def predictDataPreproc(trial, model, data, objConfig, objParams):
    type_dict = objConfig['config']['model']['type_dict']
    info = get_dataset_info(type_dict)
    node2index = info.node_types
    mol2seq = Mol2seq_simple(node2index, fix=True, oov=True)

    # index - row index in the dataframe
    x = []
    for smiles in data:
        # smiles string -> molecular graph
        m = smiles2mol(smiles)
        # molecular graph -> sequence of indices of atom types
        seq = mol2seq(m)
        #    1 2 3   - this is to get the connectivity
        # 1  0 1 0   - rows must have the same ordering of atoms!
        # 2  1 0 0
        # 3 ...
        adj = rdkit.Chem.rdmolops.GetAdjacencyMatrix(m)
        # so this an intermediate step, from adj -> g_nx
        g_nx = networkx.convert_matrix.from_numpy_matrix(adj)
        # g_nx -> g
        g = dgl.from_networkx(g_nx)

        tensor = torch.as_tensor(seq, dtype=torch.long)
        # define 'type' var in the graph
        g.ndata['type'] = tensor

        x.append(g)
    return x