import logging
from py4ml.hpo.objclass import Objective
from py4ml.hpo.hpo_utils import preproc_training_params
from py4ml.models.model_mlp import MlpWithLightning, get_dataloaders
from py4ml.hpo.hpo_utils import NN_model_train, NN_model_train_cross_validate, sample_params
from py4ml.hpo.hpo_utils import NN_addBestModelRetrainCallback
from py4ml.hpo.hpo_utils import NN_logBestTrialRetraining
from py4ml.hpo.hpo_utils import NN_valDataCheck, NN_loadModelFromCheckpoint, NN_ModelPredict
import torch
import torch.utils.data
import pandas as pd

def getObjectiveMLP(
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
    objectiveMLP = Objective(modelName=modelName, data=data, config=config,
                        logFile=logFile, logDir=logDir, logHead=logHead)

    # add goal and model specific settings
    if bestTrialRetraining:
        objectiveMLP = addBestTrialRetrainingSettings(objectiveMLP, config)
    elif modelPredict:
        objectiveMLP = addModelPredictSettings(objectiveMLP)
    else:
        objectiveMLP = addHpoSettings(objectiveMLP, crossValidation, config)
    return objectiveMLP

def addBestTrialRetrainingSettings(objective, config):
    # for not cv job, make sure there is non empty validation set
    # as NN methods require it for training. In case of the best trial
    # retraining, the cross_validation must always be False
    objective.data = NN_valDataCheck(objective.data, config)

    objective.setModelCreator(funcHandle=model_create,extArgs=[MlpWithLightning])
    objective.setModelTrainer(funcHandle=NN_model_train,extArgs=[data_preproc])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    objective.addPostModelCreateTask(objParamsKey='callbackBestTrialRetraining', funcHandle=NN_addBestModelRetrainCallback)
    objective.addPostTrainingTask(objParamsKey='logBestTrialRetrain', funcHandle=NN_logBestTrialRetraining)
    return objective

def addModelPredictSettings(objective):
    objective.addPreModelCreateTask(objParamsKey='ckpt_path', funcHandle=setModelCheckpoint)
    objective.setModelCreator(funcHandle=NN_loadModelFromCheckpoint, extArgs=[MlpWithLightning])
    objective.addPostModelCreateTask(objParamsKey='predictDataPreproc', funcHandle=predictDataPreproc)
    objective.addPostModelCreateTask(objParamsKey='predictModel', funcHandle=NN_ModelPredict)
    return objective


def addHpoSettings(objective, crossValidation, config):
    model_trainer_func = NN_model_train_cross_validate if crossValidation else NN_model_train

    if not crossValidation:
        objective.data = NN_valDataCheck(objective.data, config)
    # this flag disables model creation in the objclass _createModel step, instead the model is
    # created in the trainer as part of the cross validation loop
    objective.setCrossValidation(crossValidation)
    objective.setModelCreator(funcHandle=model_create,extArgs=[MlpWithLightning])
    objective.setModelTrainer(funcHandle=model_trainer_func,extArgs=[data_preproc])
    objective.addPreModelCreateTask(objParamsKey='training', funcHandle=preproc_training_params)
    return objective

def model_create(trial, data, objConfig, objParams, modelCreatorClass):
    transformer = data['transformer']

    optimizer = objParams['training']['optimiser']

    # set model parameters from the config file
    model_params = sample_params(
        model_params=objConfig['config']['model']['model_specific'],
        trial = trial
    )

    # add output dimension to the mlp_units
    model_params['mlp_units'] = model_params.get('mlp_units', []) + [len(data['y_column'])]

    mlp_units_with_input_dim = [model_params.pop('in_features', len(data['x_column']))]
    mlp_units_with_input_dim.extend(model_params['mlp_units'])
    model_params['mlp_units'] = mlp_units_with_input_dim

    # add extra params not defined in the config file
    extra_params =  {
        # additional non-hyperparameter values
        'transformer': transformer
    }
    model_params.update(extra_params)
    logging.info('model params=%s', model_params)

    model_params.pop('mlp_layers',None) # this is not needed for the model creation
    model = modelCreatorClass(**model_params, optimizer=optimizer)

    return model

def data_preproc(trial, data, objConfig, objParams):
    df_train = data['train']
    df_val = data['val']
    df_test = data['test']
    transformer = data['transformer']

    x_column = data['x_column']
    y_column = data['y_column']

    batch_size = objParams['training']['batch_size']

    # dataloaders

    train_dl, val_dl, test_dl = get_dataloaders(
        train=df_train,
        val=df_val,
        test=df_test,
        batch_size=batch_size,
        column_x=x_column,
        column_y=y_column,
        transformer=transformer)

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
    x = []
    transformer = model.transformer

    for xi in data:
        x_transformed = transformer.transform_x(pd.Series(xi))
        xt = torch.tensor([x_transformed], dtype = torch.float32) # type: ignore
        x.append(xt)
    return x