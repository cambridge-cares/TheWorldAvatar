import logging
from py4ml.utils.util_config import set_config_param
import pytorch_lightning as pl
from sklearn.model_selection import KFold, ShuffleSplit
from pytorch_lightning.callbacks.early_stopping import EarlyStopping
from pytorch_lightning.callbacks import ModelCheckpoint
from py4ml.utils.util_lightning import get_standard_params_for_trainer
import numpy as np
from py4ml.utils.util_sklearn import train_model_cross_validate
from py4ml.utils.util_sklearn import train_model
import glob
import torch
import pandas as pd
import os
from py4ml.visualization.util_sns_plot import prediction_plot
import re
import dill
import copy

class MetricsCallback(pl.Callback):

    def __init__(self):
        super().__init__()
        self.metrics = []

    def on_validation_end(self, trainer, pl_module):
        self.metrics.append(trainer.callback_metrics)

def NN_model_train(trial, model, data, objConfig, objParams, dataPreproc):
    epochs = objParams['training']['epochs']
    metric = objParams['training']['metric']
    es_monitor = objParams['training'].get('es_monitor', metric)
    es_mode = objParams['training'].get('es_mode', 'auto')
    es_patience = objParams['training']['es_patience']
    es_min_delta = objParams['training']['es_min_delta']
    n_trials = objParams['training']['trials']
    log_head = objConfig['log_head']
    log_dir = objConfig['log_dir']
    cv_index = objParams.get('cv_fold', '')

    if log_head is None:
        log_head = '[Trial '+ str(trial.number) +']'
        objConfig['log_head'] = log_head


    # create callbacks for Optuna for receiving the metric values from Lightning and for
    # pruning trials
    metrics_callback = MetricsCallback()
    callbacks = [metrics_callback]
    #if trial:
    #    pruning_callback = optuna.integration.PyTorchLightningPruningCallback(trial, monitor=metric)
    #    callbacks.append(pruning_callback)

    if es_patience > 0:
        early_stopping_callback = EarlyStopping(monitor=es_monitor, min_delta=es_min_delta, patience=es_patience, verbose=False, mode=es_mode)
        callbacks.append(early_stopping_callback)

    for key, val in objParams.items():
        if 'callback' in key:
            callbacks.append(val)

    logging.info('%s model for trial %s=%s', log_head, trial.number, model)

    # create standard params for Ligthning trainer

    # if the number of trials is 1 then save checkpoints for the last and best epoch
    # otherwise if HPO is running (i.e. unspecified time-contrained number of trials or finite number > 1 )
    # then save no checkpoints
    trainer_params = get_standard_params_for_trainer(metric, False)

    # create Lightning metric logger that logs metric values for each trial in its own csv file
    # version='' means that no version-subdirectory is created
    csv_logger = pl.loggers.CSVLogger(save_dir=log_dir,
                                      name='trial_' + str(trial.number),
                                      version=cv_index)

    # put all trainer params together
    trainer_params.update({
        'max_epochs': epochs,
        'logger': csv_logger,
        'callbacks': callbacks,
        'profiler': 'simple'
    })

    logging.info('%s params for Lightning trainer=%s', log_head, trainer_params)

    trainer = pl.Trainer(**trainer_params)

    logging.info('%s fitting trial %s / %s', log_head, trial.number, n_trials)
    data = dataPreproc(trial, data, objConfig, objParams)

    objParams['trainer'] = trainer
    objParams['data_preproc'] = data
    if 'transferLearningModel' in objParams:
        model = objParams['transferLearningModel']

    trainer.fit(model, train_dataloader=data['train'], val_dataloaders=data['val'])

    # return the value for the metric specified in the start script
    if es_patience > 0:
        # return the best score while early stopping is applied
        obj_error = early_stopping_callback.best_score.item()
    else:
        obj_error = metrics_callback.metrics[-1][metric].item()

    logging.info('%s finished fitting for trial %s with %s = %s', log_head, trial.number, metric, obj_error)

    return obj_error

def NN_model_train_cross_validate(trial, model, data, objConfig, objParams, dataPreproc):
    seed = objConfig['config']['numerical_settings']['seed']
    cross_validation = objParams['training']['cross_validation']
    metric = objParams['training']['metric']

    df_train = data['train']
    df_val = data['val']
    df_test = data['test']
    transformer = data['transformer']

    kf = KFold(n_splits=cross_validation, random_state=seed, shuffle=True)
    assert df_val is None, "validation set should be added to training set for cross validation"
    kf.get_n_splits(df_train)
    cv_index = 1
    cv_metric = []
    split_data = {}
    modelCreator = objParams['modelCreator']

    for train_index, val_index in kf.split(df_train):
        logging.info('[trial %s] run %s of %s fold cross-validation', trial.number, cv_index, cross_validation)
        split_data = {'train': df_train.iloc[train_index],
                      'val': df_train.iloc[val_index],
                      'test': df_test,
                      'transformer': transformer}
        split_data = {**data, **split_data}

        objConfig['log_head'] = '[Trial '+ str(trial.number) + ' - fold ' + str(cv_index) + ']'
        objParams['cv_fold'] = str(cv_index)

        model = modelCreator.run(trial, split_data, objConfig, objParams)
        metric_value = NN_model_train(trial, model, split_data, objConfig, objParams, dataPreproc)

        cv_index += 1
        cv_metric.append(metric_value)
    metric_value = np.array(cv_metric).mean()
    std = np.array(cv_metric).std()

    logging.info('[trial %s - finished %s fold cross-validation] %s: %s', trial.number,
                    cross_validation,
                    metric,
                    cv_metric)
    logging.info('[trial %s - finished %s fold cross-validation] %s mean: %s', trial.number,
                    cross_validation,
                    metric,
                    metric_value)
    logging.info('[trial %s - finished %s fold cross-validation] %s variance: %s', trial.number,
                    cross_validation,
                    metric,
                    np.array(cv_metric).var())

    return metric_value

def BL_model_train(trial, model, data, objConfig, objParams, dataPreproc, trainerFunc):
    data = dataPreproc(trial, data, objConfig, objParams)
    obj_value = train_model(trial, model, data, objConfig, objParams, trainerFunc)
    return obj_value

def BL_model_train_cross_validate(trial, model, data, objConfig, objParams, dataPreproc, *args):
    data = dataPreproc(trial, data, objConfig, objParams)
    obj_value = train_model_cross_validate(trial, model, data, objConfig, objParams)
    return obj_value

def preproc_training_params(trial, data, objConfig, objParams):
    training_settings = objConfig['config']['training']
    if 'optimiser' in training_settings:
        # remove optimiser inner nested dict from the training settings
        # as the sample_params cannot handle such nested constructs
        optimiser = training_settings.pop('optimiser')

        # sample optimiser
        optimiser_sampled = sample_params(optimiser, trial)
        # sample other params
        training_params_sampled = sample_params(training_settings, trial)
        # add sampled optimiser params
        training_params_sampled['optimiser'] = optimiser_sampled

        # add back popped default optimiser settings
        training_settings['optimiser'] = optimiser
    else:
        training_params_sampled = sample_params(training_settings, trial)

    return training_params_sampled

def NN_valDataCheck(data, jobConfig, transferLearning=False):
    if data['val'] is not None:
        if len(data['val']) > 0:
            return data

    cv = jobConfig['training']['cross_validation']
    seed = jobConfig['numerical_settings']['seed']

    if transferLearning:
        test_size=0.2
    elif cv > 0:
        test_size=1/cv
    else:
        test_size = 0.2

    data_reshuffled = _reshuffleAndSplitData(data=data, n_splits=1,
                            test_size=test_size, seed=seed+1)

    return data_reshuffled

def _reshuffleAndSplitData(data, n_splits, test_size, seed):
    rs = ShuffleSplit(n_splits=n_splits, test_size=test_size, random_state=seed)
    rs.get_n_splits(data['train'])
    data_reshuffled = {}
    for train_index, val_index in rs.split(data['train']):
        data_reshuffled = {
            'train': data['train'].iloc[train_index],
            'val': data['train'].iloc[val_index],
            'test': data['test'],
            'transformer': data['transformer']
        }
    data_reshuffled= {**data, **data_reshuffled}
    return data_reshuffled

def BL_bestTrialRetrainDataPreproc(data):
    df_train = data['train']
    df_val = data['val']
    df_test = data['test']
    transformer = data['transformer']

    data_preproc = {
        'train' : pd.concat([df_train, df_val]) if df_val is not None else df_train,
        'val' : None,
        'test' : df_test,
        'transformer' : transformer
    }
    data_preproc = {**data, **data_preproc}
    return data_preproc

def NN_logBestTrialRetraining(trial, model, data, objConfig, objParams):
    _logAndPlotResults(trial, model, data, objConfig, objParams, 'best_trial_retrain_model')
    return None

def NN_logTransferLearning(trial, model, data, objConfig, objParams):
    _logAndPlotResults(trial, model, data, objConfig, objParams, 'transfer_learning_model')
    return None

def _logAndPlotResults(trial, model, data, objConfig, objParams, fileName):
    def _getMetrics(dataset_dl, data_type, model, transformer):
        test_result = trainer.test(model, test_dataloaders=dataset_dl)[0]
        test_result['phase'] = f"{data_type}"

        ycol_num = len(dataset_dl.dataset.column_y)
        pred_df = pd.DataFrame()

        for i in range(ycol_num):
            pred_df[f"Measured Y{i+1}"] = model.test_predictions[0][i::ycol_num]
            pred_df[f"Predicted Y{i+1}"] = model.test_predictions[1][i::ycol_num]

            if transformer.transform_type is not None:
                pred_df[f"Measured Y{i+1} untransformed"] = transformer.inverse_transform_y(pred_df[f"Measured Y{i+1}"], ind = i)
                pred_df[f"Predicted Y{i+1} untransformed"] = transformer.inverse_transform_y(pred_df[f"Predicted Y{i+1}"], ind = i)

        pred_df.to_csv(os.path.join(dirpath,f"predictions_{index_.replace(' ', '_')}.csv"))
        pred_df = pred_df[[col for col in pred_df.columns if "untransformed" not in col]]
        return test_result, pred_df

    transformer = data['transformer']
    regression_plot = objConfig['config']['post_processing']['regression_plot']
    log_dir = objConfig['log_dir']
    data = objParams['data_preproc']
    trainer = objParams['trainer']
    train_dl = data['train']
    val_dl = data['val']
    test_dl = data['test']

    dirpath = os.path.join(log_dir, 'trial_' + str(trial.number))
    ckpt_path = getLastCheckpointPath(os.path.join(dirpath, fileName+'*.ckpt'))
    model.load_state_dict(torch.load(ckpt_path, map_location=torch.device('cpu'))['state_dict'])
    model.eval()

    objParams['best_trial_retrain_dirpath'] = dirpath
    index_dl = ['training set', 'validation set', 'test set']

    dataset_dl_list = [train_dl, val_dl, test_dl]
    results_metric = []
    pred_dfs = {index_dl[0]: None, index_dl[1]: None, index_dl[2]: None}
    for index_, dataset_dl in zip(index_dl, dataset_dl_list):
        metrics, pred_df = _getMetrics(dataset_dl, data_type=index_, model=model, transformer = transformer)
        results_metric.append(metrics)
        pred_dfs[index_] = pred_df

    pd.DataFrame(results_metric).to_csv(os.path.join(dirpath,fileName+'.csv'))

    if regression_plot:

        columns = []
        ycol_num = len(train_dl.dataset.column_y)
        for i in range(ycol_num):
            columns.append([f"Measured Y{i+1}", f"Predicted Y{i+1}"])

        prediction_plot(
            figure_dir= dirpath+'\\',
            train_pred = pred_dfs[index_dl[0]],
            val_pred = pred_dfs[index_dl[1]],
            test_pred = pred_dfs[index_dl[2]],
            cols = columns
        )

def NN_addBestModelRetrainCallback(trial, model, data, objConfig, objParams):
    chp_monitor = objParams['training']['chp_monitor']
    chp_mode = objParams['training']['chp_mode']
    log_dir = objConfig['log_dir']

    dirpath = log_dir + '/trial_' + str(trial.number) + '/'
    checkpoint_callback = ModelCheckpoint(monitor=chp_monitor, dirpath=dirpath.replace('//', '/'),
                                            filename='best_trial_retrain_model',
                                            save_top_k=1, mode=chp_mode)
    return checkpoint_callback

def NN_transferLearningCallback(trial, model, data, objConfig, objParams):
    chp_monitor = objParams['training']['chp_monitor']
    chp_mode = objParams['training']['chp_mode']
    log_dir = objConfig['log_dir']

    dirpath = log_dir + '/trial_' + str(trial.number) + '/'
    checkpoint_callback = ModelCheckpoint(monitor=chp_monitor, dirpath=dirpath.replace('//', '/'),
                                            filename='transfer_learning_model',
                                            save_top_k=1, mode=chp_mode)
    return checkpoint_callback

def NN_prepareTransferLearningModel(trial, model, data, objConfig, objParams):
    transfer_learning = objConfig['config']['transfer_learning']
    ckpt_path = getLastCheckpointPath(transfer_learning['ckpt_path'])
    model.load_state_dict(torch.load(ckpt_path, map_location=torch.device('cpu'))['state_dict'])
    model.eval()
    return model

def NN_loadModelFromCheckpoint(trial, data, objConfig, objParams, modelCreatorClass):
    ckpt_path = getLastCheckpointPath(objParams['ckpt_path'])
    model = modelCreatorClass.load_from_checkpoint(ckpt_path)
    model.eval()
    return model

def BL_loadModelFromCheckpoint(trial, data, objConfig, objParams):
    ckpt_path = getLastCheckpointPath(objConfig['config']['predict_settings']['ckpt_path'])
    model_and_params = torch.load(f=ckpt_path, pickle_module=dill)
    model = model_and_params['model']
    objParams['model_params'] = model_and_params['model_params']
    return model

def NN_ModelPredict(trial, model, data, objConfig, objParams):
    log_dir = objConfig['log_dir']
    data = objParams['predictDataPreproc']
    model.freeze()
    transformer = model.transformer

    splitBatchesFlag = 'splitBatchesFlag' in objParams
    obj_values = []
    for batch in data:
        if splitBatchesFlag:
            obj_value = model(*batch).numpy()[0][0]
        else:
            obj_value = model(batch).numpy()[0]
            obj_value= transformer.inverse_transform_y(obj_value)
            obj_value = obj_value[0]
        obj_values.append(obj_value)
    _logModelPredict(trial, model, data, objConfig, objParams, obj_values)
    return obj_values

def BL_ModelPredict(trial, model, data, objConfig, objParams):
    data = objParams['predictDataPreproc']
    obj_values = model.predict(data)
    model_params = objParams['model_params']
    transformer = model_params.get('transformer')
    if transformer:
        obj_values = transformer.inverse_transform_y(obj_values)
    _logModelPredict(trial, model, data, objConfig, objParams, obj_values)

    return obj_values.tolist()

def _logModelPredict(trial, model, data, objConfig, objParams, obj_values):
    predict_inputs = objConfig['config']['predict_settings']['predict_input']
    log_dir = objConfig['log_dir']
    log_head = objConfig['log_head']
    model_name = objConfig['config']['model']['name']
    logging.info('%s for %s', log_head, model_name)
    actual_output = objConfig['config']['predict_settings'].get('actual_output', None)
    predicted_results = []
    if actual_output is None:
        actual_output = []

    for i, (obj_val, predict_input) in enumerate(zip(obj_values,predict_inputs)):
        if i >= len(actual_output):
            act_out = 'Not provided'
        else:
            act_out= actual_output[i]
        predicted_results.append([obj_val, act_out, predict_input])
        logging.info('objective value: %s , actual value: %s for input: %s', obj_val, act_out, predict_input)
    df = pd.DataFrame(predicted_results, columns=['predicted Y', 'actual Y','x'])

    df.to_csv(path_or_buf=os.path.join(log_dir,'model_predict.csv'), index=False)

def NN_empty_torch_cache():
    torch.cuda.empty_cache()

def getLastCheckpointPath(ckpt_path):
    max_version = -1
    max_version_ind = 0
    ckpt_list = glob.glob(ckpt_path)
    for i, ckpt in enumerate(ckpt_list):
        match = re.search('-v\d+?\.ckpt',ckpt)
        if match is not None:
            version_ = int(match.group().replace(".ckpt",'').replace("-v",''))
            if version_ > max_version:
                max_version = version_
                max_version_ind = i
    return ckpt_list[max_version_ind]

def sample_params(model_params, trial):
    model_params_copy = copy.deepcopy(model_params)
    sampled_model_params = {}
    for key, value in model_params_copy.items():
        sampled_model_params.update(
            {
                key: set_config_param(
                        trial=trial,
                        param_name=key,param=value,
                        all_params=sampled_model_params
                    )
            }
        )
    return sampled_model_params