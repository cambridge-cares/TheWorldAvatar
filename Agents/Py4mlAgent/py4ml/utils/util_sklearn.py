import logging
import sklearn
import pandas as pd
from pathlib import Path
import dill
import os
import py4ml.utils.util
import torch

def calculate_mean_prediction(regressors, x, y):

    # ensemble learning
    y_sum = None
    # average mse
    mse_sum = None

    for reg in regressors:
        y_pred = reg.predict(x)
        metrics = py4ml.utils.util.calculate_metrics(y, y_pred)
        if y_sum is None:
            y_sum = y_pred
            mse_sum = metrics['mse']
        else:
            y_sum += y_pred
            mse_sum += metrics['mse']
    y_mean = y_sum / len(regressors)
    mse_mean = mse_sum / len(regressors)
    metrics_y_mean = py4ml.utils.util.calculate_metrics(y, y_mean)
    return mse_mean, metrics_y_mean

def train_model_cross_validate(trial, model, data, objConfig, objParams):
    # use cross_validate instead of cross_val_score to get more information about scores
    # only use 1 CPU (n_jobs=1)
    x_train, y_train = data['train']
    x_test, y_test = data['test']
    cross_validation = objParams['training']['cross_validation']

    log_head = '[Trial '+ str(trial.number) + ' - ' + str(cross_validation) + ' fold cross-validation]'

    all_scores = sklearn.model_selection.cross_validate(model, x_train, y_train, cv=cross_validation,
                    scoring='neg_mean_squared_error',  n_jobs=1, verbose=0, fit_params=None, pre_dispatch='2*n_jobs',
                    return_train_score=True, return_estimator=True , error_score='raise')
    for phase in ['train', 'test']:
        scores = all_scores[phase + '_score']
        mean = scores.mean()
        std = scores.std()
        logging.info('%s %s: mean %s, std=%s, scores=%s', log_head, phase, mean, std, scores)

    objective_value = - mean # from test scores

    #print('MY', all_scores)
    assert cross_validation == len(all_scores['estimator'])

    regressors = all_scores['estimator']
    mse_mean_train, result_train = calculate_mean_prediction(regressors, x_train, y_train)
    logging.info('%s mean mse train=%s, ensemble train result=%s', log_head, mse_mean_train, result_train)
    mse_mean_test, result_test = calculate_mean_prediction(regressors, x_test, y_test)
    logging.info('%s mean mse test=%s, ensemble test result=%s', log_head, mse_mean_test, result_test)

    # check the objective value
    """
    score_sum = 0.
    for reg in all_scores['estimator']:
        y_pred = reg.predict(x_train)
        metrics = py4ml.utils.util.calculate_metrics(y_train, y_pred)
        score_sum += metrics['mse']
    mean_score = score_sum / len(all_scores['estimator'])
    logging.info('mean score sum on entire train set=' + str(mean_score))
    """
    return objective_value

def train_model(trial, model, data, objConfig, objParams, trainerFunc):
    log_head = objConfig['log_head']
    log_dir = objConfig['log_dir']
    transformer = data['transformer']
    regression_plot = objConfig['config']['post_processing']['regression_plot']

    x_train, y_train = data['train']
    if data['val'] is not None:
        x_val, y_val = data['val']
    else:
        x_val = y_val = None
    x_test, y_test = data['test']
    metric = objParams['training']['metric']
    log_head = '[Trial ' + str(trial.number) + ']'


    objective_value = trainerFunc(trial, model, objParams, metric, x_train, y_train, x_val, y_val,
                                x_test, y_test, log_head, log_dir, transformer, regression_plot)

    return objective_value

def train_model_hpo(trial, model, objParams, metric, x_train, y_train, x_val, y_val, x_test, y_test, log_head, *args):
    model.fit(x_train, y_train)

    calculate_metrics(model, x_train, y_train, metric, 'train', log_head)
    objective_value = 0.
    if x_val is not None:
        result = calculate_metrics(model, x_val, y_val, metric, 'val', log_head)
        objective_value = result[metric]
    if x_test is not None:
        result = calculate_metrics(model, x_test, y_test, metric, 'test', log_head)
    return objective_value

def calculate_metrics(model, x, y, metric, ml_phase, log_head):
    y_pred = model.predict(x)
    if len(y_pred.shape) == 1:
        y_pred = y_pred.reshape((-1,1))
    if metric == 'mse':
        result = py4ml.utils.util.calculate_metrics(y, y_pred)
    elif metric == 'all':
        result = {'phase': ml_phase}
        result.update(py4ml.utils.util.calculate_metrics(y, y_pred))
        return result, y_pred
    else: # accuracy
        result = {'accuracy': sklearn.metrics.accuracy_score(y, y_pred)}

    logging.info('%s %s result=%s', log_head, ml_phase, result)
    return result

def log_and_plot(model, x_train, y_train, x_test, y_test, dirpath, transformer=None,
                 regression_plot=False, log_head=None):

    def _getMetrics(model, x_, y_, transformer, index_, log_head):
        test_results, y_pred = calculate_metrics(model, x_, y_, 'all', index_, log_head)

        pred_df = pd.DataFrame()
        for i in range(y_.shape[1]):
            pred_df[f"Measured Y{i+1}"] = y_[:,i]
            pred_df[f"Predicted Y{i+1}"] = y_pred[:,i]

            if transformer.transform_type is not None:
                pred_df[f"Measured untransformed Y{i+1}"] = transformer.inverse_transform_y(y_[:,i], ind=i)
                pred_df[f"Predicted untransformed Y{i+1}"] = transformer.inverse_transform_y(y_pred[:,i], ind=i)

        pred_df.to_csv(os.path.join(dirpath,f"predictions_{index_.replace(' ', '_')}.csv"))
        pred_df = pred_df[[col for col in pred_df.columns if "untransformed" not in col]]
        return test_results, pred_df


    index_ml = ['training set', 'test set']
    x_ml = [x_train, x_test]
    y_ml = [y_train, y_test]

    results_metric = []
    pred_dfs = {'training set': None, 'validation set': None, 'test set': None}
    for index_, x_, y_ in zip(index_ml, x_ml, y_ml):
        metrics, pred_df = _getMetrics(model, x_, y_, transformer, index_, log_head)
        results_metric.append(metrics)
        pred_dfs[index_] = pred_df


    pd.DataFrame(results_metric).to_csv(dirpath + 'best_trial_retrain_model.csv')

    if regression_plot:
        columns = []
        for i in range(y_ml[0].shape[1]):
            columns.append([f"Measured Y{i+1}", f"Predicted Y{i+1}"])

        py4ml.visualization.util_sns_plot.prediction_plot(
            figure_dir= dirpath+'\\',
            train_pred = pred_dfs["training set"],
            val_pred = pred_dfs["validation set"],
            test_pred = pred_dfs["test set"],
            cols = columns
        )

def best_model_retraining(trial, model, objParams, metric, x_train, y_train, x_val, y_val,
                                x_test, y_test, log_head, log_dir, transformer, regression_plot):
    model_params = {}
    model_params_in_objParams = objParams.get('model_params')
    if model_params_in_objParams is not None:
        model_params.update(model_params_in_objParams)
    model_params["transformer"] = transformer

    dirpath = log_dir + '/trial_' + str(trial.number) + '/'
    Path(dirpath).mkdir(parents=True, exist_ok=True)

    model.fit(x_train, y_train)

    model_pkl_file = os.path.join(dirpath,"best_trial_retrain_model.ckpt")
    # using torch.save method rather than pure pickle or joblib
    # found that pickle or joblib do not work for some lambda functions in the svr model
    # torch.save in turn does work with lambdas.
    torch.save(obj={"model":model, "model_params":model_params},
               f=model_pkl_file,
               pickle_module=dill)
    log_and_plot(model, x_train, y_train, x_test, y_test, dirpath, transformer=transformer,
                 regression_plot=regression_plot, log_head=log_head)
    return calculate_metrics(model, x_train, y_train, metric, 'val', log_head)


