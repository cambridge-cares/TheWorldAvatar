import json
from collections import OrderedDict, abc
import copy
from oscml.utils.util_pytorch import torch_init
from oscml.utils.util import init_file_logging
from oscml.data.dataset import get_dataframes
from oscml.hpo.optunawrapper import check_for_existing_study
from oscml.hpo.optunawrapper import runHPO, runPostProcTraining, runModelPredict
from oscml.visualization.util_sns_plot import contour_plot
import logging
import os
from pprint import pformat
import pandas as pd
import functools
import datetime
from oscml.resources.defaults import DEFAULTS_

class JobGoal:
    "JobGoal class for managing multiple ML goals"
    def __init__(self, name=None, runFunc=None):
        self.name = name
        self.runFunc = runFunc

    def run(self):
        self.runFunc()

class JobHandler:
    """
    JobHandler class for managing ML jobs

    Attributes:
    ----------
    cmdArgs : dict
        command line arguments and values passed by a user
    defaultArgs : dict
        default arguments and values
    configFile : dict
        arguments and values read from the user provided config file
    configParams : dict
        final arguments and values used in the ML job obtained after
        processing the command line, default and config file arguments
    data : data_frame
        data to be used for the ML job
    dataNestedCv : list[data_frame]
        data to be used for each fold of nested cross validation
    dataInit : bool
        flag indicating if data has been read
    transferDataInit : bool
        flag indicating if data for transfer learning has been read
    total_trials_nr : int
        total number of trials, including any previously run trials
        found in the job's storage backend
    log_file : str
        path to the job log file
    log_dir : str
        directory of the job log file
    objective : obj
        current ML job objective
    goals : obj
        defined ML job goals to be executed
    """
    def __init__(self,cmdArgs):
        "JobHandler constructor"
        self.cmdArgs = self._processCmdArgs(cmdArgs)
        self.defaultArgs = None
        self.configFile = self._readConfigFile()
        self.configParams = self._setConfigParams()
        self.data = {}
        self.dataNestedCv = []
        self.dataInit = False
        self.transferDataInit = False
        self.total_trials_nr = 0
        self.log_file = None
        self.log_dir = None
        self.objective = None
        self.goals = []

    def _setGoals(self):
        goals = self.configParams['goals']

        if goals['hpo']: self.goals.append(JobGoal(name='hpo_or_train', runFunc=self._runHpoOrtrain))
        if goals['contour_plot']: self.goals.append(JobGoal(name='contour_plot', runFunc=self._runCountourPlot))
        if goals['best_trial_retraining']: self.goals.append(JobGoal(name='best_trial_retraining', runFunc=self._runBestTrialRetraining))
        if goals['transfer_learning']: self.goals.append(JobGoal(name='transfer_learning', runFunc=self._runTransferLearning))
        if goals['predict']: self.goals.append(JobGoal(name='predict', runFunc=self._runModelPredict))

    def runJob(self):
        # set the goals first
        self._setGoals()

        nestedCvFolds = self.configParams['training']['nested_cross_validation']
        if nestedCvFolds:
            # get the data, and
            # - if cross_validation on: splits them into train, val and test
            crossValidation = self.configParams['training']['cross_validation'] > 1
            transferLearning = self.configParams['post_processing']['transfer'] and self.goals[0].name == 'transfer_learning'

            self._initData(crossValidation=crossValidation, transferLearning=transferLearning, nestedCvFolds=nestedCvFolds)
            for cvFold in range(nestedCvFolds):
                self._initLogging(cvFold)
                self._initTorch()
                self._initJobMisc(cvFold)
                self._getDataFold(cvFold)
                self._runGoals()
        else:
            self._initLogging()
            self._initTorch()
            self._runGoals()

    def _runGoals(self):
        for goal in self.goals:
            logging.info('Executing goal : %s', goal.name)
            goal.run()

    def _runHpoOrtrain(self):
        crossValidation = self.configParams['training']['cross_validation'] > 1
        if not self.dataInit:
            self._initData(crossValidation=crossValidation)
        log_dir = os.path.join(self.log_dir,'hpo')
        logHead=None
        self._initObjective(logDir=log_dir, logHead=logHead, crossValidation=crossValidation)
        runHPO(objective=self.objective, config=self.configParams, total_number_trials=self.total_trials_nr)

    def _runBestTrialRetraining(self):
        if not self.dataInit:
            self._initData(crossValidation=False)
        logDir = os.path.join(self.log_dir,'best_trial_retrain')
        logHead = '[Best trial retrain - Trial '
        self._initObjective(logDir=logDir, logHead=logHead, crossValidation=False, bestTrialRetraining=True)
        runPostProcTraining(objective=self.objective, jobConfig=self.configParams)

    def _runTransferLearning(self):
        if not self.transferDataInit:
            self._initData(crossValidation=False, transferLearning=True)
        logDir = os.path.join(self.log_dir,'transfer_learning')
        logHead = '[Best trial transfer learning - Trial '
        self._initObjective(logDir=logDir, logHead=logHead, crossValidation=False, transferLearning=True)
        runPostProcTraining(objective=self.objective, jobConfig=self.configParams)

    def _runModelPredict(self):
        self.data = self.configParams['predict_settings']['predict_input']
        logDir = os.path.join(self.log_dir,'model_predict')
        logHead = 'Model predict'
        self._initObjective(logDir=logDir, logHead=logHead, crossValidation=False, bestTrialRetraining=False, modelPredict=True)
        runModelPredict(objective=self.objective, jobConfig=self.configParams)

    def _runCountourPlot(self):
        logDir = self.configParams['post_processing']['contour_plot_alt_dir']
        if logDir is None: logDir = self.log_dir
        path = os.path.join(logDir,'hpo','hpo_result.csv')
        contour_plot(logDir, path)

    def _initObjective(
                self,
                crossValidation,
                transferLearning=False,
                bestTrialRetraining=False,
                modelPredict=False,
                logDir=None,
                logHead=None
                ):
        n_previous_trials = check_for_existing_study(self.configParams['training']['storage'], \
                                                     self.configParams['training']['study_name'])

        n_trials = self.configParams['training']['trials']
        if n_previous_trials > 0:
            total_number_trials = n_trials + n_previous_trials
        else:
            total_number_trials = n_trials

        self.total_trials_nr = total_number_trials

        model_name = self.configParams['model']['name']
        if model_name == 'RF':
            from oscml.hpo.hpo_rf import getObjectiveRF as getObjective

        elif model_name == 'SVR':
            from oscml.hpo.hpo_svr import getObjectiveSVR as getObjective

        elif model_name == 'BILSTM':
            from oscml.hpo.hpo_bilstm import getObjectiveBilstm as getObjective

        elif model_name == 'AttentiveFP':
            from oscml.hpo.hpo_attentivefp import getObjectiveAttentiveFP as getObjective

        elif model_name == 'SimpleGNN':
            from oscml.hpo.hpo_simplegnn import getObjectiveSimpleGNN as getObjective

        if logDir is None: logDir = self.log_dir
        self.objective = getObjective(
                            modelName=model_name,
                            data=self.data,
                            config=self.configParams,
                            logFile= self.log_file,
                            logDir=logDir,
                            logHead=logHead,
                            transferLearning=transferLearning,
                            crossValidation=crossValidation,
                            bestTrialRetraining=bestTrialRetraining,
                            modelPredict=modelPredict
                        )

    def _initTorch(self):
        torch_init(**self.configParams['numerical_settings'])

    def _initLogging(self, cvFold=-1):
        # init file logging
        if cvFold >= 0:
            logging_settings = self.configFile.get('logging_settings',  DEFAULTS_['logging_settings'])
            log_main_dir = logging_settings['log_main_dir']
            log_sub_dir_prefix = logging_settings['log_sub_dir_prefix']
            use_date_time = logging_settings['use_date_time']

            if cvFold ==0:
                current_date_time = ''
                if use_date_time:
                    current_date_time = datetime.datetime.now().strftime('%Y%m%d_%H%M%S')
                self.configParams['logging_settings']['log_main_dir'] = os.path.normpath(os.path.join(log_main_dir,
                            log_sub_dir_prefix+current_date_time))
            self.configParams['logging_settings']['log_sub_dir_prefix'] = log_sub_dir_prefix + 'm'+str(cvFold)+'_'

        self.log_file = init_file_logging(**self.configParams['logging_settings'])
        self.log_dir = os.path.dirname(self.log_file)

        logging.info('current working directory=%s', os.getcwd())
        logging.info('cmdArgs=%s', pformat(self.cmdArgs))
        logging.info('defaultArgs=%s', pformat(self.defaultArgs))
        logging.info('finalJobParams=%s', pformat(self.configParams))

    def _initJobMisc(self, cvFold):
        self.configParams['training']['study_name'] = self.configFile['training'].get('study_name',
                             DEFAULTS_['training']['study_name']) + '_m'+str(cvFold)
        if self.configParams['training']['study_name'] is not None:
            self.configParams['training']['storage'] = self.configFile['training'].get('storage',
                            DEFAULTS_['training']['storage']).replace('.db','_m'+str(cvFold)+'.db')

    def _initData(self, crossValidation, transferLearning=False, nestedCvFolds=0):
        if transferLearning and not self.transferDataInit:
            dataset = self.configParams['transfer_learning']['dataset']
            self.transferDataInit = True
            self.dataInit = False
        elif not self.dataInit:
            dataset = self.configParams['dataset']
            self.transferDataInit = False
            self.dataInit = True

        seed = self.configParams['numerical_settings']['seed']
        if nestedCvFolds:
            for cvFold in range(nestedCvFolds):
                df_train, df_val, df_test, transformer = get_dataframes(dataset=dataset, seed = seed, cvFold=cvFold, nestedCvFolds=nestedCvFolds)

                # concatenate the train and validation dataset to one dataset when cross-validation is on
                if crossValidation:
                    df_train = pd.concat([df_train, df_val])
                    df_val = None
                if df_val is not None:
                    if df_val.empty:
                        df_val = None
                data = {}
                data['x_column'] = dataset['x_column']
                data['y_column'] = dataset['y_column']
                data['train'] = df_train
                data['val'] = df_val
                data['test'] = df_test
                data['transformer'] = transformer

                self.dataNestedCv.append(data)
        else:
            df_train, df_val, df_test, transformer = get_dataframes(dataset=dataset, seed = seed)

            # concatenate the train and validation dataset to one dataset when cross-validation is on
            if crossValidation:
                df_train = pd.concat([df_train, df_val])
                df_val = None

            self.data['x_column'] = dataset['x_column']
            self.data['y_column'] = dataset['y_column']
            self.data['train'] = df_train
            self.data['val'] = df_val
            self.data['test'] = df_test
            self.data['transformer'] = transformer

    def _getDataFold(self, cvFold):
        self.data = self.dataNestedCv[cvFold]

    def _readConfigFile(self):
        try:
            with open(self.cmdArgs['<configFile>']) as json_config:
                return json.load(json_config, object_pairs_hook=OrderedDict)
        except FileNotFoundError:
            raise FileNotFoundError('Error: Config file not found.')

    @staticmethod
    def _processCmdArgs(cmdArgs):
        # remove None values and convert 'False' and 'True' strings to boolean
        cmdArgs = {k: v for k, v in cmdArgs.items() if v is not None}
        for k, v in cmdArgs.items():
            if v.lower()=="true": cmdArgs[k] = True
            elif v.lower()=="false": cmdArgs[k] = False
            elif v[0]=="[" and v[-1]==']': cmdArgs[k] = v[1:-1].split(',')
        return cmdArgs

    def _setConfigParams(self):
        def getDefaultsToAdd(dinter, confArgs, defArgs):
            for k, v in defArgs.items():
                if isinstance(v, abc.Mapping):
                    dinter[k] = getDefaultsToAdd(dinter.get(k, {}), confArgs.get(k, {}), v)
                    if not dinter[k]: dinter.pop(k)
                else:
                    if k in confArgs: continue
                    dinter[k] = v
            return dinter

        def merge(mergeFrom, mergeTo, overWrite=False, path=None):
            "merges mergeFrom into mergeTo"
            if path is None: path = []
            for key in mergeFrom:
                if key in mergeTo:
                    if isinstance(mergeTo[key], dict) and isinstance(mergeTo[key], dict):
                        merge(mergeFrom[key], mergeTo[key], path + [str(key)])
                    elif mergeTo[key] == mergeFrom[key]:
                        pass # same leaf value
                    elif overWrite:
                         mergeTo[key] = mergeFrom[key]
                    else:
                        raise Exception('Conflict at %s' % '.'.join(path + [str(key)]))
                else:
                    mergeTo[key] = mergeFrom[key]
            return mergeTo

        def cmdArgsToDict(dout, din, args):
            for k, v in din.items():
                if isinstance(v, abc.Mapping):
                    dout[k] = cmdArgsToDict(dout.get(k, {}), v, args)
                    if not dout[k]: dout.pop(k)
                else:
                    updval = args.get('--'+k,None)
                    if updval is not None:
                        if v is not None:
                            dout[k] = type(v)(updval)
                        else:
                            dout[k] = updval
            return dout

        cmdArgsDict = cmdArgsToDict(OrderedDict(), DEFAULTS_, self.cmdArgs)
        self.configParams = copy.deepcopy(self.configFile)
        self.configParams = merge(mergeFrom=cmdArgsDict, mergeTo=self.configParams, overWrite=True)

        #self.configParams = updateArgs(OrderedDict(), self.configFile,  self.cmdArgs)
        defaults_to_add = getDefaultsToAdd(OrderedDict(), self.configParams, DEFAULTS_)
        if defaults_to_add:
            self.defaultArgs = defaults_to_add
            self.configParams = merge(mergeTo=self.configParams, mergeFrom=self.defaultArgs)
        return self.configParams
