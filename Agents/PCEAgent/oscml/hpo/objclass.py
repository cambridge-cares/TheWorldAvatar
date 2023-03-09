from oscml.hpo.hpo_utils import NN_empty_torch_cache
class ObjectiveTask:
    def __init__(self, funcHandle=None, objParamsKey=None, extArgs=[]):
        self.funcHandle = funcHandle
        self.extArgs = extArgs
        self.objParamsKey = objParamsKey

    def run(self, *intArgs):
        return self.funcHandle(*intArgs, *self.extArgs)

class Objective:
    def __init__(self, modelName=None,
                 data=None, config=None, logFile= None, logDir=None,
                 logHead=None):
        self.modelName = modelName
        self.data = data
        self.objConfig = {
            'config': config,
            "log_file": logFile,
            "log_dir": logDir,
            "log_head": logHead
        }
        self.objParams = {
            'preModelCreateTasks': [],
            'postModelCreateTasks': [],
            'postTrainingTasks': [],
            'modelCreator': None,
            'modelTrainer': None
        }

        self.trial = None
        self.obj_val = None
        self.model = None
        self.crossValidation = False

    def addPreModelCreateTask(self, funcHandle, objParamsKey, extArgs=[]):
        preModelCreateTasks = self.objParams['preModelCreateTasks']
        preModelCreateTasks.append(ObjectiveTask(funcHandle=funcHandle, objParamsKey=objParamsKey, extArgs=extArgs))
        self.objParams['preModelCreateTasks'] = preModelCreateTasks

    def addPostModelCreateTask(self, funcHandle, objParamsKey, extArgs=[]):
        postModelCreateTasks = self.objParams['postModelCreateTasks']
        postModelCreateTasks.append(ObjectiveTask(funcHandle=funcHandle, objParamsKey=objParamsKey, extArgs=extArgs))
        self.objParams['postModelCreateTasks'] = postModelCreateTasks
        #self.postModelCreateTasks.append(ObjectiveTask(funcHandle=funcHandle, objParamsKey=objParamsKey, extArgs=extArgs))

    def addPostTrainingTask(self, funcHandle, objParamsKey, extArgs=[]):
        postTrainingTasks = self.objParams['postTrainingTasks']
        postTrainingTasks.append(ObjectiveTask(funcHandle=funcHandle, objParamsKey=objParamsKey, extArgs=extArgs))
        self.objParams['postTrainingTasks'] = postTrainingTasks
        #self.postTrainingTasks.append(ObjectiveTask(funcHandle=funcHandle, objParamsKey=objParamsKey, extArgs=extArgs))

    def setModelCreator(self, funcHandle, extArgs=[]):
        self.objParams['modelCreator'] = ObjectiveTask(funcHandle=funcHandle, extArgs=extArgs)

    def setModelTrainer(self, funcHandle, extArgs=[]):
        self.objParams['modelTrainer'] = ObjectiveTask(funcHandle=funcHandle, extArgs=extArgs)

    def setCrossValidation(self, crossValidation):
        self.crossValidation= crossValidation

    def _doTraining(self):
        modelTrainer = self.objParams['modelTrainer']
        if modelTrainer is not None:
            self.obj_val = modelTrainer.run(self.trial, self.model, self.data, self.objConfig, self.objParams)

    def _createModel(self):
        modelCreator = self.objParams['modelCreator']
        # for neural models run with cross_validation, the model is created in the trainer call
        if not self.crossValidation and modelCreator is not None:
            self.model = modelCreator.run(self.trial, self.data, self.objConfig, self.objParams)

    def __call__(self, trial):
        self._releaseMemory()
        self._setTrial(trial)
        self._doPreModelCreateTasks()
        self._createModel()
        self._doPostModelCreateTasks()
        self._doTraining()
        self._doPostTrainingTasks()
        return self.obj_val

    def _setTrial(self, trial):
        self.trial = trial

    def _doPreModelCreateTasks(self):
        preModelCreateTasks = self.objParams['preModelCreateTasks']
        for task in preModelCreateTasks:
            task_result= task.run(self.trial, self.data, self.objConfig, self.objParams)
            if task.objParamsKey is not None:
                self.objParams[task.objParamsKey] = task_result

    def _doPostModelCreateTasks(self):
        postModelCreateTasks = self.objParams['postModelCreateTasks']
        for task in postModelCreateTasks:
            task_result = task.run(self.trial, self.model, self.data, self.objConfig, self.objParams)
            if task.objParamsKey is not None:
                self.objParams[task.objParamsKey] = task_result

    def _doPostTrainingTasks(self):
        postTrainingTasks = self.objParams['postTrainingTasks']
        for task in postTrainingTasks:
            task_result = task.run(self.trial, self.model, self.data, self.objConfig, self.objParams)
            if task.objParamsKey is not None:
                self.objParams[task.objParamsKey] = task_result

    @staticmethod
    def _releaseMemory():
        NN_empty_torch_cache()