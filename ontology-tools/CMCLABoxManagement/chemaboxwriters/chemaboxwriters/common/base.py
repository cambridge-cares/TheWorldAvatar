from compchemparser.helpers.utils import readFile, fileExists, getRefName

class StageHandler:
    """
    Generic wrapper to create concrete handlers for abox writing stages.
    """
    def __init__(self, handlerFunc, inStage, outStage,
                 disableFileToStrConv=False, handlerFuncKwargs={},
                 fileExt='', fileWriter=None,  fileWriterKwargs={}):
        self.handlerFunc= handlerFunc
        self.inStage= inStage
        self.outStage= outStage
        self.disableFileToStrConv= disableFileToStrConv
        self.fileWriter= fileWriter
        self.fileWriterKwargs= fileWriterKwargs
        self.handlerFuncKwargs= handlerFuncKwargs
        self.fileExt=fileExt

    def run(self, input, inStage, outPath):
        output, outStage= input, inStage
        if inStage == self.inStage:
            output, outStage= self.handle_input(input)
            if self.fileWriter is not None:
                self.handle_write_output(output, outPath)
        return output, outStage

    def handle_input(self,input):
        if isinstance(input, list):
            output= self.handle_list_input(input)
        else:
            output =self.handle_single_input(input)
        outStage= self.outStage
        return output, outStage

    def handle_list_input(self,inputList):
        outputs= []
        for input in inputList:
            output = self.handle_single_input(input)
            if isinstance(output, list): outputs.extend(output)
            else: outputs.append(output)
        return outputs

    def handle_single_input(self,input):
        if not self.disableFileToStrConv:
            if fileExists(input): input = readFile(input)
        output = self.handlerFunc(input,**self.handlerFuncKwargs)
        return output

    def handle_write_output(self, output, outPath):
        if isinstance(output, list):
            self.write_list_output(output, outPath)
        else:
            self.write_single_output(output, outPath)

    def write_list_output(self,outputs, outPath):
        for i, output in enumerate(outputs):
            self.write_single_output(output,outPath, i, len(outputs))

    def write_single_output(self,output, outPath, jobId=-1, jobNum=-1):
        refOutPath = getRefName(outPath, jobId, jobNum, self.fileExt)
        self.fileWriter(refOutPath, output, **self.fileWriterKwargs)

class Pipeline:
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """
    def __init__(self):
        self.handlers= []

    def add_handler(self, handler):
        self.handlers.append(handler)
        return self

    def execute(self, inputs, inputType, outPath=None):
        for handler in self.handlers:
            inputs, inputType = handler.run(inputs, inputType, outPath)
        return inputs, inputType