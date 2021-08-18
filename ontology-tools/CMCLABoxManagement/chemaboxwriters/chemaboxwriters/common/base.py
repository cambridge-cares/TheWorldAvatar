from compchemparser.helpers.utils import readFile, fileExists, getRefName
class NotSupportedStage(Exception):
    """Raise for not supported abox processing stages. """

class StageHandler:
    """
    Generic wrapper to create concrete handlers for abox writing stages.
    """
    def __init__(self, handlerFunc, inStage, outStage,
                 disableFileToStrConv=False, handlerFuncKwargs={},
                 fileExt='', fileWriter=None,  fileWriterKwargs={},
                 unrollListInput=True):
        self.handlerFunc= handlerFunc
        self.inStage= inStage
        self.outStage= outStage
        self.disableFileToStrConv= disableFileToStrConv
        self.fileWriter= fileWriter
        self.fileWriterKwargs= fileWriterKwargs
        self.handlerFuncKwargs= handlerFuncKwargs
        self.fileExt=fileExt
        self.unrollListInput=unrollListInput

    def run(self, input, inStage, outPath):
        output, outStage= input, inStage
        writtenFiles = []
        if inStage == self.inStage:
            output, outStage= self.handle_input(input)
            if self.fileWriter is not None:
                writtenFiles = self.handle_write_output(output, outPath)
        return output, outStage, writtenFiles

    def set_handler_func_kwargs(self,handlerFuncKwargs):
        self.handlerFuncKwargs = handlerFuncKwargs
        return self

    def set_file_writer_kwargs(self,fileWriterKwargs):
        self.fileWriterKwargs = fileWriterKwargs
        return self

    def set_file_writer(self, fileWriter):
        self.fileWriter = fileWriter
        return self

    def set_file_ext(self, fileExt):
        self.fileExt = fileExt
        return self

    def handle_input(self,input):
        if isinstance(input, list) and self.unrollListInput:
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
            if isinstance(input, list):
                input = [readFile(inp) if fileExists(inp) else inp for inp in input]
            else:
                if fileExists(input): input = readFile(input)
        output = self.handlerFunc(input,**self.handlerFuncKwargs)
        return output

    def handle_write_output(self, output, outPath):
        writtenFiles = []
        if isinstance(output, list):
            writtenFiles.extend(self.write_list_output(output, outPath))
        else:
            writtenFiles.append(self.write_single_output(output, outPath))
        return writtenFiles

    def write_list_output(self,outputs, outPath):
        writtenFiles = []
        for i, output in enumerate(outputs):
            writtenFiles.append(self.write_single_output(output,outPath, i, len(outputs)))
        return writtenFiles

    def write_single_output(self,output, outPath, jobId=-1, jobNum=-1):
        refOutPath = getRefName(outPath, jobId, jobNum, self.fileExt)
        self.fileWriter(refOutPath, output, **self.fileWriterKwargs)
        return refOutPath

class Pipeline:
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """
    def __init__(self, supportedStages):
        self.handlers= {}
        self.supportedStages=supportedStages
        self.writtenFiles = []

    def add_handler(self, handler, handlerName):
        self.handlers[handlerName]=handler
        return self

    def execute(self, inputs, inputType, outPath=None):
        for handler in self.handlers.values():
            if inputType not in self.supportedStages:
                requestedStage=inputType.name.lower()
                raise NotSupportedStage(f"Error: Stage: '{requestedStage}' is not supported.")
            inputs, inputType, writtenFiles = handler.run(inputs, inputType, outPath)
            if writtenFiles: self.writtenFiles.extend(writtenFiles)
        return inputs, inputType