
class Handler:
    """
    Generic wrapper to create concrete handlers for abox writing stages.
    """
    def __init__(self, handlerFunc, inStage, outStage, fileWriter=None, fileExt=None):
        self.handlerFunc= handlerFunc
        self.inStage= inStage
        self.outStage= outStage
        self.fileWriter= fileWriter
        self.fileExt= fileExt

    def run(self, input, inStage, outDir, outBaseName):
        output, outStage= input, inStage
        if inStage == self.inStage:
            output= self.handlerFunc(input)
            outStage= self.outStage
            if self.fileWriter is not None: self.fileWriter(output, outDir, outBaseName, self.fileExt)
        return output, outStage

    def set_write_all_stages(self, writeAllStages):
        self.writeAllStages= writeAllStages
        return self