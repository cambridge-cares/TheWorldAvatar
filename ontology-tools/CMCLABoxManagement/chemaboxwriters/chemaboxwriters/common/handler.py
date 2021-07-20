class Handler:
    """
    Generic wrapper to create concrete handlers for abox writing stages.
    """
    def __init__(self, handlerFunc, inStage, outStage, writeAllStages=True):
        self.handlerFunc= handlerFunc
        self.inStage= inStage
        self.outStage= outStage
        self.writeAllStages= writeAllStages

    def __call__(self, input, inStage):
        output, outStage= input, inStage
        if inStage == self.inStage:
            output= self.handlerFunc(input)
            outStage= self.outStage
        return output, outStage

    def set_write_all_stages(self, writeAllStages):
        self.writeAllStages= writeAllStages
        return self