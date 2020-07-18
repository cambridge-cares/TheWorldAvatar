class OntoCompChemData:

    def __init__(self):
        self.log = ''
        self.parser = None
        self.data = {}

    def setParser(self, parser):
        self.parser = parser

    def getData(self, logFile):
        """ 
           parser         - Gaussian or Molpro
           postprocessor  - Arkane at the moment
           
        """
        self.log = logFile
        self.data = {}    

        self.parser.parseLog(self.log)

        #self.checkParsedData()

        #postprocessor.run(self.log, self.data)

        #self.checkPostProcessedData()

        #1 load arkane templates
        #2 create input files (streams if possible)
        #3 run Arkane and get output
        #4 postprocess output - 
        #    Empirical formula
        #    Atom counts
        #    Rotational constants (convert these from moments of inertia with Angiras’ help)
        #    Rotational constants value
        #    Rotational constants units
        #    Rotational constants size
        #    Atomic mass value (use Arkane isotopes module?)

    # check the parser generated keys
    def checkParsedData(self):
        pass

    # check the postprocessor generated keys
    def checkPostProcessedData(self):
        pass