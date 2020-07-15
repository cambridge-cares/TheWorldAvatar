class OntoCompChemdData:

    def __init__(self):
        self.log = ''
        self.data = {}

    def getData(self, logFile, parser, postprocessor):
        """ 
           parser         - Gaussian or Molpro
           postprocessor  - Arkane at the moment
           
        """
        self.log = logFile
        self.data = {}        

        parser.parseLog(self.log, self.data)
        postprocessor.run(self.log, self.data)

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
