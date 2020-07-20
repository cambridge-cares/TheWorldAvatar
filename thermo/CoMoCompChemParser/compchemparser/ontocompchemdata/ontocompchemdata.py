from parsers.ccgaussian_parser import CcGaussianParser
import helpers.ccutils as ccutils
import helpers.utils as utils

class OntoCompChemData:

    def __init__(self):
        self.log = ''
        self.parser = None
        self.data = {}

    def getData(self, logFile):        
        ccpackage = ccutils.get_ccattr(logFile,"metadata","package")

        if ccpackage in ccutils.CCPACKAGES:
            self.parser = CcGaussianParser()
        else:
            utils.dienicely("ERROR: Provided log fie is either incorrect or comes from an unsupported quantum chemistry package.")

        self.log = logFile
        self.data = {}    

        self.parser.parse(self.log)

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