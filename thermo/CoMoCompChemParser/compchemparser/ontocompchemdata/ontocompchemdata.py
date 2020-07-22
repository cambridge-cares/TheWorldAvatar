from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import compchemparser.helpers.ccutils as ccutils
import compchemparser.helpers.utils as utils

class OntoCompChemData:

    def __init__(self):
        self.log = ''
        self.parser = None
        self.data = []

    def getData(self, logFile):        
        ccpackage = ccutils.get_ccattr(logFile,"metadata","package")

        if ccpackage in ccutils.CCPACKAGES:
            self.parser = CcGaussianParser()
        else:
            utils.dienicely("ERROR: Provided log fie is either incorrect or comes from an unsupported quantum chemistry package.")

        self.log = logFile
        self.data = self.parser.parse(self.log)