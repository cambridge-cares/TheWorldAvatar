from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import compchemparser.helpers.ccutils as ccutils
import compchemparser.helpers.utils as utils
import json

# main class for parsed data
class OntoCompChemData:

    def __init__(self):
        self.log = ''
        self.parser = None
        # array of json objects
        self.data = []

    # routine that extracts data from a log ile
    def getData(self, logFile):
        # use cclib package "get_ccattr" utility to determine the log file type
        ccpackage = ccutils.get_ccattr(logFile,"metadata","package")

        # at the moment only Gaussian log files are supported
        if ccpackage in ccutils.CCPACKAGES:
            # set the parser
            self.parser = CcGaussianParser()
        else:
            utils.dienicely("ERROR: Provided log fie is either incorrect or comes from an unsupported quantum chemistry package.")

        # set and parse the log
        self.log = logFile
        self.data = self.parser.parse(self.log)

    # to be implemented by Nenad/Angiras
    def uploadToKG(self):
        print('Uploading to KG, File '+self.log)
        for i, json_dat in enumerate(self.data):
            print('    uploading json entry '+str(i+1))
            # upload call ...
            
    def outputjson(self):
        print('Dumping to JSON, File '+self.log)
        for i, json_dat in enumerate(self.data):
            if len(self.data) > 1:
                json_name = self.log.replace('.log','#'+str(i+1)+'.json')
            else:
                json_name = self.log.replace('.log','.json')

            # dump call ...
            dict_data = json.loads(json_dat)
            with open(json_name, 'w') as outfile:
                json.dump(dict_data, outfile, indent = 4)