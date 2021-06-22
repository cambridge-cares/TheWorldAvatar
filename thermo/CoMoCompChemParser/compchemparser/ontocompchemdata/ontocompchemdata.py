from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import compchemparser.helpers.ccutils as ccutils
import compchemparser.helpers.utils as utils
import json

#Added by Nenad Krdzavac


# main class for parsed data
class OntoCompChemData:

    def __init__(self,aboxwriter):
        self.log = ''
        self.parser = None
        # array of json objects
        self.data = []
        self.jsonpaths = []
        self.aboxwriter = aboxwriter

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
        for i, json_data in enumerate(self.data):
            print('    uploading json entry '+str(i+1))
            # upload call ...

    def outputjson(self):
        print('Dumping to JSON, File '+self.log)
        print("self.log : ", self.log)
        for i, json_dat in enumerate(self.data):
            if len(self.data) > 1:
                 self.jsonpaths.append(self.log + '_' + str(i+1)+'.json')                

            else:
                 self.jsonpaths.append(self.log + '.json')                

            dict_data = json.loads(json_dat)
            with open(self.jsonpaths[i], 'w') as outfile:
                json.dump(dict_data, outfile, indent = 4)


    def output_abox_csv(self):
        if not self.jsonpaths:
            self.outputjson()
        for jsonpath in self.jsonpaths:
            self.aboxwriter(jsonpath)



















