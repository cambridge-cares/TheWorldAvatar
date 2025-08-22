import csv
from config import config
#bms_IRI, bacnet_prop_name
class bmsBacnetDict():
    def __init__(self):
        self.file_address = config.DEVICE_NAME_MAPPING

    def build(self):
        namedict = {}
        with open(self.file_address) as csvfile:
            reader = csv.reader(csvfile, delimiter=',')
            idx = 0
            for row in reader:
                namedict[idx] = row[2].strip()
                idx = idx+1
        return namedict
    
    def buildIDList(self):
        idDict = {}
        with open(self.file_address) as csvfile:
            reader = csv.reader(csvfile, delimiter=',')
            idx = 0
            for row in reader:
               objecttype = row[1].strip()
               objectinstance = row[2].strip()
               idDict[idx] = str(objecttype + "." + objectinstance)
               idx =idx+1
        return idDict