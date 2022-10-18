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