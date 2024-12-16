from config import config
#bms IRI to bacnet input ID
from config.config import BMS_DEVICE_FILE, DOMAIN
import csv

#{bms_IRI, bms file_name}

class bmsPropMap():
    def __init__(self):
        pass

    def build(self):
        namedict = {}
        with open(BMS_DEVICE_FILE) as csv_file:
            csv_reader = csv.reader(csv_file, delimiter=',')
            idx = 0
            for row in csv_reader:
                fullIRI = '{}{}'.format(DOMAIN, row[0].strip())
                namedict[idx] = fullIRI
                idx = idx+1
        return namedict


if __name__ == "__main__":
    b= bmsPropMap()
    b.build()