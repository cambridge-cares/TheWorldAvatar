from config import config
#bms IRI to bacnet input ID
from config.config import BMS_DEVICE_FILE
import csv

class bmsPropMap():
    def __init__(self):
        pass

    def build(self):
        namedict = {}
        #TODO: read it from somewhere

        with open(BMS_DEVICE_FILE) as csv_file:
            csv_reader = csv.reader(csv_file, delimiter=',')
            for row in csv_reader:
                namedict[row[0]] = row[1]
        return namedict


if __name__ == "__main__":
    b= bmsPropMap()
    b.build()