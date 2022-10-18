from deviceMap.bms_bacnet_dictionary import *
from bacnet.request_dict_formatter import *
from deviceMap.bms_device_prop_map import *
from bacnet.connection import Connection
from jpsAccess.tsclient_wrapper import *
import datetime as dt
import config.config as config
import schedule
import math
class bmsBacnetAgent():
    def __init__(self):
        pass


    '''
    main function, this runs a scheduled work of update BMS periodically
    '''
    def run(self, duration):
        #TDOO: run periodically, main loop
        schedule.every(duration).minutes.do(self.updateKB)

    '''
    Inits the postgresql tables and triples to endpoint if not exists
    '''
    def init(self,db_name, db_usr, db_pw, db_host):
        bms_prop_dict = bmsPropMap().build()  #read all bms IRIs that needs initiate to a time series table in postgresql
        bms_IRIs = list(bms_prop_dict.values())
        print(bms_IRIs)
        initialize(bms_IRIs)

    def updateKB(self):
        #TODO: connection error handling
        #read data from BACNET
        bms_IRIS, data = self.readDataBacnet()
        timeValues = [dt.datetime.now()]
        #Update through Tsclient
        data = [[v] for v in data]
        print('start updating')
        update(timeValues, bms_IRIS, data)
        print('updated')

    def connect(self):
        self.c = Connection()
        self.c.connect(config.BACNET_IP)

    def readDataBacnet(self):
        #read the mapping from bms entity names to BACNET device
        bms_bacnet_dict = bmsBacnetDict().build() #
        #read the list of property to read for each device
        bms_prop_dict = bmsPropMap().build() #
        bms_IRIs = list(bms_prop_dict.values())
        print('bms_IRIs to read: '+str(bms_IRIs))
        #format the request dictionary to query bacnet
        bacnet_device_list, request_dicts = getRequestDict(bms_bacnet_dict)
        #connect bacnet and read data

        data = []
        for idx in range(len(bacnet_device_list)):
            data.append(self.c.readMultiple(bacnet_device_list[idx], request_dicts[idx]))
        return bms_IRIs, data