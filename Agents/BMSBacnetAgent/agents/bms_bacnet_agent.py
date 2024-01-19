from deviceMap.bms_bacnet_dictionary import *
from bacnet.request_dict_formatter import *
from deviceMap.bms_device_prop_map import *
from bacnet.connection import Connection
from jpsAccess.tsclient_wrapper import *
import datetime as dt
import config.config as config
import schedule
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
    def init(self):
        bms_prop_dict = bmsPropMap().build()  #
        print('return:' + str(bms_prop_dict))
        bms_IRIs = list(bms_prop_dict.keys())
        initialize(bms_IRIs)

    def updateKB(self):
        #TODO: connection error handling
        #read data from BACNET
        bms_IRIS, data = self.readDataBacnet()
        timeValues = [(dt.datetime.now() - dt.timedelta(hours=2*i)) for i in range(len(bms_IRIS))]
        #Update through Tsclient
        print(bms_IRIS)
        print(data)
        update(timeValues, bms_IRIS, data)




    def readDataBacnet(self):
        #read the mapping from bms entity names to BACNET device
        print(bmsBacnetDict)
        bms_bacnet_dict = bmsBacnetDict().build() #
        #read the list of property to read for each device
        bms_prop_dict = bmsPropMap().build() #
        print('return:'+str(bms_prop_dict))
        bms_IRIs = list(bms_prop_dict.keys())
        print(bms_IRIs)
        #format the request dictionary to query bacnet
        bacnet_device_list, request_dicts = getRequestDict(bms_bacnet_dict, bms_prop_dict)
        #connect bacnet and read data
        c = Connection()
        print(Connection)
        print(c)
        c.connect(config.BACNET_IP)#TODO: MOCK
        data = []
        for idx in range(len(bacnet_device_list)):
            #TODO: connection error handling
            data.append(c.readMultiple(bacnet_device_list[idx], request_dicts[idx]))
        return bms_IRIs, data