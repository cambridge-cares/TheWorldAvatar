from deviceMap.bms_bacnet_dictionary import *
from bacnet.request_dict_formatter import *
from deviceMap.bms_device_prop_map import *
from bacnet.connection import Connection
from jpsAccess.tsclient_wrapper import *
import datetime as dt
import config.config as config
import schedule
import requests
import json
import math
from datetime import timedelta

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
        #bms_IRIS, data = self.readDataBacnet()
        bms_IRIS, data = self.readDataWacnet()
        n = 8
        # Subtract 8 hours from datetime object
        # number of hours depend on the timezone of your machine
        # e.g. my machine is on Asia/Singapore timezone, to convert it to UTC
        # I have to minus 8 hours
        final_time = dt.datetime.now() - timedelta(hours=n)
        timeValues = [final_time]
        times = [(t).strftime('%Y-%m-%dT%H:%M:%S') for t in timeValues]

        # insert data bridge function here
        # Create JSONObject, type will be instantaneous, loop through bms_IRIs and data, insert bms_IRIs as keys and data as value
        # Create HTTP request to send to data bridge agent on Astoria

        url = 'http://172.25.186.181:3838/data-bridge-agent/timeseries'
        myobj = {"database":"bms", "namespace":"http://bg_user:bg_astoria*1010@labStack-blazegraph:8080/blazegraph/namespace/bms/sparql", "timeClass":"INSTANTANEOUS" , 
                 "timestamp" : times}
        anotherobj = {}
        for x in bms_IRIS:
            anotherobj.update({x: [data[bms_IRIS.index(x)]]})
        myobj.update({"values":anotherobj})
        requests.post(url, json=myobj)
        # Remove the liens below
        #Update through Tsclient
        data = [[v] for v in data]
        print('Updating')
        #update(timeValues, bms_IRIS, data)
        print('updated')

    #def connect(self):
        #self.c = Connection()
        #self.c.connect(config.BACNET_IP)
		
    def readDataBacnet(self):
        #read the mapping from bms entity names to BACNET device
        bms_bacnet_dict = bmsBacnetDict().build() #
        #read the list of property to read for each device
        bms_prop_dict = bmsPropMap().build() #form IRIS with "namespace + V_name_of_variable"
        bms_IRIs = list(bms_prop_dict.values())
        print('bms_IRIs to read: '+str(bms_IRIs))
        #format the request dictionary to query bacnet
        bacnet_device_list, request_dicts = getRequestDict(bms_bacnet_dict)
        #connect bacnet and read data

        data = []
        for idx in range(len(bacnet_device_list)):
            data.append(self.c.readMultiple(bacnet_device_list[idx], request_dicts[idx]))
        return bms_IRIs, data
    
    def readDataWacnet(self):
        mainurl = 'http://localhost:47800/api/v1/bacnet/devices/3616029/objects/'
        bms_bacnet_dict = bmsBacnetDict().buildIDList()
        bms_prop_dict = bmsPropMap().build()
        bms_IRIs = list(bms_prop_dict.values())
        data = []
        for id in bms_bacnet_dict:
            url = mainurl + bms_bacnet_dict[id]
            y = requests.get(url)
            json_string = y.json()
            data.append(json_string["present-value"])
        return bms_IRIs, data
