from PVLibAgent.kg_utils.utils import PREFIXES
from PVLibAgent.data_retrieval.query_data import QueryData

from configobj import ConfigObj

import uuid
import logging

class check_data_iris:
    def check_data_iris_and_create_if_not_exist(filepath):
        props = ConfigObj(filepath)
        try:
            ac_power = props['AC_Power']
        except KeyError:
            raise KeyError('Key "AC_Power" is missing in properties file: ' + filepath)
        if ac_power == '':
            ac_power = PREFIXES['ts'] + 'pvlib' + '_' + 'ac_power' + '_' + str(uuid.uuid4())

        try:
            dc_power = props['DC_Power']
        except KeyError:
            raise KeyError('Key "DC_Power" is missing in properties file: ' + filepath)
        if dc_power == '':
            dc_power = PREFIXES['ts'] + 'pvlib' + '_' + 'dc_power' + '_' + str(uuid.uuid4())

        file = open(filepath, 'w')
        file.write("DC_Power="+dc_power + "\n" + "AC_Power="+ac_power)
        print(str(ac_power))
        print(str(dc_power))
        data_iri_list = [ac_power, dc_power]
        return data_iri_list

    def check_data_iris_and_create_if_not_exist_AC_only(pv_iri, query_endpoint):
       
        try:       
            response = QueryData.query_PV_data_iri(pv_iri, query_endpoint)
            
            if len(response) == 0:
                  logging.info("create new data iri")
                  ac_power = PREFIXES['ts'] + 'pvlib' + '_' + 'ac_power' + '_' + str(uuid.uuid4())
            else:
                ac_power = response[0]["data_iri"]

            logging.info("data iri:"+str(ac_power))

        except KeyError:
            raise KeyError('ac power data iri not found/failed')
       

        data_iri_list = [ac_power]
        return data_iri_list
