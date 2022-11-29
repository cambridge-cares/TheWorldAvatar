from PVLibAgent.kg_utils.utils import PREFIXES


from configobj import ConfigObj

import uuid

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
