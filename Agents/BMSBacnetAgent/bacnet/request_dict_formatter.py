ANALOG_INPUT = "analogInput"
UNITS = "units"
PRESENT_VALUE = "presentValue"
BACNET_PROP = [PRESENT_VALUE, UNITS]
from config.config import DEVICE_ID

def getRequestDict(bms_bacnet_dict):
    device_list, req_dicts = [],[]
    for bms_IRI, bacnet_prop_name in bms_bacnet_dict.items():
        inputIds = [bacnet_prop_name]
        device_list.append(DEVICE_ID)
        req_dicts.append(getSingleRequestDict(DEVICE_ID, inputIds))
    return device_list, req_dicts

# value = {'value': 10.01, 'type': 'float', 'unit': {'isSI': True, 'unit': 'CM'}}
#prop = ('analogInput',4,'units')
#unitH = fx.read_property(prop)
#prop = ('analogInput',4,'presentValue')
#'analogInput:1094': ['objectName', 'presentValue', 'statusFlags', 'units','description'],
#One device with multiple analog inputsIds
def getSingleRequestDict(deviceId, inputIds):
    propDict = {'analogInput:'+str(inputid):BACNET_PROP.copy() for inputid in inputIds }
    rDict = {'address': deviceId, 'objects':propDict}
    return rDict