ANALOG_INPUT = "analogInput"
UNITS = "units"
PRESENT_VALUE = "presentValue"
BACNET_PROP = [PRESENT_VALUE, UNITS]
def getRequestDict(bms_bacnet_dict, bms_prop_dict):
    device_list, req_dicts = [],[]
    #TODO: the prop_list might not be necessary, depends on real maping
    for bms_id, bacnet_id in bms_bacnet_dict.items():
        inputIds = bms_prop_dict[bms_id]
        device_list.append(bacnet_id)
        req_dicts.append(getSingleRequestDict(bacnet_id, inputIds))
    return device_list, req_dicts

# value = {'value': 10.01, 'type': 'float', 'unit': {'isSI': True, 'unit': 'CM'}}
#prop = ('analogInput',4,'units')
#unitH = fx.read_property(prop)
#prop = ('analogInput',4,'presentValue')
#'analogInput:1094': ['objectName', 'presentValue', 'statusFlags', 'units','description'],
def getSingleRequestDict(deviceId, inputIds):
    propDict = {('analogInput:'+str(inputid)):BACNET_PROP.copy() for inputid in inputIds }
    rDict = {'address': deviceId, 'objects':propDict}
    return rDict