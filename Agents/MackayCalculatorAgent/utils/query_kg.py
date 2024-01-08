'''
# The purpose of this module is to query required values defined in a json via the WorldAvatar KG.
'''

import requests
from requests.auth import HTTPBasicAuth
from utils.kg_access.KGClient import KGClient




# convert heat value unit
def unit_convert_hv(hv):
    return hv/1e6

# convert population to thousands of population
def unit_convert_ppl(ppl):
    return ppl/1000



#Read in a query object, returns the result of query if success
def query_single_ep(queryobj):
    kg_client = KGClient(queryobj['url'], queryobj['url'], kg_user=None if 'user' not in queryobj else queryobj['user'], kg_password=None if 'pwd' not in queryobj else queryobj['pwd'])

    response = kg_client.performQuery(' '.join(queryobj['querystring']))
    results = {}
    if len(response) == 0:
        raise Exception('Unable to query the endpoint: ' + queryobj['name'])
    else:
        for out in queryobj['outputs']:
            results[out] = response[0][out]
    return results

def query_all(querylist):
    querylist = querylist['queries']
    results = {}
    for qobj in querylist:
        try:
            result = query_single_ep(qobj)
            for k in result:
                if k == 'hhv' or k=='lhv':#Plug in unit conversion
                    results[k] = unit_convert_hv(float(result[k]))
                elif k == 'population':
                    results[k] = unit_convert_ppl(float(result[k]))
                else:
                    results[k] = float(result[k])
        except Exception as e:
            raise e
    return results

#TODO call dataagent
def get_updated_data():
    pass
