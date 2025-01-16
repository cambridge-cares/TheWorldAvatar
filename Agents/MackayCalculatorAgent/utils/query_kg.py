'''
# The purpose of this module is to query required values defined in a json via the WorldAvatar KG.
'''

import requests
from requests.auth import HTTPBasicAuth
#from utils.kg_access.KGClient import KGClient



# Divide by million
def unit_convert_million(value):
    return value/1e6

# Divide by thousands
def unit_convert_thousand(value):
    return value/1000

def query_data_agent(agent_url):
    return requests.get(agent_url).json()




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
                    results[k] = unit_convert_million(float(result[k]))
                elif k == 'population':
                    results[k] = unit_convert_thousand(float(result[k]))
                else:
                    results[k] = float(result[k])
        except Exception as e:
            raise e
    return results