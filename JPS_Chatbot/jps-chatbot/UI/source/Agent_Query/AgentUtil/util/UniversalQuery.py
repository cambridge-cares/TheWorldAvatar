import urllib.request
import urllib.response
import logging
from http.client import InvalidURL
from urllib.error import HTTPError, URLError
import urllib.parse
import urllib.request

import urllib3
from SPARQLWrapper import SPARQLWrapper, JSON

if __name__ == '__main__':
    from MarieLogger import MarieIOLog, MarieMessage, MarieError

else:
    from .OntoSpeciesManager import OntoSpecies
    from .MarieLogger import MarieIOLog, MarieMessage, MarieError

osc = OntoSpecies()

def query_blazegraph(query, namespace):
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    return results


# take the base url and request data, return a full and encoded url
# @MarieIOLog
def construct_http_request(_url, _data):
    _url += "?"
    parameters = []
    for key in _data:
        if _data[key] is not None:
            value = _data[key]
            if isinstance(value, list):
                if 'http://kg.cmclinnovations.com:5001/api/model/predict' in _url:
                    value = [osc.findSMILES(osc.findOntoSpecies(v)) for v in value]
                value = key + '=[' + ','.join(value) + ']'

            else:
                try:
                    if ' ' in value or '=' in value:
                        value = urllib.parse.quote(str(value))
                except TypeError:
                    MarieError('Failed to quote value in URL {}'.format(value))
                value = key + '=' + str(value)

            parameters.append(value)
    full_url = _url + '&'.join(parameters)
    MarieMessage(full_url)
    return full_url


@MarieIOLog
def make_simple_http_request(_url, _data, _thermo_agent):
    MarieMessage('Requesting URL {}'.format(_url))
    print('data given to request', _data)
    full_url = construct_http_request(_url, _data)
    # req = urllib.request.Request(full_url)
    # return urllib.request.urlopen(req).read()
    # OVERRIDE THE REQUEST THING IF full_url is requesting thermo_agent
    if 'thermo_agent' in _url:
        _species = _data['species'] if 'species' in _data else None
        t = _data['temperature'] if 'temperature' in _data else None
        p = _data['pressure'] if 'pressure' in _data else None
        attribute = _data['attribute'] if 'attribute' in _data else None
        print('HERE WE GO')
        return _thermo_agent.callThermoAgent(species=_species, temperature=t, pressure=p, attribute=attribute)
    try:
        req = urllib.request.Request(full_url)
        return urllib.request.urlopen(req).read()

    except HTTPError:
        MarieError('HTTPError with URL {}'.format(full_url))
        return None
    except URLError:
        MarieError('URL Error with URL {}'.format(full_url))
        return None

    except InvalidURL:
        MarieError('URL Error with URL {}'.format(full_url))
        return None


# simple test of the module
if __name__ == '__main__':
    # construct_http_request('http.dummy', {'key1': ['hi'], 'key2': 'hello'})
    convert_species_to_SMILES('co2')
