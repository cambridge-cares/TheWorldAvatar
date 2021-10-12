import urllib.request
import urllib.response
import logging
from urllib.error import HTTPError

if __name__ == '__main__':
    from SPARQLWrapper import SPARQLWrapper, JSON
    from MarieLogger import MarieIOLog, MarieMessage, MarieError
else:
    from SPARQLWrapper import SPARQLWrapper, JSON
    from .MarieLogger import MarieIOLog, MarieMessage, MarieError


def query_blazegraph(query, namespace):
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    return results


# take the base url and request data, return a full and encoded url
@MarieIOLog
def construct_http_request(_url, _data):
    _url += "?"
    parameters = []
    for key in _data:
        if _data[key] is not None:
            value = str(_data[key])
            if isinstance(value, list):
                value = key + '=[' + ','.join(value) + ']'
            else:
                value = key + '=' + value
            parameters.append(value)
    full_url = _url + '&'.join(parameters)
    print('FULL_URL', full_url)
    MarieMessage(full_url)
    return

@MarieIOLog
def make_simple_http_request(_url, _data):
    full_url = construct_http_request(_url, _data)
    try:
        req = urllib.request.Request(full_url)
        return urllib.request.urlopen(req).read()

    except HTTPError:
        MarieError('HTTPError')
    return None


# simple test of the module
if __name__ == '__main__':
    construct_http_request('http.dummy', {'key1': ['hi'], 'key2': 'hello'})
