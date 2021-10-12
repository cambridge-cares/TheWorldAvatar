if __name__ == '__main__':
    from util.UniversalQuery import make_simple_http_request
    from util.MarieLogger import MarieIOLog
else:
    from .util.UniversalQuery import make_simple_http_request
    from .util.MarieLogger import MarieIOLog


@MarieIOLog
def construct_request(inputs, outputs, _url):
    # label: e.g. species, key for key-value pair
    # value: e.g. inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3cls
    _data = {}
    for i in inputs:
        i_l = i['label']
        i_v = i['value']
        _data[i_l] = i_v
    for o in outputs:
        o_l = o['label']
        o_v = o['value']
        if 'qualifiers' in o:
            for o_q in o['qualifiers']:
                o_q_l = o_q['label']
                o_q_v = o_q['value']
                _data[o_q_l] = o_q_v

        _data[o_l] = o_v
    return _data


class AgentCaller:
    def __init__(self):
        pass

    def call(self, _inputs, _outputs, _url):
        _data = construct_request(_inputs, _outputs, _url)
        response = make_simple_http_request(_url, _data)
        return response


if __name__ == '__main__':
    # inputs = [{'label': 'species', 'value': 'inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3'}]
    inputs = [{'label': 'species', 'value': 'co2'}]
    outputs = [{'label': 'attribute', 'value': 'entropy', 'qualifiers': [{'label': 'pressure', 'value': '1522.1 pa'},
                                                                         {'label': 'temperature',
                                                                          'value': '123245 k'}]}]
    url = 'http://127.0.0.1:5000/thermo_agent'
    ac = AgentCaller()
    r = ac.call(inputs, outputs, url)
    print(r)
