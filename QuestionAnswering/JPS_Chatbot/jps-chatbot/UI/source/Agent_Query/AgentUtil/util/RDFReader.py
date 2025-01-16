import os
from pprint import pprint

import rdflib

if __name__ == '__main__':
    from location import FILE_DIR
    from SPARQLWarehouse import *
    from MarieLogger import *
else:
    from .location import FILE_DIR
    from .SPARQLWarehouse import *
    from .MarieLogger import *


class RDFReader:
    def __init__(self):
        self.graph = rdflib.Graph()

    def get_agent_request_attributes(self, agent_name):
        try:
            agent_dir = os.path.join(FILE_DIR, agent_name) + '.owl'
            MarieMessage('Loading agent instance from {}'.format(agent_dir))
            self.graph = rdflib.Graph()
            self.graph.parse(agent_dir)
        except TypeError:
            MarieError('Failed to load an agent owl file {}'.format(agent_name))
        input_rst = self.graph.query(GET_AGENT_INPUT_PARAMETERS)
        # species fits species as the ner label
        output_rst = self.graph.query(GET_AGENT_OUTPUTS)
        # get the inputs and outputs, but also
        url_rst = self.graph.query(GET_HTTP_URL)
        return output_rst, input_rst, url_rst


if __name__ == '__main__':
    rr = RDFReader()
    _or, _ir, _ur = rr.get_agent_request_attributes('PCE_Agent')
    print('============== output ==================')
    for _o in _or:
        print(_o)

    print('============== input  ==================')
    for _i in _ir:
        print(_i)

    print('============== url    ==================')
    for _u in _ur:
        print(_u)
