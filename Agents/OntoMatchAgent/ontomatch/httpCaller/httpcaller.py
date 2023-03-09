'''
module for http requesting other agents
'''

from ontomatch.httpCaller.yellowpage import AGENT_IRIS
import requests
class caller():
    def __init__(self):
        pass

    def callAgent(self, agentName, params, method = "POST"):
        if agentName not in AGENT_IRIS:
            raise KeyError("Agent not found")
        IRI = AGENT_IRIS[agentName]
        if method == "POST":
            r = requests.post(IRI,params=params)
        elif method == "GET":
            r = requests.get(IRI,params=params)
        else:
            raise Exception("Invalid request. Invalid http method")
        if r.status_code != 200:
            raise ConnectionError()
        data = r.json()
        #{"result":{'': ,...}}
        return data['result']


