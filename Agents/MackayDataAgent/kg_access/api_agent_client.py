import requests
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import ONTODERIVATION_DERIVATIONWITHTIMESERIES
import json
import logging

class APIAgentClient:
    def __init__(self, triples_base_url:str,agent_url: str, agent_iri: str, endpoint:str):
        self.api_agent = None
        self.agent_url = agent_url
        self.triples_base_url = triples_base_url
        self.kg_endpoint = endpoint
        self.agent_iri = agent_iri


    def check_if_API_registered(self, api_iri):
        check_route = self.agent_url.split('/')
        last  = -2 if check_route[-1] == '' else -1
        check_route = '/'.join(check_route[:last])+'/check_api'
        res = requests.get(check_route, json={'api_iri':api_iri})
        result = res.json()
        if 'result' not in result:
            return False
        return bool(result['result'])

    def register_API(self, api_iri):
        derivation_client = PyDerivationClient(self.triples_base_url, self.kg_endpoint, self.kg_endpoint)
        derivation = derivation_client.createSyncDerivationForNewInfo(self.agent_iri, [api_iri],
                                                                  ONTODERIVATION_DERIVATIONWITHTIMESERIES)
        derivation_iri = derivation.getIri()
        logging.info('{} registered to API Agent'.format(api_iri))
        return derivation_iri