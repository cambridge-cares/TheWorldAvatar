import json
import os


config_fpath = os.path.join(os.path.dirname(__file__), 'config.json')

class SPARQLEndpoints:
    def __init__(self, ontospecies, ontomops):
        self.ontospecies = ontospecies
        self.ontomops = ontomops

with open(config_fpath, 'r') as file:
    config = json.load(file)

SPARQL_ENDPOINTS = SPARQLEndpoints(config['ontospecies'], config['ontomops'])
