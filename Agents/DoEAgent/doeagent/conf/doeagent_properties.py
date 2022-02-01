import json

###--- Properties for Python script ---###
# This python module reads properties from doeagent_properties.json
class DoEAgentConfig(object):
    def __init__(self, conf_file):
        """
            This method sets the value of the configuration for DoE agent.

            Arguments:
                conf_file - absolute path of the configuration JSON file
        """
        
        with open(conf_file, "r") as f:
            doeagent_properties = json.load(f)

        # Full SPARQL Query and Update endpoints of OntoRxn triple store
        self.SPARQL_QUERY_ENDPOINT = doeagent_properties['KNOWLEDGE_GRAPH']['SPARQL_QUERY_ENDPOINT']
        self.SPARQL_UPDATE_ENDPOINT = doeagent_properties['KNOWLEDGE_GRAPH']['SPARQL_UPDATE_ENDPOINT']

        # Knowledge graph username and password
        self.KG_USERNAME = doeagent_properties['KNOWLEDGE_GRAPH']['KG_USERNAME']
        self.KG_PASSWORD = doeagent_properties['KNOWLEDGE_GRAPH']['KG_PASSWORD']

        # Periodic timescale for monitoring Derivation (seconds)
        self.PERIODIC_TIMESCALE = doeagent_properties['DERIVATION_CLIENT']['PERIODIC_TIMESCALE']

        # OntoAgent:Service IRI of DoEAgent
        # an example: 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service'
        self.ONTOAGENT_SERVICE = doeagent_properties['ONTOAGENT']['ONTOAGENT_SERVICE']

        # Derivation instances base URL
        self.DERIVATION_INSTANCE_BASE_URL = doeagent_properties['DERIVATION_CLIENT']['DERIVATION_INSTANCE_BASE_URL']
