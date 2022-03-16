import json

###--- Properties for Python script ---###
# This python module reads properties from agent_properties.json
class HPLCInputAgentConfig(object):
    def __init__(self, conf_file):
        """
            This method sets the value of the configuration for HPLCInput agent.
            Arguments:
                conf_file - absolute path of the configuration JSON file
        """

        with open(conf_file, "r") as f:
            agent_properties = json.load(f)

        # Full SPARQL Query and Update endpoints of OntoRxn triple store
        self.SPARQL_QUERY_ENDPOINT = agent_properties['KNOWLEDGE_GRAPH']['SPARQL_QUERY_ENDPOINT']
        self.SPARQL_UPDATE_ENDPOINT = agent_properties['KNOWLEDGE_GRAPH']['SPARQL_UPDATE_ENDPOINT']

        # Knowledge graph username and password
        self.KG_USERNAME = agent_properties['KNOWLEDGE_GRAPH']['KG_USERNAME']
        self.KG_PASSWORD = agent_properties['KNOWLEDGE_GRAPH']['KG_PASSWORD']

        # File server URL and upload path
        self.FILE_SERVER_URL = agent_properties['KNOWLEDGE_GRAPH']['FILE_SERVER_URL']

        # Periodic timescale for monitoring Derivation (seconds)
        self.DERIVATION_PERIODIC_TIMESCALE = agent_properties['DERIVATION_CLIENT']['PERIODIC_TIMESCALE']

        # Derivation instances base URL
        self.DERIVATION_INSTANCE_BASE_URL = agent_properties['DERIVATION_CLIENT']['DERIVATION_INSTANCE_BASE_URL']

        # OntoAgent:Service IRI of DoEAgent
        # an example: 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service'
        self.ONTOAGENT_SERVICE = agent_properties['ONTOAGENT']['ONTOAGENT_SERVICE']

        # Periodic timescale for monitoring HPLC Report (seconds)
        self.HPLC_REPORT_PERIODIC_TIMESCALE = agent_properties['HPLC_REPORT_MONITOR']['PERIODIC_TIMESCALE']

        # Digital twin instance of the HPLC that to be monitored
        self.HPLC_DIGITAL_TWIN = agent_properties['HPLC_REPORT_MONITOR']['HPLC_DIGITAL_TWIN']
