from pathlib import Path
import json
import os

CONF_FOLDER_PATH = str(Path(__file__).absolute().parent)

###--- Properties for Python script ---###
# This python module reads properties from agent_properties.json
class PostProcAgentConfig(object):
    def __init__(self, conf_file):
        """
            This method sets the value of the configuration for ExpSetup agent.
            Arguments:
                conf_file - absolute path of the configuration JSON file
        """

        with open(conf_file, "r") as f:
            agent_properties = json.load(f)

        # Full SPARQL Query and Update endpoints of OntoRxn triple store
        self.SPARQL_QUERY_ENDPOINT = agent_properties['KNOWLEDGE_GRAPH']['SPARQL_QUERY_ENDPOINT']
        self.SPARQL_UPDATE_ENDPOINT = agent_properties['KNOWLEDGE_GRAPH']['SPARQL_UPDATE_ENDPOINT']

        # Fileserver URL
        self.FILESERVER_URL = agent_properties['KNOWLEDGE_GRAPH']['FILESERVER_URL']

        # Credentials for triple store and fileserver
        self.KG_USERNAME = self.read_credential(agent_properties['CREDENTIALS']['BLAZEGRAPH_USERNAME'])
        self.KG_PASSWORD = self.read_credential(agent_properties['CREDENTIALS']['BLAZEGRAPH_PASSWORD'])
        self.FS_USERNAME = self.read_credential(agent_properties['CREDENTIALS']['FILESERVER_USERNAME'])
        self.FS_PASSWORD = self.read_credential(agent_properties['CREDENTIALS']['FILESERVER_PASSWORD'])

        # Periodic timescale for monitoring Derivation (seconds)
        self.PERIODIC_TIMESCALE = agent_properties['DERIVATION_CLIENT']['PERIODIC_TIMESCALE']

        # OntoAgent:Service IRI of DoEAgent
        # an example: 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service'
        self.ONTOAGENT_SERVICE = agent_properties['ONTOAGENT']['ONTOAGENT_SERVICE']

        # Derivation instances base URL
        self.DERIVATION_INSTANCE_BASE_URL = agent_properties['DERIVATION_CLIENT']['DERIVATION_INSTANCE_BASE_URL']

    def read_credential(self, relevant_path):
        full_path = os.path.join(CONF_FOLDER_PATH,relevant_path)
        try:
            cred = Path(full_path).read_text()
            return cred if len(cred) > 0 else None
        except FileNotFoundError:
            raise Exception(
                "Please check if the credentials are provided correctly for triple store <%s> or file server <%s> in: %s" % (
                    self.SPARQL_QUERY_ENDPOINT, self.FILESERVER_URL, full_path
                )
            )
