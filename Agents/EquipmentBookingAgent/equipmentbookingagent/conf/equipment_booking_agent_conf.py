from pyderivationagent.conf import Config
from pyderivationagent.conf import config_generic


class EquipmentBookingAgentConfig(Config):
    """
    This is a config class for the EquipmentBookingAgent.
    It is a subclass of Config and can be extended to provide custom configurations for developed agents.
    It has the following fields:
      - SPARQL_QUERY_ENDPOINT: The SPARQL endpoint to be used for querying the knowledge graph.
      - SPARQL_UPDATE_ENDPOINT: The SPARQL endpoint to be used for updating the knowledge graph.
      - KG_USERNAME: The username to access the SPARQL endpoint.
      - KG_PASSWORD: The password to access the SPARQL endpoint.
    """
    SPARQL_QUERY_ENDPOINT: str = 'https://www.theworldavatar.com:1010/careslab/blazegraph/namespace/caresAsset/sparql'
    SPARQL_UPDATE_ENDPOINT: str = 'http://10.25.188.58:3838/blazegraph/namespace/test/sparql'
    KG_USERNAME: str = 'bg_user'
    KG_PASSWORD: str = 'admin'

def config_equipment_booking_agent(env_file: str = None) -> EquipmentBookingAgentConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(EquipmentBookingAgentConfig, env_file)
