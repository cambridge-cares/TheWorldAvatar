from pyderivationagent.conf import AgentConfig
from pyderivationagent.conf import config_generic


class VapourtecConfig(AgentConfig):
    VAPOURTEC_DIGITAL_TWIN: str
    VAPOURTEC_STATE_PERIODIC_TIMESCALE: int
    VAPOURTEC_IP_ADDRESS: str
    FCEXP_FILE_CONTAINER_FOLDER: str
    FCEXP_FILE_HOST_FOLDER: str
    FCEXP_TEMPLATE_FILENAME: str
    DRY_RUN: bool


def config_vapourtec_agent(env_file: str = None) -> VapourtecConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(VapourtecConfig, env_file)
