from pyderivationagent.conf import AgentConfig
from dotenv import dotenv_values
import os


class VapourtecConfig(AgentConfig):
    VAPOURTEC_DIGITAL_TWIN: str
    VAPOURTEC_STATE_PERIODIC_TIMESCALE: int
    VAPOURTEC_IP_ADDRESS: str
    FCEXP_FILE_CONTAINER_FOLDER: str
    REGISTER_AGENT: bool


def config_vapourtec(env_file: str = None) -> VapourtecConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return VapourtecConfig(dotenv_values(env_file))
    else:
        return VapourtecConfig(os.environ)
