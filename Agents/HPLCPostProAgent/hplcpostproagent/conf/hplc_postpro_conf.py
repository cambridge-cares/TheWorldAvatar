from pyderivationagent.conf import AgentConfig
from dotenv import dotenv_values
import os


class HPLCPostProConfig(AgentConfig):
    REGISTER_AGENT: bool


def config_hplc_postpro(env_file: str = None) -> HPLCPostProConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return HPLCPostProConfig(dotenv_values(env_file))
    else:
        return HPLCPostProConfig(os.environ)
