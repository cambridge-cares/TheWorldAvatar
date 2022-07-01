from pyderivationagent.conf import AgentConfig
from dotenv import dotenv_values
import os


class DoEConfig(AgentConfig):
    REGISTER_AGENT: bool


def config_doe(env_file: str = None) -> DoEConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return DoEConfig(dotenv_values(env_file))
    else:
        return DoEConfig(os.environ)
