from pyderivationagent.conf import AgentConfig
from dotenv import dotenv_values
import os


class VapourtecExecutionConfig(AgentConfig):
    MAXIMUM_CONCURRENT_EXPERIMENT: int
    REGISTER_AGENT: bool


def config_vapourtec_execution(env_file: str = None) -> VapourtecExecutionConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return VapourtecExecutionConfig(dotenv_values(env_file))
    else:
        return VapourtecExecutionConfig(os.environ)
