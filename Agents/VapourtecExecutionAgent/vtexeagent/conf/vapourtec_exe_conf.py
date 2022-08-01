from pyderivationagent.conf import AgentConfig
from pyderivationagent.conf import config_generic


class VapourtecExecutionConfig(AgentConfig):
    MAXIMUM_CONCURRENT_EXPERIMENT: int


def config_vapourtec_execution_agent(env_file: str = None) -> VapourtecExecutionConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(VapourtecExecutionConfig, env_file)
