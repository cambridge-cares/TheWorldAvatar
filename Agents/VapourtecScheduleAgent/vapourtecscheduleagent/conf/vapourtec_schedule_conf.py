from pyderivationagent.conf import AgentConfig
from pyderivationagent.conf import config_generic


class VapourtecScheduleConfig(AgentConfig):
    MAXIMUM_CONCURRENT_EXPERIMENT: int


def config_vapourtec_schedule_agent(env_file: str = None) -> VapourtecScheduleConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(VapourtecScheduleConfig, env_file)
