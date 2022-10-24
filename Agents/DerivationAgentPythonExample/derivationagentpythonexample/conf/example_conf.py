from pyderivationagent.conf import AgentConfig
from pyderivationagent.conf import config_generic


class ExampleConfig(AgentConfig):
    """Example configuration class for the derivation agent. You can add your own config parameters (all caps) here.
    """
    EXAMPLE_CONF_PARAM: str


def config_example_agent(env_file: str = None) -> ExampleConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(ExampleConfig, env_file)
