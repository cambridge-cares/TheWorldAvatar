from pyderivationagent.conf import AgentConfig
from dotenv import dotenv_values
import os


class AgilentPostProcConfig(AgentConfig):
    REGISTER_AGENT: bool


def config_agilent_postproc(env_file: str = None) -> AgilentPostProcConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return AgilentPostProcConfig(dotenv_values(env_file))
    else:
        return AgilentPostProcConfig(os.environ)
