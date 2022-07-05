from pyderivationagent.conf import AgentConfig
from dotenv import dotenv_values
import os


class AgilentConfig(AgentConfig):
    HPLC_DIGITAL_TWIN: str
    HPLC_REPORT_PERIODIC_TIMESCALE: int
    HPLC_REPORT_CONTAINER_DIR: str
    CURRENT_HPLC_METHOD: str
    HPLC_REPORT_FILE_EXTENSION: str
    REGISTER_AGENT: bool


def config_agilent(env_file: str = None) -> AgilentConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return AgilentConfig(dotenv_values(env_file))
    else:
        return AgilentConfig(os.environ)
