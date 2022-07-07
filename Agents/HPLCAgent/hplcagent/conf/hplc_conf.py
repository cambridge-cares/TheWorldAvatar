from pyderivationagent.conf import AgentConfig
from dotenv import dotenv_values
import os


class HPLCConfig(AgentConfig):
    HPLC_DIGITAL_TWIN: str
    HPLC_REPORT_PERIODIC_TIMESCALE: int
    HPLC_REPORT_CONTAINER_DIR: str
    CURRENT_HPLC_METHOD: str
    HPLC_REPORT_FILE_EXTENSION: str
    REGISTER_AGENT: bool


def config_hplc(env_file: str = None) -> HPLCConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return HPLCConfig(dotenv_values(env_file))
    else:
        return HPLCConfig(os.environ)
