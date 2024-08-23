from pyderivationagent.conf import AgentConfig
from pyderivationagent.conf import config_generic


class HPLCConfig(AgentConfig):
    HPLC_DIGITAL_TWIN: str
    HPLC_REPORT_PERIODIC_TIMESCALE: int
    HPLC_REPORT_CONTAINER_DIR: str
    CURRENT_HPLC_METHOD: str
    HPLC_REPORT_FILE_EXTENSION: str
    DRY_RUN: bool = True


def config_hplc_agent(env_file: str = None) -> HPLCConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(HPLCConfig, env_file)
