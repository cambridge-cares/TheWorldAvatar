from tests.conftest import config_generic
from tests.conftest import Config4Test1
from tests.conftest import Config4Test2

from tests.conftest import config_derivation_agent
from tests.conftest import AgentConfig
from tests.conftest import RNGAGENT_ENV
from tests.conftest import MAXAGENT_ENV
from tests.conftest import MINAGENT_ENV
from tests.conftest import DIFFAGENT_ENV
from tests.conftest import DIFFREVERSEAGENT_ENV
from tests.conftest import UPDATEENDPOINT_ENV

from typing import get_type_hints
from dotenv import dotenv_values
import pytest


@pytest.mark.parametrize(
    "target_conf_cls",
    [ # different config classes for testing simple configs, and configs with inheritance
        (Config4Test1),
        (Config4Test2),
        (AgentConfig),
    ],
)
def test_config_generic(generate_random_env_file, target_conf_cls):
    # Create temporary env file with random generated values
    env_file_path, conf_test_dct = generate_random_env_file(target_conf_cls)

    # Create config instance from temporary env file
    config = config_generic(target_conf_cls, env_file_path)

    # Check if config instance is populated correctly
    # i.e. the values should be the same as in the env file
    for key, value in config.__dict__.items():
        assert value == conf_test_dct[key]


@pytest.mark.parametrize(
    "env_file",
    [ # test all provided env files for sample agents
        (RNGAGENT_ENV),
        (MAXAGENT_ENV),
        (MINAGENT_ENV),
        (DIFFAGENT_ENV),
        (DIFFREVERSEAGENT_ENV),
        (UPDATEENDPOINT_ENV),
    ],
)
def test_config_derivation_agent(env_file):
    # Read variables from env file
    env = dotenv_values(env_file)

    # Create config instance from env file
    config = config_derivation_agent(env_file)

    # Check if config instance is populated correctly
    # i.e. the values should be the same as in the env file
    for key, value in config.__dict__.items():
        var_type = get_type_hints(AgentConfig)[key]
        default_value = getattr(AgentConfig, key, None)
        if var_type == bool:
            env_value = env.get(key) if type(env.get(key)) == bool else env.get(key).lower() in ['true', 'yes', '1']
        else:
            env_value = var_type(env.get(key, default_value)) # assign default value if env var not found
        assert value == env_value
