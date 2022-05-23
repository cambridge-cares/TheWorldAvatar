from typing import get_type_hints, Union
from dotenv import dotenv_values
import os

# modified based on https://blog.doppler.com/environment-variables-in-python


class AppConfigError(Exception):
    pass


def _parse_bool(val: Union[str, bool]) -> bool:  # pylint: disable=E1136
    return val if type(val) == bool else val.lower() in ['true', 'yes', '1']


# AgentConfig class with required fields, default values, type checking, and typecasting for int and bool values
class AgentConfig:
    ONTOAGENT_SERVICE_IRI: str
    DERIVATION_PERIODIC_TIMESCALE: int
    DERIVATION_INSTANCE_BASE_URL: str
    SPARQL_QUERY_ENDPOINT: str
    SPARQL_UPDATE_ENDPOINT: str
    KG_USERNAME: str
    KG_PASSWORD: str
    FILE_SERVER_ENDPOINT: str
    FILE_SERVER_USERNAME: str
    FILE_SERVER_PASSWORD: str
    ONTOAGENT_OPERATION_HTTP_URL: str

    """
    Map environment variables to class fields according to these rules:
      - Field won't be parsed unless it has a type annotation
      - Field will be skipped if not in all caps
      - Class field and environment variable name are the same
    """
    def __init__(self, env):
        for field in self.__annotations__:
            if not field.isupper():
                continue

            # Raise AppConfigError if required field not supplied
            default_value = getattr(self, field, None)
            if default_value is None and env.get(field) is None:
                raise AppConfigError('The {} field is required'.format(field))

            # Cast env var value to expected type and raise AppConfigError on failure
            try:
                var_type = get_type_hints(self.__class__)[field]
                if var_type == bool:
                    value = _parse_bool(env.get(field, default_value))
                else:
                    value = var_type(env.get(field, default_value))

                self.__setattr__(field, value)
            except ValueError:
                raise AppConfigError('Unable to cast value of "{}" to type "{}" for "{}" field'.format(
                    env[field],
                    var_type,
                    field
                )
            )

    def __repr__(self):
        return str(self.__dict__)


def config_derivation_agent(env_file: str = None) -> AgentConfig:
    """Return configurations from either environment variables or env_file."""
    if env_file is not None:
        return AgentConfig(dotenv_values(env_file))
    else:
        return AgentConfig(os.environ)
