from typing import get_type_hints, Union
from collections import ChainMap
from dotenv import dotenv_values
import os

# modified based on https://blog.doppler.com/environment-variables-in-python


class AppConfigError(Exception):
    pass


def _parse_bool(val: Union[str, bool]) -> bool:  # pylint: disable=E1136
    return val if type(val) == bool else val.lower() in ['true', 'yes', '1']

#################################
## Config that can be extended ##
#################################
class Config:
    """
    This is a generic config class that can be extended by other classes.
    Map environment variables to class fields according to these rules:
      - Field won't be parsed unless it has a type annotation
      - Field will be skipped if not in all caps
      - Class field and environment variable name are the same
    """
    def __init__(self, env):
        for field in self.all_annotations(): # this ensures the annotations of all parent classes are included
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

    # modified based on https://stackoverflow.com/a/72037059
    @classmethod
    def all_annotations(cls) -> ChainMap:
        """Returns a dictionary-like ChainMap that includes annotations for all 
        attributes defined in cls or inherited from superclasses."""
        return ChainMap(*(c.__annotations__ for c in cls.__mro__ if '__annotations__' in c.__dict__) )


def config_generic(conf_cls: Config, env_file: str = None) -> Config:
    """Return configurations for target conf_cls from either environment variables or env_file."""
    if env_file is not None:
        return conf_cls(dotenv_values(env_file))
    else:
        return conf_cls(os.environ)


#####################################
## AgentConfig for DerivationAgent ##
#####################################
# AgentConfig class with required fields, default values, type checking, and typecasting for int and bool values
class AgentConfig(Config):
    """
    This is a config class for the DerivationAgent.
    It is a subclass of Config and can be extended to provide custom configurations for developed agents.
    It has the following fields:
      - ONTOAGENT_SERVICE_IRI: The IRI of the OntoAgent:Service for the configured agent.
      - DERIVATION_PERIODIC_TIMESCALE: The time scale of the periodic job that monitors asynchronous derivations.
      - DERIVATION_INSTANCE_BASE_URL: The base URL of the derivation instances that to be created by this agent.
      - SPARQL_QUERY_ENDPOINT: The SPARQL endpoint to be used for querying the knowledge graph.
      - SPARQL_UPDATE_ENDPOINT: The SPARQL endpoint to be used for updating the knowledge graph.
      - KG_USERNAME: The username to access the SPARQL endpoint.
      - KG_PASSWORD: The password to access the SPARQL endpoint.
      - FILE_SERVER_ENDPOINT: The endpoint of the file server.
      - FILE_SERVER_USERNAME: The username to access the file server.
      - FILE_SERVER_PASSWORD: The password to access the file server.
      - ONTOAGENT_OPERATION_HTTP_URL: The URL of the OntoAgent:Operation HTTP endpoint.
      - REGISTER_AGENT: Whether to register the OntoAgent instance of the configured agent to knowledge graph.
    """
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
    REGISTER_AGENT: bool


def config_derivation_agent(env_file: str = None) -> AgentConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(AgentConfig, env_file)
