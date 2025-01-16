from typing import get_type_hints, Type, Union, Any
from collections import ChainMap
from collections.abc import Mapping
from dotenv import dotenv_values
import os

from twa.data_model.iris import TWA_BASE_URL


# modified based on https://blog.doppler.com/environment-variables-in-python


class AppConfigError(Exception):
    pass


def _parse_bool(val: Union[str, bool]) -> bool:  # pylint: disable=E1136
    if isinstance(val, bool):
        return val
    if val.lower() in ['true', 'yes', '1']:
        return True
    elif val.lower() in ['false', 'no', '0']:
        return False
    else:
        raise ValueError(f"""Invalid boolean value received: {val}.
            Valid options (upper letters accepted): {{'true', 'yes', '1'}} for True; {{'false', 'no', '0'}} for False.""")

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
    def __init__(self, env: Mapping[str, Any]):
        """
        The constructor for the Config class.

        Args:
            env (Mapping[str, Any]): A mapping of environment variables.

        Raises:
            AppConfigError: Required fields are not provided
            AppConfigError: Provided value for field does not match with the specified data type
        """
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


def config_generic(conf_cls: Type[Config], env_file: str = None) -> Config:
    """
    Load and return the configuration for the specified configuration class from either environment variables or a specified environment file.

    Args:
        conf_cls (Type[Config]): The configuration class to instantiate.
        env_file (str, optional): The path to a dotenv (.env) file containing environment variables. If not provided, environment variables will be loaded from the system environment.

    Returns:
        Config: An instance of the provided configuration class, populated with environment variables.

    Raises:
        AppConfigError: If required configuration fields are missing or cannot be cast to the specified types.

    Examples:
        ```
        # Load configuration from a .env file
        config = config_generic(MyConfigClass, env_file=".env")

        # Load configuration from system environment variables
        config = config_generic(MyConfigClass)
        ```

    Note:
        The function prioritizes loading environment variables from the provided env_file if specified.
        Otherwise, it defaults to using the system environment variables.
    """
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
    Configuration class for the DerivationAgent.
    This class is a subclass of Config and provides custom configurations for developed agents.
    It includes various fields to configure the agent's behavior and connectivity.

    Attributes:
        DERIVATION_PERIODIC_TIMESCALE (int): The time scale of the periodic job that monitors asynchronous derivations.
        DERIVATION_INSTANCE_BASE_URL (str): The base URL of the derivation instances that to be created by this agent.
        SPARQL_QUERY_ENDPOINT (str): The SPARQL endpoint to be used for querying the knowledge graph.
        SPARQL_UPDATE_ENDPOINT (str): The SPARQL endpoint to be used for updating the knowledge graph.
        KG_USERNAME (str): The username to access the SPARQL endpoint.
        KG_PASSWORD (str): The password to access the SPARQL endpoint.
        FILE_SERVER_ENDPOINT (str): The endpoint of the file server.
        FILE_SERVER_USERNAME (str): The username to access the file server.
        FILE_SERVER_PASSWORD (str): The password to access the file server.
        ONTOAGENT_OPERATION_HTTP_BASE_URL (str): The URL of the OntoAgent:Operation HTTP endpoint.
        REGISTER_AGENT (bool): Whether to register the OntoAgent instance of the configured agent to knowledge graph.
        MAX_THREAD_MONITOR_ASYNC_DERIVATIONS (int): The maximum number of thread can be invoked to monitor async derivations at the same time, the default value is 1.
        EMAIL_RECIPIENT (str): The list of recipients of email notifications during agent operation, multiple email address should be seperated by semicolon, e.g. foo.1@bar.com;foo.2@bar.com.
        EMAIL_SUBJECT_PREFIX (str): The subject prefix for email notifications, "[] " is automatically added, e.g. the prefix will be "[YourAgent] " if "YourAgent" is specified.
        EMAIL_USERNAME (str): The username of the email sender, note that a gmail account is required.
        EMAIL_AUTH_JSON_PATH (str): The json file path to the OAuth2 file of the gmail account defined by EMAIL_USERNAME.
        EMAIL_START_END_ASYNC_DERIVATIONS (bool): The boolean flag to choose whether to send email notification at the start and end of process an async derivation, the default value is False.
    """
    DERIVATION_PERIODIC_TIMESCALE: int
    DERIVATION_INSTANCE_BASE_URL: str = TWA_BASE_URL
    SPARQL_QUERY_ENDPOINT: str
    SPARQL_UPDATE_ENDPOINT: str = ''
    KG_USERNAME: str = ''
    KG_PASSWORD: str = ''
    FILE_SERVER_ENDPOINT: str = ''
    FILE_SERVER_USERNAME: str = ''
    FILE_SERVER_PASSWORD: str = ''
    ONTOAGENT_OPERATION_HTTP_BASE_URL: str = 'http://localhost:5000/'
    REGISTER_AGENT: bool = True
    MAX_THREAD_MONITOR_ASYNC_DERIVATIONS: int = 1
    EMAIL_RECIPIENT: str = ''
    EMAIL_SUBJECT_PREFIX: str = ''
    EMAIL_USERNAME: str = ''
    EMAIL_AUTH_JSON_PATH: str = ''
    EMAIL_START_END_ASYNC_DERIVATIONS: bool = False


def config_derivation_agent(env_file: str = None) -> AgentConfig:
    """
    Load and return the configuration for the DerivationAgent from either environment variables or a specified environment file.

    Args:
        env_file (str, optional): The path to a dotenv file containing environment variables. If not provided, environment variables will be loaded from the system environment.

    Returns:
        AgentConfig: An instance of the AgentConfig class, populated with environment variables.

    Raises:
        AppConfigError: If required configuration fields are missing or cannot be cast to the specified types.

    Examples:
        ```
        # Load configuration from a .env file
        config = config_derivation_agent(env_file=".env")

        # Load configuration from system environment variables
        config = config_derivation_agent()
        ```

    Note:
        The function prioritizes loading environment variables from the provided env_file if specified.
        Otherwise, it defaults to using the system environment variables.
    """
    return config_generic(AgentConfig, env_file)
