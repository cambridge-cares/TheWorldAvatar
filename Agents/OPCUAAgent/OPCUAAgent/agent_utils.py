import os
from jproperties import Properties

def get_env_variable(var_name):
    try:
        # Get the value of the environment variable
        value = os.environ[var_name]
        return value
    except KeyError:
        # Handle the case where the environment variable is not set
        raise KeyError(f"Environment variable '{var_name}' not found.")

def read_property(file_path, key):
    # Create an instance of the Properties class
    properties = Properties()

    # Load the properties file
    with open(file_path, 'rb') as file:
        properties.load(file)
        
    try:
        # Retrieve the value of the key
        value = properties.get(key).data
        # Return the value if the key exists, else return an error message
        return value
    except (KeyError, AttributeError) as error:
        # Handle the case where the environment variable is not set
        raise KeyError(f"Key '{key}' not found.")