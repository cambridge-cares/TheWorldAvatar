import os
from jproperties import Properties

def get_env_variable(var_name):
    try:
        # Get the value of the environment variable
        value = os.environ[var_name]
        return value
    except KeyError:
        # Handle the case where the environment variable is not set
        return f"Environment variable '{var_name}' not found."

def read_property(file_path, key):
    # Create an instance of the Properties class
    properties = Properties()

    # Load the properties file
    with open(file_path, 'rb') as file:
        properties.load(file)
        
    # Check if the key exists
    if key in properties:
        # Retrieve the value of the key
        value = properties.get(key).data
        # Return the value if the key exists, else return an error message
    return value