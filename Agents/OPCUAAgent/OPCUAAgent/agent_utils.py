import os
from jproperties import Properties
import re

def get_env_variable(var_name):
    """
    Get the value of the environment variable
    """
    try:
        # Get the value of the environment variable
        value = os.environ[var_name]
        return value
    except KeyError:
        # Handle the case where the environment variable is not set
        raise KeyError(f"Environment variable '{var_name}' not found.")

def read_property(file_path, key):
    """
    Get the value of a key in the provided properties file
    """
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
    
def remove_char_between_characters(text, start_char, end_char):
    """
    Remove characters between start_char and end_char, returned back the cleaned string
    """
    # Define the regular expression pattern to match text between the specified characters
    pattern = re.escape(start_char) + r'.*?' + re.escape(end_char)
    
    # Use re.sub() to replace the matched pattern with an empty string
    cleaned_text = re.sub(pattern, '', text)
    
    return cleaned_text.strip()