import glob
import os
from typing import List, Tuple
import pyuploader.errorhandling.appexceptions as appexcept


def get_files_by_extensions(fileOrDir: str, extensions: str) -> List[str]:

    files = []
    if os.path.isdir(fileOrDir):
        extensions_list = [ext.strip() for ext in extensions.split(",")]
        for ext in extensions_list:
            # use glob to easily filter the files
            files.extend(glob.glob(os.path.join(fileOrDir, "*." + ext)))
    elif os.path.isfile(fileOrDir):
        # if it is a file, then do not check for its extension here
        files.append(fileOrDir)
    return files


def get_credentials_from_str(auth_str: str) -> Tuple[str, str]:

    auth_parts = auth_str.split(":")
    if len(auth_parts) > 2:
        raise ValueError("Wrong authorisation string format.")

    return (auth_parts[0], auth_parts[1])


def get_credentials_from_file(auth_file_path: str) -> Tuple[str, str]:

    auth_str = read_file_content(auth_file_path)

    return get_credentials_from_str(auth_str=auth_str)


def read_file_content(file_path: str) -> str:
    with open(file_path) as f:
        content = f.read()
    return content


def get_env_var_value(env_var_name: str) -> str:
    try:
        env_var_value = os.environ[env_var_name]
    except KeyError as ex:
        raise appexcept.EnvironmentVarError(
            f"Error: {env_var_name} not found in the environment variables."
        ) from ex
    return env_var_value
