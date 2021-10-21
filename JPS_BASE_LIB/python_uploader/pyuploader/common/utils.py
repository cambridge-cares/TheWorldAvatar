import glob
import os
from typing import List, Tuple

def get_files_by_extensions(
        fileOrDir: str,
        extensions: str) -> List[str]:

    files: List[str,]= []
    extensions_list = [ext.strip() for ext in extensions.split(',')]
    if os.path.isdir(fileOrDir):
        for ext in extensions_list:
            # use glob to easily filter the files
            files.extend(glob.glob(os.path.join(fileOrDir, '*.'+ext)))
    elif os.path.isfile(fileOrDir):
        # if it is a file, then do not check for its extension
        files.append(fileOrDir)
    return files

def get_credentials_from_str(
    auth_str: str) -> Tuple[str,str]:

    auth_parts = auth_str.split(':')
    if len(auth_parts)>2:
        raise ValueError('Wrong authorisation string format.')
    return (auth_parts[0], auth_parts[1])