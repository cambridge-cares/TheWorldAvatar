import glob
import os
import re
import hashlib

def get_files_by_extensions(fileOrDir, extensions):
    files= []

    extensions = [ext.strip() for ext in extensions.split(',')]
    if os.path.isdir(fileOrDir):
        for ext in extensions:
            # use glob to easily filter the files
            files.extend(glob.glob(os.path.join(fileOrDir, '*'+ext)))
    elif os.path.isfile(fileOrDir):
        # if it is a file, then do not check for its extension
        files.append(fileOrDir)
    return files
