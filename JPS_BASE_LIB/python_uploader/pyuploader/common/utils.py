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
        for ext in extensions:
            # use regex to check if a given file has the required extension
            if re.search(r""+ext.replace('.','')+'$',fileOrDir) is not None:
                files.append(fileOrDir)
                break
    else:
        #raise invalid input exception
        pass
    return files

def hash_file(file):
    BUF_SIZE = 65536
    h = hashlib.new('sha1')
    with open (file, 'rb') as f:
        while True:
            data = f.read(BUF_SIZE)
            if not data:
                break
            h.update(data)
    return h.hexdigest()
