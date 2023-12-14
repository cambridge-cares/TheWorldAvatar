import requests
import os
import json
import sys

def get_args(name='default', url='', folder=""):
    return url, folder


if "__name__" == "__main__":
    url, folder = get_args(*sys.argv)
    if url == "" or folder == "":
        raise Exception("Please uinput the URL and path to folder in the parameter")

    for filename in os.listdir(folder):
        if filename.endswith(".json"):
            with open(filename) as f:
                req = requests.post(url, data = json.load(filename))

    print("Command success!")

