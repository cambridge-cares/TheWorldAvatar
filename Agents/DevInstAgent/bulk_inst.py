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
    
    result= {}

    for filename in os.listdir(folder):
        status = ""
        if filename.endswith(".json"):
            try:
                with open(filename) as f:
                    req = requests.post(url, data = json.load(filename))
                    status = req.text
            except Exception as error:
                status = error

        result[filename] = status
                

    print("Command success!")
    print("Instantiation script result:")
    for key in result.keys():
        print(key, ":", result[key])

