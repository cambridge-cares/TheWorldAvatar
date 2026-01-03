import requests
import urllib.parse
import time
import json
import io

# Input parameter

BASE_URL = "" #"http://localhost:58085/"
QUERY_FILE = "" #"./examples/HDMR.json"

if len(BASE_URL)>0 and len(QUERY_FILE)>0:

    # read JSON file

    with open(QUERY_FILE,'r') as file:
        data = file.read()
    data = data.replace('\r','').replace('\n','').replace('\t','')
    QUERY_URL=f"{BASE_URL}request?query={urllib.parse.quote(data)}"

    try:
        if len(QUERY_URL)<1000:
            response = requests.get(QUERY_URL)
        else:
            my_file = io.BytesIO(json.dumps(json.loads(data),indent=2).encode())
            response = requests.post(f"{BASE_URL}filerequest",files={"file":my_file})
        print(f"Response status code: {response.status_code}")
        print(f"Response content: {response.text}")
        time.sleep(10)
        # try to get result
        RESULT_URL=f"{BASE_URL}output/request?query={urllib.parse.quote(response.text)}"
        resp = requests.get(RESULT_URL)
        print(f"Response status code: {resp.status_code}")
        print(f"Response content: {resp.text}")
    except:
        print("Something went wrong.")

else:

    print("Please specify BASE_URL and QUERY_FILE.")