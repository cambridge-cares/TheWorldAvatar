import requests
import urllib.parse

# Input parameter

BASE_URL = "" #"http://localhost:58085/request"
QUERY_FILE = "" #"./examples/HDMR.json"

if len(BASE_URL)>0 and len(QUERY_FILE)>0:

    # read JSON file

    with open(QUERY_FILE,'r') as file:
        data = file.read()
    data = data.replace('\r','').replace('\n','').replace('\t','').replace(' ','')
    QUERY_URL=f"{BASE_URL}?query={urllib.parse.quote(data)}"

    try:
        response = requests.get(QUERY_URL)
        print(f"Response status code: {response.status_code}")
        print(f"Response content: {response.text}")
    except:
        print("Something went wrong.")

else:

    print("Please specify BASE_URL and QUERY_FILE.")