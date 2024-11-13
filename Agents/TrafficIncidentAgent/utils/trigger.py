import requests

url = "http://localhost:1016/traffic-incident-agent/retrieve"

response = requests.request("POST", url, headers={}, data="")

print(response.text)
