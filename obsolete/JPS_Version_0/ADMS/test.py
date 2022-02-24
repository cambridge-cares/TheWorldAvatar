import requests
url = 'http://www.accuweather.com/en/sg/singapore/300597/current-weather/300597?day=1'
page = requests.get(url)
print(page.content.decode('utf-8', errors='replace'))
