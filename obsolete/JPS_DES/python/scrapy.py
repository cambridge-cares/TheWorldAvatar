# No longer used. Now everything is in Forecast Agent
import requests
import csv
import json
from datetime import datetime
try:
	URL = "https://api.solcast.com.au/weather_sites/0ff4-0cb4-c270-5389/forecasts?format=json&api_key=IxJaiBo4-jICEIZSFPuRYVvJ2OqiFBqN"
	r = requests.get(url = URL)
	data = r.json()
	URL2 = "http://dataservice.accuweather.com/forecasts/v1/hourly/12hour/300565?apikey=%20%09NP6DUl1mQkBlOAn7CE5j3MGPAAR9xbpg&details=true&metric=true"
	r = requests.get(url = URL2)
	data2 = r.json()
	lst = []
	now = datetime.now()
	current_hour = now.hour
	current_year = now.year
	for i in range(0,48, 2):
		currentweather = 9*[0]
		currentweather[0] = current_year
		#check the current hour. if current_hour + str(i/2) > 24 reset date
		if (current_hour+i/2>=24):
			currentweather[2] = '{:02d}'.format(int(current_hour + i/2 - 24))  + ":00:00"
			currentweather[1] = '{:02d}-{:02d}'.format(now.month, now.day + 1)
		else:
			currentweather[2] = '{:02d}'.format(int(current_hour + i/2 ))  + ":00:00"
			currentweather[1]= '{:02d}-{:02d}'.format(now.month, now.day)
			
		currentweather[8] =data["forecasts"][i]["ghi"]
		currentweather[4] =data["forecasts"][i]["air_temp"]
		lst.append(currentweather)
	for i in range(12):
		lst[i][6] = data2[i]["Wind"]["Speed"]["Value"]
		
	with open("WeatherForecast.json", "w") as f:
		json0 = {}
		json0["result"] = lst
		json.dump(json0, f)
	with open("WeatherForecast.csv", "w+", newline='') as weather:
		csvWriter = csv.writer(weather, delimiter = ",")
		csvWriter.writerows(lst)
except Exception as ex: 
	print("An error has occurred ")
	with open ('error log.txt', 'w') as outfile:
		outfile.write('error occurred\n')
		outfile.write(str(ex)+'\n')
		outfile.write('There is only 10 calls per day for the solar radiation api. If you call more than that, then forecast would not be created.\n')
