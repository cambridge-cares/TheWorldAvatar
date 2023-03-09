#this script accesses a webpage with the weather forecast for Singapore and stores the current weather conditions

from lxml import html
import requests, os, csv, sys, re, random
from datetime import datetime

#dictionary function which appends a .csv file given its address, an array with values to be saved and an array with headers under which it is supposed to write the values (to be improved)
def ACSV(address, output, headers):
    does_exist = os.path.isfile(address)
    with open(address, 'a') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=headers, lineterminator = '\n')
            if (does_exist==False): writer.writeheader()
            writer.writerow({headers[x]: output[x] for x in range(len(headers))})

			
#def run(weather_data = r"weather_data.csv",meteo_data = r"test.met"):
def run(weather_data = r"weather_data.csv",meteo_data = "C://JPS_DATA/workingdir/JPS/ADMS/test.met"):
	# working_dir = os.getcwd()
	# move to working directory
	# os.chdir(working_dir)
	#address of the webpage; I believe this should work for any "current weather" page on the accuweather webapge
	try:
		print ('start getting weather')
		url = 'http://www.accuweather.com/en/sg/singapore/300597/current-weather/300597'

		#this is the address of a file in which the weather data is to be stored; note that the data will be appened to the file rather than overwriting it
		# weather_data = r"weather_data.csv"
		# meteo_data = r"test.met"
		# apl_data = r"test.APL"
		
		#this line downloads source code from the url; note that it assumes it does not require any log in procedure
		page = requests.get(url,timeout = 500, headers = {'user-agent': 'my-app/0.0.1'})
		print ('got page')

		#this line parses the html code so that it can be read as an XML file making searches on it more convenient
		tree = html.fromstring(page.content.decode('utf-8', errors='replace'))

		#the array weather_conditions stores headers in the first subarray and corresponding data in the second subarray
		weather_conditions = [[],[]]

		#temperature
		weather_conditions[0].append('Temperature (C)')
		weather_conditions[1].append(tree.xpath('//div[@id="detail-now"]//span[@class="large-temp"]/text()')[0][:-1])

		# Wind direction
		wind = {'CLM':0.0, 'N':0.0, 'NNE':22.5,'NE':45.0, 'ENE':67.5,'E':90.0,'ESE':112.5,'SE':135.0,'SSE':157.5,'S':180.0,'SSW':202.5,'SW':225.0,'WSW':247.5,'W':270.0,'WNW':292.5,'NW':315.0,'NNW':337.5}
		# wind = {'ESE': 292.5, 'NNE': 202.5, 'W': 90.0, 'SSE': 337.5, 'SSW': 22.5, 'SE': 315.0, 'N': 180.0, 'ENE': 247.5, 'WSW': 67.5, 'S': 0.0, 'CLM': 0.0, 'NE': 225.0, 'NW': 135.0, 'SW': 45.0, 'E': 270.0, 'WNW': 112.5, 'NNW': 157.5}
		weather_conditions[0].append('Wind direction (deg)')
		weather_conditions[1].append(wind[tree.xpath("//div[re:test(@class, 'wind-point [A-Z]+')]", namespaces={'re': "http://exslt.org/regular-expressions"})[0].attrib['class'][11:]])
		#weather_conditions[1].append(.0)
		
		#downloading Wind speed, Humidity, Pressure, UV Index, Cloud Cover, Cloud Ceiling, Dew Point, Visibility 
		tree.xpath('//ul/li/strong/text()')
		c = tree.xpath('//ul/li/strong')
		for i in range(len(c)):

			if i == 0: weather_conditions[0].append(c[i].getparent().attrib['class'])
			else: weather_conditions[0].append(c[i].getparent().text)
			weather_conditions[1].append(c[i].text)

		#this loop changes the headers and reformat the data
		for i in range(len(weather_conditions[0])):
			if weather_conditions[0][i] == 'wind':
				weather_conditions[0][i] = 'Wind speed (m/s)'
				if float(weather_conditions[1][i][:-4])/3.6 < 0.77: weather_conditions[1][i] = 0.77
				else: weather_conditions[1][i] = round(float(weather_conditions[1][i][:-4])/3.6,5)
			if weather_conditions[0][i] == 'Humidity: ':
				weather_conditions[0][i] = 'Humidity (%)'
				weather_conditions[1][i] = float(weather_conditions[1][i][:-1])
			if weather_conditions[0][i] == 'Pressure: ':
				weather_conditions[0][i] = 'Pressure (hPa)'
				weather_conditions[1][i] = float(weather_conditions[1][i][:-4])
			if weather_conditions[0][i] == 'UV Index: ': weather_conditions[0][i] = 'UV Index'		
			if weather_conditions[0][i] == 'Cloud Cover: ':
				weather_conditions[0][i] = 'Cloud Cover (%)'
				weather_conditions[1][i] = float(weather_conditions[1][i][:-1])			
			if weather_conditions[0][i] == 'Ceiling: ':
				weather_conditions[0][i] = 'Ceiling (m)'
				weather_conditions[1][i] = float(weather_conditions[1][i][:-1])		
			if weather_conditions[0][i] == 'Dew Point: ':
				weather_conditions[0][i] = 'Dew Point (C)'
				weather_conditions[1][i] = float(weather_conditions[1][i][:-3])	
			if weather_conditions[0][i] == 'Visibility: ':
				weather_conditions[0][i] = 'Visibility (m)'
				weather_conditions[1][i] = float(weather_conditions[1][i][:-2])*1000	
				
		#this line saves the data by appending it to the file under the given address (it will create a new file if none exists)
		ACSV(weather_data, [page.headers['Date'][5:]]+weather_conditions[1], ['Date']+weather_conditions[0])

		# The following code creates the meteorology file for ADMS
		headers = ['Date']+weather_conditions[0]
		data = [page.headers['Date'][5:]]+weather_conditions[1]
		storage = ['VARIABLES:', '9', 'STATION DCNN', 'YEAR', 'TDAY', 'THOUR', 'T0C', 'U', 'PHI', 'P', 'CL', 'DATA:']
		date_ref = datetime.strptime('1 Jan 2018 00:00:00 GMT', '%d %b %Y %H:%M:%S %Z')

		# This code puts data into an appropriate format
		temp ='4238.0, '
		condition = len(headers)
		space = ', '
		for i in range(len(data)):
			if i%condition == 0:
				datetime_object = datetime.strptime(data[0], '%d %b %Y %H:%M:%S %Z')
				temp += str(datetime_object.year)+space
				temp += str(float(datetime_object.toordinal() - date_ref.toordinal()+1))+space
				temp += str(float(datetime_object.hour))+space
			if i%condition == 1: temp += str(data[i])+space
			if i%condition == 2: temp2 = str(data[i])+space # wind direction
			if i%condition == 3: temp += str(round(data[i],1))+space+temp2 #wind speed
			if i%condition == 4: temp += str(0.0)+space
			if i%condition == 7: temp += str(round(data[i]/100*8,0))
			if i%condition == 10:
				storage.append(temp)
				temp ='4238.0, '
			
		# This code writes the file
		f = open(meteo_data, 'w')
		for line in storage:
			f.write(line+'\n')

		f.close()


		print('Success')

	except:
		print("Unexpected error:", sys.exc_info()[0])
		raise	


if __name__ == "__main__":
	run()
	# run(r"C:\Users\mjk_2\Desktop\ADMS_OWL")