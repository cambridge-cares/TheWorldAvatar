#print("Here we go", sys.argv[0], sys.argv[1], sys.argv[2])
import sys
import json


template = '''VARIABLES:
9
STATION DCNN
YEAR
TDAY
THOUR
T0C
U
PHI
P
CL
DATA:
4238.0, 2018, 235.0, 8.0, %s, %s, %s, %s, %s
'''



try:
	weatherData = json.loads(sys.argv[2])
	fullPath = sys.argv[1]
	
	
	cloudCover = weatherData['cloudCover']
	windDirection = weatherData['windDirection']
	windSpeed = weatherData['windSpeed']
	precitipation = weatherData['precitipation']
	temperature = weatherData['temperature']
	
except:
	print("ERROR: Invalid Input")

try:
	with open(fullPath + '/test.met', 'w') as file:
		result =  template%(temperature,windSpeed,windDirection,precitipation,cloudCover)
		file.write(result)
		file.close()
	print("SUCCESS: MET File is Created")
	
except:
	print("ERROR: File Not Written")