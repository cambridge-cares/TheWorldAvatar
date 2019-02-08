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
	weatherData = json.loads(sys.argv[2].replace('$','"'))
	fullPath = sys.argv[1]
	
	cloudCover = weatherData['hascloudcover']['hascloudcovervalue']
	cloudCover = (float(cloudCover) / 100) * 8
	if weatherData['haswind']['hasdirection'] == '':
		windDirection = 180
	else:
		#windDirection = 180 + float(weatherData['haswind']['hasdirection']) (why like this?)
		windDirection =  float(weatherData['haswind']['hasdirection'])
	if windDirection > 360:
		windDirection = windDirection % 360
	windSpeed = weatherData['haswind']['hasspeed']
	precitipation = weatherData['hasprecipation']['hasintensity']
	temperature = weatherData['hasexteriortemperature']['hasvalue']
	
except:
	print("ERROR: Invalid Input")

try:
	metpath = "C://JPS_DATA/workingdir/JPS/ADMS/test.met"
	with open(metpath, 'w') as file:
		result =  template%(temperature,windSpeed,windDirection,precitipation,cloudCover)
		file.write(result)
		file.close()
	print("SUCCESS: MET File is Created")
	
except:
	print("ERROR: File Not Written")