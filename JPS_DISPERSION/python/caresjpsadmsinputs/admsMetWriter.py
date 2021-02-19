#print("Here we go", sys.argv[0], sys.argv[1], sys.argv[2])
import sys
import json
import datetime

now=datetime.datetime.now()


template = '''VARIABLES:
10
STATION DCNN
YEAR
TDAY
THOUR
T0C
U
PHI
P
CL
RHUM
DATA:
4238.0, %s, %s, %s, %s, %s, %s, %s, %s, %s
'''

hournow= now.hour
yearnow="%d" % now.year
daynow="%d" % now.day

#print (hournow)
#print (yearnow)
#print (daynow)
fmt = '%Y.%m.%d'
s=str(yearnow)+'.'+str(now.month)+'.'+str(now.day)
dt = datetime.datetime.strptime(s, fmt)
tt=dt.timetuple()
#print(tt.tm_yday)



try:
	weatherData = json.loads(sys.argv[2].replace('$','"'))
	fullPath = sys.argv[1]
	
	cloudCover = weatherData['hascloudcover']['hascloudcovervalue']
	cloudCover = (float(cloudCover)) * 8
	if weatherData['haswind']['hasdirection'] == '':
		windDirection = 180
	else:
		#windDirection = 180 + float(weatherData['haswind']['hasdirection']) (why like this?)
		windDirection =  float(weatherData['haswind']['hasdirection'])
	if windDirection > 360:
		windDirection = windDirection % 360
	windSpeed = weatherData['haswind']['hasspeed']
	if float(windSpeed) < 0.75:
		windSpeed= 0.75
	precitipation = weatherData['hasprecipation']['hasintensity']
	temperature = weatherData['hasexteriortemperature']['hasvalue']
	relativehumidity=weatherData['hashumidity']['hasvalue']
	
except:
	print("ERROR: Invalid Input")

try:
	metpath = str(fullPath)+"/test.met"
	with open(metpath, 'w') as file:
		result =  template%(yearnow,tt.tm_yday,hournow,temperature,windSpeed,windDirection,precitipation,cloudCover,relativehumidity)
		file.write(result)
		file.close()
	print("SUCCESS: MET File is Created")
	
except:
	print("ERROR: File Not Written")