#print("Here we go", sys.argv[0], sys.argv[1], sys.argv[2])
import sys
import json
import datetime

now=datetime.datetime.now()

template = '''BackgroundVersion2
9
CO2
NOx
NO2
O3
SO2
PM10
PM2.5
CO
Particulate001

UNITS:
ppb
ppb
ug/m3
ug/m3
ppb
ug/m3
ug/m3
ppb
ppb

DATA:
%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s
'''

hournow= now.hour+1
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
 	#bgdData = json.loads(sys.argv[2].replace('$','"'))
	fullPath = sys.argv[1]
	co2conc=414000
	noxconc=60
	no2conc=30
	o3conc=10
	so2conc=15.13
	pm10conc=56.3
	pm25conc=8
	coconc=1222
	particulateconc=64.3
# 	
# 	cloudCover = bgdData['hascloudcover']['hascloudcovervalue']
# 	cloudCover = (float(cloudCover) / 100) * 8
# 	if bgdData['haswind']['hasdirection'] == '':
# 		windDirection = 180
# 	else:
# 		windDirection =  float(bgdData['haswind']['hasdirection'])
# 	if windDirection > 360:
# 		windDirection = windDirection % 360
# 	windSpeed = bgdData['haswind']['hasspeed']
# 	precitipation = bgdData['hasprecipation']['hasintensity']
# 	temperature = bgdData['hasexteriortemperature']['hasvalue']
# 	relativehumidity=bgdData['hashumidity']['hasvalue']

	
except:
	print("ERROR: Invalid Input")

try:
	metpath = str(fullPath)+"/testbackgrnd.bgd"
	with open(metpath, 'w') as file:
		result =  template%(yearnow,tt.tm_yday,hournow,co2conc,noxconc,no2conc,o3conc,so2conc,pm10conc,pm25conc,coconc,particulateconc)
		file.write(result)
		file.close()
	print("SUCCESS: BGD File is Created")
	
except:
	print("ERROR: File Not Written")