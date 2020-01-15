from PIL import Image
from pytesseract import *
import urllib.request
from datetime import datetime
import json
import time
import random

#read and download image

def ocr():
	url = 'https://www.solar-repository.sg/ftp_up/weather/500_Weather.png'
	response = urllib.request.urlretrieve(url, '500_image.png')
	#scan image provided. 
	im = Image.open('500_image.png')
	text = image_to_string(im)
	im.close()
	r = text.split('\n')
	result = {}
	for i in r:
		#print(i)
		if i.startswith('Ambient'):
			temp = i.split(' ')[2]
			if (temp == '|'):
				temp = i.split(' ')[3]
			if (len(temp.split('.')))>2:
				temp = temp[:-1]
			result['temperature'] = temp
		if i.startswith('Global'):
			irrad = i.split(' ')[2]
			if (irrad == '|'):
				irrad = i.split(' ')[3]
			if (len(irrad.split('.')))>2:
				irrad = irrad[:-1]
			result['irradiance'] = irrad
		if i.startswith('Wind Speed'):
			speed = i.split(' ')[2]
			if (speed == '|'):
				speed = i.split(' ')[3]
			result['windspeed'] = speed
	now = datetime.now() # current date and time
	if ('windspeed' not in result):
		result['windspeed'] = str(random.randint(0,13))
	elif (float(result['windspeed']) > 13):
		result['windspeed'] = str(float(result['windspeed'])/10)
	result['year']= now.strftime("%Y")
	result['month'] = now.strftime("%m")
	result['date']= now.strftime("%d")
	result['time'] =now.strftime("%H:%M:%S")
	with open ('data.json', 'w') as outfile:
		json.dump(result, outfile)
try:
	ocr()
except Exception as e:
	with open ('error log.txt', 'w') as outfile:
		outfile.write('error occurred\n')
		outfile.write(str(e))
		outfile.write('I said end of message')
