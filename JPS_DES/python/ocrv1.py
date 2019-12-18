from PIL import Image
from pytesseract import *
import urllib.request
from datetime import datetime
import json
import time

#read and download image

def ocr():
	url = 'https://www.solar-repository.sg/ftp_up/weather/500_Weather.png'
	response = urllib.request.urlretrieve(url, '500_image.png')
	#scan image provided. 
	im = Image.open('500_image.png')
	text = image_to_string(im)
	r = text.split('\n')
	result = {}
	print(r)
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
	result['year']= now.strftime("%Y")
	result['month'] = now.strftime("%m")
	result['date']= now.strftime("%d")
	result['time'] =now.strftime("%H:%M:%S")
	with open ('data.json', 'w') as outfile:
		json.dump(result, outfile)
ocr()
sys.exit()