# redundant method if ocrv1.py is not in use (ie method has changed for solar repository IRP
from PIL import Image
from pytesseract import *
import urllib.request
from datetime import datetime
import json
import time
import random
import requests
#read and download image
def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False
def ocr():
    result = {}
    URL = "https://api.data.gov.sg/v1/environment/air-temperature"
    r = requests.get(url = URL)
    data = r.json()
    temp = str(data["items"][0]["readings"][1]["value"])
    result["temperature"] = temp
    URL2 = "https://api.data.gov.sg/v1/environment/wind-speed"
    r = requests.get(url = URL2)
    data = r.json()
    speed = str(data["items"][0]["readings"][1]["value"])
    result["windspeed"] = speed
    url = 'https://www.solar-repository.sg/ftp_up/irradiance/NSR_IrrMap.png'
    response = urllib.request.urlretrieve(url, 'NSR_IrrMap.png')
    #scan image provided. 
    im = Image.open('NSR_IrrMap.png')
    text = image_to_string(im)
    im.close()
    r = text.split('\n')
    for i in r:
        print(i)
        if 'Avg' in i:
            temp = i.split(' ')[1]
            if (not is_number(temp)):
                temp = "{0:.2f}".format(random.uniform (26, 32))
            result["irradiance"] = temp
    #     elif i.startswith('Global'):
    #         irrad = i.split(' ')[2]
    #         if (irrad == '|'):
    #             irrad = i.split(' ')[3]
    #         if (len(irrad.split('.')))>2:
    #             irrad = irrad[:-1]
    #         if (not is_number(irrad)):
    #             #check the time. if night, then print zero. 
    #             hou = datetime.now().hour
    #             if (( hou <= 7 )or (hou > 18)):
    #                 irrad = 0 
    #             else:
    #                 irrad="{0:.2f}".format(random.uniform(0,100))
    #         result['irradiance'] =irrad
    #     elif "Irradiance" in i: 
    #         irrad = i.split(' ')[7]
    #         if (irrad == '|'):
    #             irrad = i.split(' ')[3]
    #         if (len(irrad.split('.')))>2:
    #             irrad = irrad[:-1]
    #         if (not is_number(irrad)):
    #             #check the time. if night, then print zero. 
    #             hou = datetime.now().hour
    #             if (( hou <= 7 )or (hou > 18)):
    #                 irrad = 0 
    #             else:
    #                 irrad="{0:.2f}".format(random.uniform(0,100))
    #         result['irradiance'] =irrad
    #     elif 'Speed' in i:
    #         speed = i.split(' ')[2]
    #         if (speed == '|'):
    #             speed = i.split(' ')[3]
    #         if (not is_number(speed)):
    #             speed = "{0:.2f}".format(random.uniform(0,5))
            
    #         result["windspeed"] = speed
    if "windspeed" not in result:
        result["windspeed"] = "0.0"
    if "irradiance" not in result:
        result["irradiance"] ="{0:.2f}".format(random.uniform(0,100))
    if "temperature" not in result:
        result["temperature"] =  "{0:.2f}".format(random.uniform(26,32))
    now = datetime.now() # current date and time
    
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
        result= {}
        result["windspeed"] = "0.0"
        
        now = datetime.now() # current date and time
        hou = now.hour
        if (( hou <= 7 )or (hou > 18)):
            irrad = 0 
        else:
            irrad="{0:.2f}".format(random.uniform(0,100))
        result["temperature"] =  "{0:.2f}".format(random.uniform(26,32))
        result['year']= now.strftime("%Y")
        result['month'] = now.strftime("%m")
        result['date']= now.strftime("%d")
        result['time'] =now.strftime("%H:%M:%S")
        with open ('data.json', 'w') as outfile:
            json.dump(result, outfile)
        outfile.write('I said end of message')
