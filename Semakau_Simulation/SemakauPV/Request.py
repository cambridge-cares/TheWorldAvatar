#when installing selenium you need to download geckodriver.exe and add its and path to the environment variables
#in order to run in a headless mode (i.e. no separate browser window) you need to download phantomjs and add its and path to the environment variables
#after adding paths the OS needs a restart
#also, the script may need to be run in admin mode (at least on Windows 10)
from lxml import html
from time import sleep
import requests
from selenium import webdriver
#driver = webdriver.Firefox()
driver = webdriver.PhantomJS() 

currentTime = ''


lastline = ''
f = open('data.csv')
temp = f.readlines()
length = 0
if(len(temp)==0):
	lastline = ''
else:
	lastline = temp[len(temp) - 1]



url = 'https://inetapps.nus.edu.sg/fas/geog/'
driver.get(url)
tree = html.fromstring(driver.page_source)
data = tree.xpath("//div[@id='divLatestData']//li/text()")
print('Year',data[0])
print('Day',data[1])
print('Time',data[2])
print(data[8])#

tsvLine = data[0] + "," + data[1] + "," + data[2] + "," + data[8] + "\n"

if(tsvLine == lastline):
	print('tick')
else:
	with open("data.csv", "a") as myfile:
		#myfile.write(str(data))
		myfile.write(tsvLine)
		currentTime = data[2]
		
# where the Solar Radiation is stored 
#headers
#headers = tree.xpath("//thead/tr/th/text()")

"//div[@id='a']//a[@class='click']"

 

#for i in range(len(data)):
#	if i % 12 == 0: print(data[i])
#	if i % 12 == 7: print(data[i])