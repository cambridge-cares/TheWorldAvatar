##
# @file
# File documentation
#


from lxml import html
from re import sub
import requests, sys
from selenium import webdriver
import json

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger

# takes in a time period e.g. MA809 and returns a string denoting the month and year SEP 2018
def convertMarketTimePeriods(stringTimePeriod):
	dictMonths = {
        '01': 'JAN',
        '02': 'FEB',
        '03': 'MAR',
        '04': 'APR',
        '05': 'MAY',
        '06': 'JUN',
        '07': 'JUL',
        '08': 'AUG',
        '09': 'SEP',
        '10': 'OCT',
        '11': 'NOV',
        '12': 'DEC'
    }
	
	year = "201{}".format(stringTimePeriod[2])
	return dictMonths['{}'.format(stringTimePeriod[-2:])] + " " + year;


 ##this function downloads methanol futures prices for a methanol delivered by Zhengzhou Commodity Exchange; it is done by downloading their page source and parsing through it as if it was an XML file
def ZCE(url_address, driver):
	#url_address = 'http://english.czce.com.cn/enportal/DFSStaticFiles/Future/EnglishFutureQuotesMA.htm'
	page = requests.get(url_address)
	tree = html.fromstring(page.content)
	data = tree.xpath('//td/text()')
	delivery = []
	price = []

	for i in range(int(len(data)/7-1)):
		delivery.append(convertMarketTimePeriods(str(data[(i+1)*7])))
		price.append(float(sub(',','',data[(i+1)*7+6])))
	
	if len(price) == 0 or len(delivery) == 0:
		return "retry"

	arrayHeader = ["MeOH", "Date", "Price type", "Size (tonne)"]

	arrayMonths = []
	for i in range(len(delivery)):
		arrayMonths.append(str(delivery[i]))

	arrayDatetime = ["{}".format(page.headers['Date']), "Prior Settlement (CNY per tonne)", "1.0"]

	arrayPrices = []
	for i in range(len(price)):
		arrayPrices.append(str(price[i]))

	results = {
		"arrayHeader": arrayHeader,
		"arrayMonths": arrayMonths,
		"arrayDatetime": arrayDatetime,
		"arrayPrices": arrayPrices
	}

	return json.dumps(results)

if __name__ == "__main__":
	pythonLogger = PythonLogger('ZCE_download.pyw')
	pythonLogger.postInfoToLogServer('start of ZCE_download.pyw')
 
	urlAddress = str(sys.argv[1])
	driver = webdriver.PhantomJS()
 	  
	try:
		returnResultsToJava(ZCE(urlAddress, driver))
		pythonLogger.postInfoToLogServer('Success')
	except Exception as e:
		returnExceptionToJava(e)
		pythonLogger.postInfoToLogServer('It seems that the page becomes unresponsive if it is queried too fast. Please wait 5 minutes and try again. Alternatively, the page address is incorrect or format of page\'s code changed')
	finally:
		driver.quit()