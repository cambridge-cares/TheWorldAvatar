##
# @file
# File documentation
#

import warnings
warnings.filterwarnings("ignore")
from lxml import html
from re import sub
import requests, sys
from selenium import webdriver
import json

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger
	
##this function removes duplicates while preserving order within an array
def remove_duplicates(seq):
	seen = set()
	seen_add = seen.add
	return [x for x in seq if not (x in seen or seen_add(x))]

##this function downloads natural gas futures prices for a gas delivered by Henry Hub pipeline; it is done by downloading their page source and parsing through it as if it was an XML file
def CPO(url_address, driver):
	#url_address = 'http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/usd-malaysian-crude-palm-oil-calendar.html?optionProductId=8075'

	#downloading source code
	driver.get(url_address)
	tree = html.fromstring(driver.page_source)
	page = requests.get(url_address)
	#parsing and selecting relevant entries
	price = tree.xpath("//td[re:test(@id, 'quotesFuturesProductTable1_CPO[A-Z][0-9]_priorSettle')]/text()", namespaces={'re': "http://exslt.org/regular-expressions"})
	delivery = remove_duplicates(tree.xpath('//span[@class="cmeNoWrap"]/text()'))

	if len(price) == 0 or len(delivery) == 0:
		return "retry"

	# string = '&CPO,Date,Price type,Size (tonne)'
	# for i in range(len(delivery)):
	# 	string += "," + delivery[i]

	# string += '&'+page.headers['Date']+ ',Prior Settlement (USD per tonne),25.0'
	# for i in range(len(price)):
	# 	string += "," + price[i]
	#
	# print(string)

	arrayHeader = ["CPO", "Date", "Price type", "Size (tonne)"]

	arrayMonths = []
	for i in range(len(delivery)):
		arrayMonths.append(str(delivery[i]))

	arrayDatetime = ["{}".format(page.headers['Date']), "Prior Settlement (USD per tonne)", "25.0"]

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

# def run(url_address):
# 	driver = webdriver.PhantomJS()
# 	try:
# 		CPO(url_address, driver)
# 		driver.quit()
# 		print('Success')
#
# 	except:
# 		driver.quit()
# 		print('It seems that the page becomes unresponsive if it is queried too fast. Please wait 5 minutes and try again. Alternatively, the page address is incorrect or format of page\'s code changed')

if __name__ == "__main__":
	# run(str(sys.argv[1]))
	pythonLogger = PythonLogger('CPO_download.pyw')
	pythonLogger.postInfoToLogServer('start of CPO_download.pyw')

	urlAddress = str(sys.argv[1])
	# urlAddress = "http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/usd-malaysian-crude-palm-oil-calendar.html?optionProductId=8075"
	driver = webdriver.PhantomJS()

	try:
		returnResultsToJava(CPO(urlAddress, driver))
		driver.quit()
		pythonLogger.postInfoToLogServer('Success')
	except Exception as e:
		returnExceptionToJava(e)
		driver.quit()
		pythonLogger.postInfoToLogServer('It seems that the page becomes unresponsive if it is queried too fast. Please wait 5 minutes and try again. Alternatively, the page address is incorrect or format of page\'s code changed')