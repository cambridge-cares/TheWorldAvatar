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
	
##this function removes duplicates while preserving order within an array
def remove_duplicates(seq):
	seen = set()
	seen_add = seen.add
	return [x for x in seq if not (x in seen or seen_add(x))]

##this function downloads natural gas futures prices for a gas delivered by Henry Hub pipeline; it is done by downloading their page source and parsing through it as if it was an XML file
def FAME(url_address, driver):
	#url_address = 'http://www.cmegroup.com/trading/energy/refined-products/fame-0-argus-biodiesel-fob-rdam-red-compliant-swap-futures.html'
	#downloading source code
	driver.get(url_address)
	tree = html.fromstring(driver.page_source)
	page = requests.get(url_address)
	#parsing and selecting relevant entries
	price = tree.xpath("//td[re:test(@id, 'quotesFuturesProductTable1_FBD[A-Z][0-9]_priorSettle')]/text()", namespaces={'re': "http://exslt.org/regular-expressions"})
	delivery = remove_duplicates(tree.xpath('//span[@class="cmeNoWrap"]/text()'))

	if len(price) == 0 or len(delivery) == 0:
		return "retry"

	arrayHeader = ["FAME", "Date", "Price type", "Size (tonne)"]

	arrayMonths = []
	for i in range(len(delivery)):
		arrayMonths.append(str(delivery[i]))

	arrayDatetime = ["{}".format(page.headers['Date']), "Prior Settlement (USD per tonne)", "100.0"]

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
	pythonLogger = PythonLogger('FAME_download.pyw')
	pythonLogger.postInfoToLogServer('start of FAME_download.pyw')
 
	urlAddress = str(sys.argv[1])
	driver = webdriver.PhantomJS()
 	  
	try:
		returnResultsToJava(FAME(urlAddress, driver))
		pythonLogger.postInfoToLogServer('Success')
	except Exception as e:
		returnExceptionToJava(e)
		pythonLogger.postInfoToLogServer('It seems that the page becomes unresponsive if it is queried too fast. Please wait 5 minutes and try again. Alternatively, the page address is incorrect or format of page\'s code changed')
	finally:
		driver.quit()