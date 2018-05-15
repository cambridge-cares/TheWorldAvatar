##
# @file
# File documentation
#


from lxml import html
from re import sub
import requests, sys
from selenium import webdriver

	
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
	
	string = 'Date, Price type, Size (tonne)'
	for i in range(len(delivery)):
		string += "," + delivery[i]
	
	string += page.headers['Date']+ 'Prior Settlement (USD per tonne), 100.0'
	for i in range(len(price)):
		string += "," + price[i]
	
	print(string)



		
def run(url_address):		
	driver = webdriver.PhantomJS()
	try:
		CPO(url_address, driver)
		driver.quit()
		print('Success')
		
	except:
		driver.quit()
		print('It seems that the page becomes unresponsive if it is queried too fast. Please wait 5 minutes and try again. Alternatively, the page address is incorrect or format of page\'s code changed')

		
if __name__ == "__main__":
	run(str(sys.argv[1]))