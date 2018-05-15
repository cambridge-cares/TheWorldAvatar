##
# @file
# File documentation
#


from lxml import html
from re import sub
import requests, sys
from selenium import webdriver

 ##this function downloads methanol futures prices for a methanol delivered by Zhengzhou Commodity Exchange; it is done by downloading their page source and parsing through it as if it was an XML file
def ZCE(url_address, driver):
	#url_address = 'http://english.czce.com.cn/enportal/DFSStaticFiles/Future/EnglishFutureQuotesMA.htm'
	page = requests.get(url_address)
	tree = html.fromstring(page.content)
	data = tree.xpath('//td/text()')
	delivery = []
	price = []

	for i in range(int(len(data)/7-1)):
		delivery.append(data[(i+1)*7])
		price.append(float(sub(',','',data[(i+1)*7+6])))
	
	string = 'Date,Price type,Size (tonne)'
	for i in range(len(delivery)):
		string += "," + str(delivery[i])

	string += page.headers['Date']+ ',Prior Settlement (CNY per tonne),1.0'
	for i in range(len(price)):
		string += "," + str(price[i])
	
	print(string)	


def run(url_address):		
	driver = webdriver.PhantomJS()
	try:
		ZCE(url_address, driver)
		driver.quit()
		print('Success')
		
	except:
		driver.quit()
		print('It seems that the page becomes unresponsive if it is queried too fast. Please wait 5 minutes and try again. Alternatively, the page address is incorrect or format of page\'s code changed')
		
if __name__ == "__main__":
	run(str(sys.argv[1]))