##
# @file
# File documentation
#


from lxml import html
import requests, sys
import json

##this function downloads natural gas futures prices for a gas delivered by Henry Hub pipeline; it is done by downloading their page source and parsing through it as if it was an XML file
def exchange_rates():
	# This function converts an array of prices from one currency to another. It accepts an array with floats (but skips over non-floats) and an array with two strings. The latter need to be the correct currency codenames as per www.xe.com as this function will download the rates from there.
	# The first entry in the array with codenames corresponds to the present currency, while the second to the target currency. 
	
	currency_pairs = [['CNY', 'USD'], ['SGD', 'USD']]
	rates = []
	headers = []
	string1 = ""
	string2=""
	dictHeaderValue = {}

	for currencies in currency_pairs:
		# Url is being formulated
		url_address = 'http://www.xe.com/currencyconverter/convert/?Amount=1&From='+currencies[1]+'&To='+currencies[0]
		
		# requests library is used to download the source code of the page with exchange rates. The code is then parsed as html.
		page = requests.get(url_address)
		tree = html.fromstring(page.content)
		page.close()
		
		# lxml library is used to search through the html file using its structure and attributes
		exchange_rate = float(tree.xpath('//span[@class="uccResultUnit"]')[0].attrib['data-amount'])
		# rates.append(str(exchange_rate))
		# headers.append(currencies[1]+'_to_'+currencies[0])
        #
		# string1 = string1 + currencies[1] + '_to_' + currencies[0]+ ","
		# string2 = string2 + str(exchange_rate)+ ","

		dictHeaderValue['{}_to_{}'.format(currencies[1], currencies[0])] = str(exchange_rate)

	# string2 = string2[:-1]
	# print(string1 + string2)
	print(json.dumps(dictHeaderValue))



if __name__ == "__main__":
	exchange_rates()