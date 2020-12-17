##
# @file
# File documentation
#

import warnings
warnings.filterwarnings("ignore")
from lxml import html
import requests, sys
import json
import urllib

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger

##this function downloads natural gas futures prices for a gas delivered by Henry Hub pipeline; it is done by downloading their page source and parsing through it as if it was an XML file
def exchange_rates():
	# This function converts an array of prices from one currency to another. It accepts an array with floats (but skips over non-floats) and an array with two strings. The latter need to be the correct currency codenames as per www.xe.com as this function will download the rates from there.
	# The first entry in the array with codenames corresponds to the present currency, while the second to the target currency. 
	
	currency_pairs = [['CNY', 'USD'], ['SGD', 'USD']]
	dictHeaderValue = {}

	url = 'http://apilayer.net/api/live?access_key=402d77f0850c35adfa5a797e325262dd&currencies=CNY,SGD&source=USD&format=1'
	f = urllib.request.urlopen(url)
	dict = json.loads(f.read().decode('utf-8'))

	dictQuotes = dict['quotes']

	i = 1
	for currencies in currency_pairs:
		dictHeaderValue['{}_to_{}'.format(currencies[1], currencies[0])] = dictQuotes['{}{}'.format(currencies[1], currencies[0])]

	return json.dumps(dictHeaderValue)



if __name__ == "__main__":
	pythonLogger = PythonLogger('exchange_rates.pyw')
	pythonLogger.postInfoToLogServer('start of exchange_rates.pyw')

	try:
		returnResultsToJava(exchange_rates())
		pythonLogger.postInfoToLogServer('end of exchange_rates.pyw')
	except Exception as e:
		returnExceptionToJava(e)
		pythonLogger.postInfoToLogServer('end of exchange_rates.pyw')