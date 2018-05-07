##
# @file
# File documentation
#


from lxml import html
from re import sub
import requests, time, sys
from csv_funcs import RCSV, ACSV, WCSV
from selenium import webdriver

	
##this function removes duplicates while preserving order within an array
def remove_duplicates(seq):
	seen = set()
	seen_add = seen.add
	return [x for x in seq if not (x in seen or seen_add(x))]

##this function takes old data (split into an array with headers and content) and new data (same split), merges them together, and returns 2 arrys: headers and content; right now it only works if there are new headers added to the end
def adjust_columns(previous_headers, previous_data, new_headers, new_data):
	# previous_headers = [1,2,3,4,5,6]
	# new_headers = [3,4,5,6,7,8,9]
	# previous_data = [1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6]
	# new_data = [3, 4, 5, 6, 7, 8, 9]


	merged_headers = remove_duplicates(previous_headers+new_headers)
	merged_data = []
	split_previous_data = []

	##this loop splits old content into seperate array (as long as the header array) and, if merged_headers are longer than previous_headers, adds an appropriate nuber of '-' at the end of all rows
	for i in range(int(len(previous_data)/len(previous_headers))):
		temp = previous_data[i*len(previous_headers):(i+1)*len(previous_headers)]+['-']*(len(merged_headers)-len(previous_headers))
		split_previous_data.append(temp)

	split_previous_data.append(new_data)
	
	#content is merged back into a single array
	for array in split_previous_data:
		merged_data += array
		
	# print(split_previous_data)
	# print(merged_data)
	return merged_headers, merged_data
 
 ##this function downloads methanol futures prices for a methanol delivered by Zhengzhou Commodity Exchange; it is done by downloading their page source and parsing through it as if it was an XML file
def ZCE(file_address, driver):
	url = 'http://english.czce.com.cn/enportal/DFSStaticFiles/Future/EnglishFutureQuotesMA.htm'
	page = requests.get(url)
	tree = html.fromstring(page.content)
	data = tree.xpath('//td/text()')
	delivery = []
	price = []

	for i in range(int(len(data)/7-1)):
		delivery.append(data[(i+1)*7])
		price.append(float(sub(',','',data[(i+1)*7+6])))
	
	try:
		prev_data, prev_headers = RCSV(file_address)
		if prev_data[-len(prev_headers)][:16] == page.headers['Date'][:16]:
			raise NameError('The data has already been downloaded today.') 

	except NameError:
		pass
		
	except FileNotFoundError:
		print('File does not exist and will be created.')
		ACSV(file_address, [page.headers['Date'], 'Prior Settlement (CNY per tonne)', 1.0]+price, ['Date', 'Price type', 'Size (tonne)']+delivery)
		time.sleep(0.5)
	
	else:
		merged = [[],[]]
		merged[0],merged[1] = adjust_columns(prev_headers, prev_data, delivery,[page.headers['Date'], 'Prior Settlement (CNY per tonne)', 1.0]+['-']*(len(remove_duplicates(prev_headers+delivery))-len(['Date', 'Price type', 'Size (tonne)']+delivery))+price)
		page.close()
		WCSV(file_address, merged[1], merged[0])


def run(file_address):		
	driver = webdriver.PhantomJS()
	#file_address = r'C:\Users\Janusz\Desktop\Commodity_prices\Market_data\ZCE_data.csv'
	ZCE(file_address, driver)
	driver.quit()
	print('Success')

if __name__ == "__main__":
	run(str(sys.argv[1]))