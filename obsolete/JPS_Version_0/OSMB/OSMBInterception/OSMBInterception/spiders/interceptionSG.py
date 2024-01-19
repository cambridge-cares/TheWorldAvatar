import scrapy

# The purpose of this spider is to obtain json files storing 3D models of Singapore
# the urls follow such pattern :
#  https://a.data.osmbuildings.org/0.2/anonymous/tile/15/25818/16266.json
#	25818 is the x parameter while 16266 is the y parameter
#	we will iterate from 25810 to 25830 and from 16250 to 16270



class QuotesSpider(scrapy.Spider):
	name = "interceptSG"

	def start_requests(self):
		urls = []
		# construct the array of urls 
		for y in range(16250,16271):
			for x in range(25810,25831):
				yString = str(y) 
				xString = str(x)
				url = ('https://a.data.osmbuildings.org/0.2/anonymous/tile/15/%s/%s.json'%(xString,yString))				
				urls.append({'url':url,'name':xString + '_' + yString}) # pass the name as well e.g. 25810_16266
		
		
		for url in urls:
			yield scrapy.Request(url=url['url'], callback=self.parse, meta = {'name': url['name']}) # pass the name as meta data

	def parse(self, response):
		filename = response.meta['name'] + '.json'
		with open(filename, 'w') as f:
			f.write(response.text)
		self.log('----------------------------')
		self.log('--- Saved file %s' % filename)