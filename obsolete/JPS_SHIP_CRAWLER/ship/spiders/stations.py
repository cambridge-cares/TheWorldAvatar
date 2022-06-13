import scrapy

class get_all_stations(scrapy.Spider):
	name = "stations"
	def start_requests(self):
		url = 'http://www.aishub.net/stations?Station%5BSID%5D=&Station%5Bstatus%5D=0&Station%5Buptime%5D=&Station%5BCOUNTRY%5D=Hong%20Kong&Station%5BLOCATION%5D=&Station%5BCOUNT%5D=&Station%5BDISTINCT%5D=&sort=COUNTRY';

		yield scrapy.Request(url=url, callback=self.parse)
			
	def parse(self, response):
		station_list = response.css('td[data-col-seq="0"]::text').extract()
		ship_number_list = response.css('td[data-col-seq="6"]::text').extract()
		with open('./station_list.txt','w') as file:
			for station,number in zip(station_list,ship_number_list):
				if int(number) >= 20:
					file.write(station + "\n")
