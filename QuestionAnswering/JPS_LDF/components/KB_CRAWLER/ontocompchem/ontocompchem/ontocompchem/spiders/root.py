import scrapy
import math
import json

class DownloadOntoCompChem(scrapy.Spider):
    name = "ontocompchem"

    def start_requests(self):
        urls = [
            'http://www.theworldavatar.com/kb/ontocompchem'
        ]
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response):
        all_items =  response.css('a tt::text').getall()
        directories = [item for item in all_items if not '.owl' in item]
        
        root_file_sizes =[float(text.replace(' kb','')) for text in response.css('td tt::text').getall() if 'kb' in text] 
        root_file_size_sum = sum(root_file_sizes) /1000/1000 # calculate the size 
        print('====== file size ========')
        print("{:.2f}".format(root_file_size_sum), 'GB') # find the file sizes in the root folder, calculate the full size
        
        owls = [item for item in all_items if '.owl' in item]
        
        with open('owls', 'w') as f:
            f.write(json.dumps(owls))
            f.close()
            
        with open('directories', 'w') as f:
            f.write(json.dumps(directories))
            f.close()
            
        print('======== number of owls =========')
        print(len(owls))
        