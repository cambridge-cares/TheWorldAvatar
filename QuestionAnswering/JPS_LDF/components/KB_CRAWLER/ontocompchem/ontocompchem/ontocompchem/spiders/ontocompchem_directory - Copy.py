import scrapy
import math
import json

class DownloadOntoCompChem(scrapy.Spider):
    name = "ontocompchem_subdirectory"
    
    def __init__(self):
        self.file_size_sum = 0
        self.owls_in_directory = []

    def start_requests(self):
    
        with open('directories') as f:
            directories = json.loads(f.read())
    
        urls = directories
        for directory in urls:
            url = 'http://www.theworldavatar.com/kb/ontocompchem/' + directory
            yield scrapy.Request(url=url, callback=self.parse, meta = {'directory':directory})
        
        with open('owls_in_directory', 'w') as f:
            f.write(json.dumps(self.owls_in_directory))
            f.close()
            
            
        
        
    def parse(self, response):
        all_items =  response.css('a tt::text').getall()
        directory = response.meta['directory']
        owls = [directory + item for item in all_items if '.owl' in item]
        print('========== numbers of owls ================', len(owls))
        
        self.owls_in_directory = self.owls_in_directory + owls 
        
        
        
        
        file_sizes =[float(text.replace(' kb','')) for text in response.css('td tt::text').getall() if 'kb' in text] 
        file_size_sum = sum(file_sizes)
        self.file_size_sum = self.file_size_sum + file_size_sum
        print('====== file size ========')
        print("{:.2f}".format(self.file_size_sum), 'kb') # find the file sizes in the root folder, calculate the full size
        
        
        