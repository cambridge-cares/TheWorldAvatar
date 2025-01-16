import scrapy
import math
import json

class DownloadOntoKin(scrapy.Spider):
    name = "ontokin_files"
    
    def __init__(self):
        self.file_size_sum = 0
        self.owls_in_directory = []

    def start_requests(self):
    
        with open('root_owls') as f:
            root_owls = json.loads(f.read())
    
        with open('owls_in_directory') as f:
            owls_in_directory = json.loads(f.read())
          
        # all_owl_files = list(set(owls_in_directory)) # to test with less files 

        all_owl_files = list(set(root_owls + owls_in_directory))
        
        urls = all_owl_files
        for name in urls:
            url = 'http://www.theworldavatar.com/kb/ontokin/' + name
            yield scrapy.Request(url=url, callback=self.parse,  meta={'name': name})
         
        
        
    def parse(self, response):
        name = response.meta['name']
        if '/' in name:
            name = name.split('/')[1]
        else:
            name = name 
            
        with open('owlfiles/' + name, 'wb') as f:
            f.write(response.body)
            f.close()
        
        
        