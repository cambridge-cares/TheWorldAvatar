from matchers import GeoAttrFinder
import logging
class Agent():

    def __init__(self, featureSelect = '3F'):
        logging.info('initializing geoNames agent')
        #Import global file
        addr = "./data/geoName_dict_All_Feature.json"
        self.attrFinder = GeoAttrFinder(featureSelect, addr)



    def query(self, geoName:str, country:str):
        #check feature selection method
        result = self.attrFinder.getCoordiIfGeoname(geoName,country)
        if len(result)>0:
            return result[0],result[1]
        logging.info('no geo coordinates found for geoName=%s, country=%s', geoName, country)
        return None, None
