from matchers import GeoAttrFinder
import logging
class Agent():

    def __init__(self, featureSelect = '3F'):
        logging.info('initializing geoNames agent')
        #Import global file
        addr = "../data/geoName_dict_All_Feature.json"
        self.attrFinder = GeoAttrFinder(featureSelect, addr)



    def query(self, geoName:str, country:str):
        #check feature selection method
        if geoName is None:
            logging.warning('GeonameAgent: geoName parameter not defined for searching')
            return None, None

        if country is None:
            logging.warning('GeonameAgent: country parameter is not defined when searching geoName=%s')
            return None, None

        result = self.attrFinder.getCoordiIfGeoname(geoName,country)
        if len(result)>0:#TODO: might implement multiple return in the future
            return result[0],result[1]
        logging.info('no geo coordinates found for geoName=%s, country=%s', geoName, country)
        return None, None

    def onclose(self):
        self.attrFinder.save()
