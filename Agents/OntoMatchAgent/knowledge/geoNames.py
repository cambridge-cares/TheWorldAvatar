from matchers import GeoAttrFinder
import logging
import time
import requests
class Agent():

    def __init__(self, country="Germany"):
        logging.info('initializing geoNames agent')
        #Import global file
        #addr = "./data/geoName_dict_All_Feature.json"
        #self.attrFinder = GeoAttrFinder(featureSelect, addr)
        self.EP = "http://www.theworldavatar.com/blazegraph/namespace/geonames/sparql"
        self.country = country
        self.lookup = {}
        self.buildDict(country)
        self.featureSelect = "featureSelect3F" #TODO:make it into a parameter later

    #query to get all names
    def buildDict(self, country):
        starttime = time.time()
        #TODO, check country name validity
        query = '''
        PREFIX gn:<http://www.geonames.org/ontology#>
        SELECT distinct ?s ?n FROM <Geonames_{}>
        {{ {{ ?s a <http://www.geonames.org/ontology#Feature>.
         ?s gn:name ?n.
         }} UNION {{
         ?s a <http://www.geonames.org/ontology#Feature>.
         ?s gn:alternateName ?n.
         }} }}
        '''.format(country)
        res = requests.post(self.EP, params={'query': query, 'format': 'json'})
        if res.status_code != 200:
            logging.info("can not connect to endpoint, status:"+str(res.status_code))
            raise ConnectionError("Error connecting to endpoint with status code {}".format(str(res.status_code)))
        jres = res.json()
        for line in jres['results']['bindings']: #{'head': {'vars': ['n']}, 'results': {'bindings': [{'n': {'type': 'literal', 'value': 'Weikering'}}]}}
            key = line['n']['value'].lower()
            if key not in self.lookup:
                self.lookup[key] = []
            self.lookup[key].append(line['s']['value'])
        endtime = time.time() - starttime
        logging.info("Dictionary for {} built in {} s with {} entries".format(country, endtime, len(self.lookup.keys())))

    def query(self, name):
        #exact match
        name = name.lower()
        if name in self.lookup:
            IRIs = self.lookup[name]
            try:
                clist = self.requestCoordinate(IRIs)
                #TODO: error handling, empty list
                return self.featureSelect3F(clist)
            except (ValueError, ConnectionError):
                logging.info("geoname:{} with IRI:{} query coordinates failed".format(name,IRI))
                return None, None
        else:
            logging.info("geoname:{} Not found".format(name))
            return None, None


    def requestCoordinate(self, IRIs):
        coords = []
        for IRI in IRIs:
            query = '''
                PREFIX gn:<http://www.geonames.org/ontology#>
                PREFIX wgs84_pos:<http://www.w3.org/2003/01/geo/wgs84_pos#>
        
                SELECT ?lat ?lng ?fcode FROM <Geonames_{}>
                {{ <{}> a <http://www.geonames.org/ontology#Feature>.
                 <{}> wgs84_pos:lat ?lat.
                 <{}> wgs84_pos:long ?lng.    
                 <{}> gn:featureCode ?fcode.
                 }}
                '''.format(self.country, IRI, IRI, IRI, IRI)
            #rdf:resource="https://www.geonames.org/ontology#P.PPL"
            res = requests.post(self.EP, params={'query': query, 'format': 'json'})
            #logging.info("connect to endpoint, status:"+str(res.status_code))
            if res.status_code != 200:
                raise ConnectionError("Error connecting to endpoint with status code {}".format(str(res.status_code)))
            jres = res.json()
            if 'results' not in jres or 'bindings' not in jres['results'] or len(jres['results']['bindings']) < 1:
                logging.info("coordinates of geoname: {} not found in endpoint".format(IRI))
            else:
                lat = float(jres['results']['bindings'][0]['lat']['value'])
                lng = float(jres['results']['bindings'][0]['lng']['value'])
                fcode = jres['results']['bindings'][0]['fcode']['value']
                coords.append((lat, lng, fcode))
            if len(coords) == 0:
                raise ValueError('coordinates of geoname not found in endpoint')
        return coords


    def featureSelect3F(self, listOfFeature):
        for feature in listOfFeature:
            x,y,fcode = feature
            x = float(x)
            y = float(y)
            if "ADM" in fcode:
                return x,y
            elif "PPL" in fcode:
                return x,y
        else:
            return None, None


'''
    
    @deprecated
    def query_deprecated(self, geoName:str, country:str):
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
'''
