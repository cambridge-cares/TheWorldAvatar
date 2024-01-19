from ontomatch.matchers import GeoAttrFinder
import logging
import time
import requests
from os.path import exists
import pickle
class Agent():

    def __init__(self, country="Germany", isOnline = False, save = None):
        logging.info('initializing geoNames agent')
        #Import global file
        #addr = "./data/geoName_dict_All_Feature.json"
        #self.attrFinder = GeoAttrFinder(featureSelect, addr)
        self.EP = "http://www.theworldavatar.com/blazegraph/namespace/geonames/sparql"
        self.country = country
        self.lookup = {}
        self.coordiDict = {}
        self.online = isOnline
        self.pklAddr = None
        #check if save file already exist:
        if save is not None and type(save) is str:
            if exists(save):
                self.loadDictFromSave(save)
            else:
                self.pklAddr = save
                if isOnline:
                    self.buildDict()
                else:
                    self.buildDictCoordi()
        self.featureSelect = "featureSelect3F" #TODO:make it into a parameter later

    def loadDictFromSave(self, loadAddr):
        with open(loadAddr, 'rb') as f:
            #TODO: err handling
            self.lookup, self.coordiDict = pickle.load(f)
            logging.info("Lookup dictionary loaded from {}".format(loadAddr))


    #query to get all names
    def buildDict(self):
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
        '''.format(self.country)
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
        logging.info("Dictionary for {} built in {} s with {} entries".format(self.country, endtime, len(self.lookup.keys())))
        if self.pklAddr is not None:
            self.saveLookup()
            logging.info("Dictionary saved to {}".format(self.pklAddr))

    def buildDictCoordi(self):
        starttime = time.time()
        logging.info("build Dictionary starts")

        #TODO, check country name validity by ASK
        query = '''
        PREFIX gn:<http://www.geonames.org/ontology#>
        PREFIX wgs84_pos:<http://www.w3.org/2003/01/geo/wgs84_pos#>
        SELECT distinct ?s ?n ?lat ?lng ?fcode ?alt FROM <Geonames_{}>
        {{  ?s a <http://www.geonames.org/ontology#Feature>.
         ?s gn:name ?n.
         ?s wgs84_pos:lat ?lat.
         ?s wgs84_pos:long ?lng.
         ?s gn:featureCode ?fcode.
    OPTIONAL {{ ?s gn:alternateName ?alt }}}}
       '''.format(self.country)
        res = requests.post(self.EP, params={'query': query, 'format': 'json'})
        if res.status_code != 200:
            logging.info("can not connect to endpoint, status:%s", res.status_code)
            raise ConnectionError("Error connecting to endpoint with status code {}".format(str(res.status_code)))
        jres = res.json()
        for line in jres['results']['bindings']: #{'head': {'vars': ['n']}, 'results': {'bindings': [{'n': {'type': 'literal', 'value': 'Weikering'}}]}}
            geoname = line['n']['value'].lower()
            IRI = line['s']['value']
            if geoname not in self.lookup:
                self.lookup[geoname] = set()
            self.lookup[geoname].add(IRI)
            if 'alt' in line:
                altname = line['alt']['value'].lower()
                if altname not in self.lookup:
                    self.lookup[altname] = set()
                self.lookup[altname].add(IRI)


            lat = float(line['lat']['value'])
            lng = float(line['lng']['value'])
            fcode = line['fcode']['value']
            self.coordiDict[IRI] = (lat, lng, fcode)

        endtime = time.time() - starttime
        logging.info("Dictionary for {} built in {} s with {} entries".format(self.country, endtime, len(self.lookup.keys())))
        if self.pklAddr is not None:
            self.saveLookup()
            logging.info("Dictionary saved to {}".format(self.pklAddr))

    def query(self, name):
        #exact match
        start = time.time()
        name = name.lower()
        if name in self.lookup:
            IRIs = list(self.lookup[name])
            try:
                if len(IRIs) == 0:
                    raise ValueError
                if self.online:
                    clist = self.requestCoordinate(IRIs)
                else:
                    clist = [self.coordiDict[IRI] for IRI in IRIs]
                #TODO: error handling, empty list
                if len(clist) == 0:
                    raise ValueError
                logging.info("coordi query finished in {} s".format(str(time.time()-start)))
                return self.featureSelect3F(clist)
            except (ValueError, ConnectionError):
                logging.info("geoname:{} with IRI:{} query coordinates failed".format(name,IRIs))
                return None, None
        else:#Geoname not found
            logging.info("geoname:{} Not found".format(name))
            return None, None

    def saveLookup(self):
        with open(self.pklAddr, 'wb') as f:
            pickle.dump((self.lookup, self.coordiDict), f)


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


    '''
    listOfFeature: Not-empty list of (x, y, fcode)
    '''
    def featureSelect3F(self, listOfFeature):
        # Choose in this order: ADM > PPL > Others
        chosen = listOfFeature[0]
        for feature in listOfFeature:
            if feature is False:#check not empty
                continue
            x,y,fcode = feature
            x = float(x)
            y = float(y)
            if "ADM" in fcode: #If we found ADM, don't need to consider others, return
                return x,y
            elif "PPL" in fcode:
                chosen = feature

        #LOOP is over, ADM is not found! chosen is either any PPL feature or the first feature
        return float(chosen[0]), float(chosen[1])

    #TODO: more refined feature selection

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
