
import logging

import rdflib

import knowledge.search

class Agent():

    def __init__(self):
        logging.info('initializing geocoding agent')
        addr = './data/municipalities_germany.ttl'
        frmt = 'turtle'
        self.graph = rdflib.Graph()
        self.graph.parse(addr, format=frmt)
        self.graph.bind('sdo', rdflib.SDO)
        self.graph.bind('geo', rdflib.Namespace('http://www.w3.org/2003/01/geo/wgs84_pos#'))

        #self.index = self.__create_index()
        properties = ['rdfs:label', 'sdo:postalCode']
        self.index = knowledge.search.create_index(addr, frmt, properties)
        logging.info('created index with %s keys', len(self.index))
        logging.info('geocoding agent initialized')

    def query(self, location:str, zipcode:int) -> tuple[float, float]:

        found_iri = None

        iris_zipcode = self.index.get(zipcode) if zipcode else None
        if iris_zipcode and len(iris_zipcode) > 1:
            logging.info('several entries found for zipcode=%s', zipcode)

        iris_location = None
        if location:
            location_normalized = knowledge.search.normalize(location)
            iris_location = self.index.get(location_normalized)
            if iris_location and len(iris_location) > 1:
                logging.info('several entries found for location=%s', location)

        if iris_zipcode and not iris_location:
            found_iri = iris_zipcode[0]
        elif iris_location and not iris_zipcode:
            found_iri = iris_location[0]
        elif iris_zipcode and iris_location:
            # return the first common IRI
            for i in iris_zipcode:
                if i in iris_location:
                    found_iri = i
                    break

            if not found_iri:
                found_iri = iris_zipcode[0]

        if found_iri:
            sparql_iri = '<' + found_iri + '>'
            query = '''
            SELECT ?lat ?long
            WHERE {
            %s geo:lat ?lat .
            %s geo:long ?long .
            }''' % (sparql_iri, sparql_iri)

            result = self.graph.query(query)
            for row in result:
                lat = row['lat']
                long = row['long']
                if lat and long:
                    return lat.toPython(), long.toPython()

        logging.info('no geo coordinates found for location=%s, zipcode=%s', location, zipcode)
        return None, None
