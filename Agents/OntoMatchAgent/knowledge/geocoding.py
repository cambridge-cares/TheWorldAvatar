
import logging

import rdflib

class Agent():

    def __init__(self):
        logging.info('initializing geocoding agent ...')
        addr = './data/municipalities_germany.ttl'
        frmt = 'turtle'
        self.graph = rdflib.Graph()
        self.graph.parse(addr, format=frmt)
        self.graph.bind('sdo', rdflib.SDO)
        self.graph.bind('geo', rdflib.Namespace('http://www.w3.org/2003/01/geo/wgs84_pos#'))

        self.index = self.__create_index()
        logging.info('geocoding agent initialized')

    def __normalize(self, s:str):
        return s.strip().lower()

    def __create_index(self):
        logging.info('create index')
        # one dictionary for both labels and postal codes
        # d = { 'label' or 'postalcode : { municipality IRIs }}
        d = {}
        query = '''
        SELECT ?munic ?label ?postalCode
        WHERE {
        ?munic rdfs:label ?label .
        OPTIONAL { ?munic sdo:postalCode ?postalCode . }
        }'''

        result = self.graph.query(query)
        for row in result:
            #print(row['munic'], row['label'], row['postalCode'])

            munic_iri = row['munic']
            for prop in ['label', 'postalCode']:
                if not row[prop]:
                    continue
                value = row[prop].toPython()
                if prop == 'label':
                    value = self.__normalize(value)
                iris = d.get(value)
                if iris:
                    iris.append(munic_iri)
                else:
                    d[value] = [munic_iri]

        return d

    def query(self, location:str, zipcode:int) -> tuple[float, float]:

        found_iri = None

        iris_zipcode = self.index.get(zipcode) if zipcode else None
        if iris_zipcode:
            logging.info('several entries found for zipcode=%s', zipcode)

        iris_location = None
        if location:
            location_normalized = self.__normalize(location)
            iris_location = self.index.get(location_normalized)
            if iris_location:
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
                #print('MY COORD', lat, long)
                if lat and long:
                    return lat.toPython(), long.toPython()

        logging.info('no geo coordinates found for location=%s, zipcode=%s', location, zipcode)
        return None, None
