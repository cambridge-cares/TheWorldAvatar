import logging

import rdflib

class Agent():

    def __init__(self):
        logging.info('initializing search agent')
        addr = './data/power_plant_DEU/municipalities_germany.ttl'
        properties = ['rdfs:label', 'sdo:postalCode']
        self.index = create_index(addr, 'turtle', properties)

    def search(self, string: str) -> list[str]:
        return self.index[str]


def normalize(value):
    if isinstance(value, str):
        return value.strip().lower()
    return value

def create_index(address, frmt, properties):
    # create a single dictionary for both labels and postal codes
    # d has the form { property object or value : { subject IRIs }}
    d = {}

    query = 'SELECT ?subject'
    for i, prop in enumerate(properties):
        query += ' ?obj' + str(i)
    query += '\nWHERE {'
    for i, prop in enumerate(properties):
        query += '\nOPTIONAL { ?subject ' + prop + ' ?obj' + str(i) + ' . }'
    query += '\n}'

    logging.debug('SPARQL=%s', query)

    graph = rdflib.Graph()
    graph.parse(address, format=frmt)
    result = graph.query(query)

    for row in result:
        subject = row['subject']
        for i, prop in enumerate(properties):
            obj= row['obj' + str(i)]
            if not obj:
                continue
            obj = obj.toPython()
            obj = normalize(obj)
            iris = d.get(obj)
            if iris:
                iris.append(subject)
            else:
                d[obj] = [subject]

    return d
