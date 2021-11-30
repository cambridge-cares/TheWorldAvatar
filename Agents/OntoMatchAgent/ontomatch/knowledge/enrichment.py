import logging
import typing

import rdflib
import rdflib.namespace
from tqdm import tqdm

import ontomatch.utils.blackboard
import ontomatch.utils.util
import ontomatch.knowledge.geocoding
import ontomatch.knowledge.geoNames

class Agent():

    def start(self, addr:str, add_knowledge:bool, http:bool=False) -> typing.Tuple[bool, str]:

        enriched, addr, graph = self.load_rdflib_graph(addr, add_knowledge)

        # Write the graph into the blackboard and return the handle
        # such that other agent can read (serialized) graph from the blackboard.
        # This is done both cases where the graph was enriched with addition background knowledge or not
        # Thus, in either case, the graph is available via the blackboard and instance matchers can read it
        # However, the conversion of a rdflib graph to str does not work properly:
        #graph_str = ontomatch.utils.util.serialize_graph_to_str(graph)
        #handle = ontomatch.utils.util.call_agent_blackboard_for_writing(addr, graph_str, http)
        # Thus, we use a hack here (we use format='xml' because owlready2 does not support loading from turtle file)
        handle = ontomatch.utils.blackboard.Agent.create_handle(addr) + '.xml'
        path = ontomatch.utils.blackboard.LOCAL_BLACKBOARD_DIR + '/' + handle
        logging.info('storing graph to path=%s', path)
        graph.serialize(path, format='xml')

        handle_turtle = ontomatch.utils.blackboard.Agent.create_handle(addr) + '.ttl'
        path = ontomatch.utils.blackboard.LOCAL_BLACKBOARD_DIR + '/' + handle_turtle
        logging.info('additionally, storing graph in turtle format to path=%s', path)
        graph.serialize(path, format='turtle')

        return enriched, handle

    def load_rdflib_graph(self, addr, add_knowledge):
        frmt = 'xml'
        if addr.endswith('.ttl'):
            frmt = 'turtle'

        graph = rdflib.Graph()
        graph.parse(addr, format=frmt)
        enriched = False
        new_addr = addr
        if add_knowledge:
            logging.info('adding knowledge for %s', addr)
            enriched = self.add_knowledge_fct(graph, add_knowledge, addr)
            if enriched:
                if addr.endswith('.xml') or addr.endswith('.ttl'):
                    new_addr = addr[:-4] + '_enriched' + addr[-4:]
                else:
                    new_addr = addr + '_enriched'
            logging.info('added knowledge, enriched=%s, new_addr=%s', enriched, new_addr)
        return enriched, new_addr, graph

    def add_knowledge_fct(self, graph, agent_name, addr):

        query = '''
        SELECT DISTINCT ?pred
        WHERE {
            ?subj ?pred ?obj .
        }'''

        tokens_coord = ['coordinate', 'latitude', 'longitude', 'lat', 'long']
        found_coordinate_props = False
        result = graph.query(query)
        for row in result:
            uri = row.pred.n3().lower()

            for t in tokens_coord:
                if t in uri:
                    found_coordinate_props = True
                    break

            if  found_coordinate_props:
                break


        if found_coordinate_props:
            logging.info('no background knowledge has been added')
            return False

        logging.info('adding geographic coordinates')

        query = '''
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        SELECT DISTINCT ?subj
        WHERE {
            ?subj a owl:NamedIndividual .
        }'''

        if agent_name == 'ontomatch.knowledge.geocoding':
            geocoding_agent = ontomatch.knowledge.geocoding.Agent()
        elif agent_name == 'ontomatch.knowledge.geoNames':
            #TODO-AE URGENT 211101 configure country for geoNames
            #geocoding_agent = knowledge.geoNames.Agent(country="Germany")
            geocoding_agent = ontomatch.knowledge.geoNames.Agent(country="UnitedKingdom")
        else:
            logging.error('not found geocoding agent with name=%s', agent_name)

        geo = rdflib.Namespace('http://www.w3.org/2003/01/geo/wgs84_pos#')
        graph.bind('geo', geo )

        count_total = 0
        count_geo = 0
        for row in tqdm(graph.query(query)):
            count_total += 1
            # rdflib v5
            # address = graph.triples((row.subj, rdflib.SDO['address'], None))
            # rdflib v6.0.2
            address = graph.triples((row.subj, rdflib.term.URIRef('https://schema.org/address'), None))
            address_tmp = [obj for _, _, obj in address]
            address = address_tmp[0]

            location = None
            zipcode = None
            for _, pred, obj in graph.triples((address, None, None)):
                obj = obj.toPython()
                if 'Locality' in pred.n3():
                    location = obj
                elif 'postalCode' in pred.n3():
                    zipcode = obj

            if location or zipcode:
                if agent_name == 'ontomatch.knowledge.geocoding':
                    #latitude, longitude = geocoding_agent.query(location, zipcode)
                    latitude, longitude = geocoding_agent.query(location, None)
                else:
                    if location is None:
                        continue
                    latitude, longitude = geocoding_agent.query(location)
                if latitude and longitude:

                    latitude = rdflib.Literal(latitude, datatype=rdflib.namespace.XSD.float)
                    graph.add((row.subj, geo['lat'], latitude ))
                    longitude = rdflib.Literal(longitude, datatype=rdflib.namespace.XSD.float)
                    graph.add((row.subj, geo['long'], longitude ))
                    count_geo += 1
                else:
                    pass

        logging.info('finished adding geographic coordinates, enriched individuals=%s, total individuals=%s', count_geo, count_total)
        return True
