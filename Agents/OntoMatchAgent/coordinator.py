import logging
import pickle
import traceback

from owlready2 import get_ontology
import rdflib
import rdflib.namespace
from tqdm import tqdm

import knowledge.geocoding
from matchManager import matchManager
from ontologyWrapper import Ontology


class Agent():

    def load(self, srcaddr, tgtaddr, add_knowledge=False, dump_ontology=False):

        logging.info('loading ontology for %s', srcaddr)
        if srcaddr.endswith('.pkl'):
            with open(srcaddr,'rb') as file:
                srconto = pickle.load(file)
        else:
            srcgraph = self.load_rdflib_graph(srcaddr, add_knowledge)
            srcowlready2onto = self.load_owlready2_ontology(srcgraph)
            srconto = Ontology(srcaddr, ontology=srcowlready2onto, graph=srcgraph)
            if dump_ontology:
                self.dump(srcaddr, srconto)

        logging.info('finished loading ontology for %s', srcaddr)

        logging.info('loading ontology for %s', tgtaddr)

        if tgtaddr.endswith('.pkl'):
            with open(tgtaddr,'rb') as file:
                tgtonto = pickle.load(file)
        else:
            tgtgraph = self.load_rdflib_graph(tgtaddr, add_knowledge)
            tgtowlready2onto = self.load_owlready2_ontology(tgtgraph)
            tgtonto = Ontology(tgtaddr, ontology=tgtowlready2onto, graph=tgtgraph)
            if dump_ontology:
                self.dump(tgtaddr, tgtonto)

        logging.info('finished loading ontology for %s', tgtaddr)

        return srconto, tgtonto

    def load_owlready2_ontology(self, graph):
        # TODO-AE This is a hack to convert the rdflib graph into owlready2
        # unfortunately, owlready2 only allows an URL or file name as parameter for loading, no stream parameter
        tmp_file='../logs/tmp_rdflib_onto.owl'
        graph.serialize(tmp_file, format="xml")
        onto = get_ontology(tmp_file).load()
        return onto

    def load_rdflib_graph(self, addr, add_knowledge=False):

        frmt = 'xml'
        if addr.endswith('.ttl'):
            frmt = 'turtle'

        graph = rdflib.Graph()
        graph.parse(addr, format=frmt)
        if add_knowledge:
            logging.info('adding knowledge for %s', addr)
            self.add_knowledge_fct(graph)
            logging.info('finished adding knowledge for %s', addr)
        return graph

    def dump(self, addr, onto):
        onto.ontology = None
        onto.graph = None
        pklname = addr.replace('rdf','pkl').replace('owl','pkl').replace('xml','pkl')
        logging.info('dumping ontology to file=%s', pklname)
        with open(pklname,'wb') as file:
            pickle.dump(onto, file, -1)

    def add_knowledge_fct(self, graph):

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
            return

        logging.info('adding geographic coordinates')

        query = '''
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        SELECT DISTINCT ?subj
        WHERE {
            ?subj a owl:NamedIndividual .
        }'''

        geocoding_agent = knowledge.geocoding.Agent()
        geo = rdflib.Namespace('http://www.w3.org/2003/01/geo/wgs84_pos#')
        graph.bind('geo', geo )

        count_geo = 0
        for row in tqdm(graph.query(query)):
            #print(row.subj.n3())
            address = graph.triples((row.subj, rdflib.SDO['address'], None))
            address = [obj for _, _, obj in address][0]
            #print(address, type(address))

            location = None
            zipcode = None
            for _, pred, obj in graph.triples((address, None, None)):
                #print(pred, obj)
                obj = obj.toPython()
                if 'Locality' in pred.n3():
                    location = obj
                elif 'postalCode' in pred.n3():
                    zipcode = obj

            if location or zipcode:
                latitude, longitude = geocoding_agent.query(location, zipcode)
                #print('coord=', lat, long)
                if latitude and longitude:

                    latitude = rdflib.Literal(latitude, datatype=rdflib.namespace.XSD.float)
                    graph.add((row.subj, geo['lat'], latitude ))
                    longitude = rdflib.Literal(longitude, datatype=rdflib.namespace.XSD.float)
                    graph.add((row.subj, geo['long'], longitude ))
                    count_geo += 1
                else:
                    #print('no coordinates found for ', row.subj.n3())
                    pass

        logging.info('finished adding geographic coordinates, enhanced individuals=%s', count_geo)

    def start(self, params, penalize):

        try:
            params_for_loading = {
                'srcaddr': params['dataset']['src'],
                'tgtaddr': params['dataset']['tgt'],
                'add_knowledge': params['pre_processing']['add_knowledge'],
                'dump_ontology': params['pre_processing']['pickle_dump'],
            }
            #srconto, tgtonto = agent.load(srcaddr, tgtaddr, add_knowledge=True, dump_ontology=True)
            srconto, tgtonto = self.load(**params_for_loading)

            params_model_specific = params['matching']['model_specific']
            match_steps  = params_model_specific['steps']
            match_weights = params_model_specific['weights']
            additional_match_params = params_model_specific['params']
            threshold = params_model_specific['threshold']

            params_blocking = params['blocking']

            # TODO-AE move more config params to dictionary / config file
            match_manager = matchManager(match_steps, srconto, tgtonto, thre=threshold,
                    weight=match_weights, paras=additional_match_params,
                    matchIndividuals=True, penalize=penalize, useAttrFinder=False)

            alignment = match_manager.runMatch("matchWrite2Matrix", to1=False, rematch=False, params_blocking=params_blocking)
            #match_manager.showResult(match_manager.A,'individualList')
            match_manager.renderResult(" http://dbpedia.org/resource", "http://www.theworldavatar.com", '2109xx.owl', True)
        except:
            full_traceback = traceback.format_exc()
            print(full_traceback)
            logging.fatal(full_traceback)
            raise