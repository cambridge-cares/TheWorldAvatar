import argparse
import collections
import json
import logging
import os
import pickle
import random
import sys
import time
import traceback


import numpy as np
import pandas as pd
from owlready2 import get_ontology, prop
import rdflib
import rdflib.namespace
from tqdm import tqdm

from alignment import Alignment
import evaluate
import instancematching
import knowledge.geocoding
import knowledge.geoNames
from matchManager import matchManager
from ontologyWrapper import Ontology
import scoring
import util


class Agent():

    '''
    def load(self, srcaddr, tgtaddr, add_knowledge=None, dump_ontology=False):

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
    '''

    def load(self, srcaddr, tgtaddr, add_knowledge=None, dump_ontology=False):
        srconto = self.load_ontology(srcaddr, add_knowledge, dump_ontology)
        tgtonto = self.load_ontology(tgtaddr, add_knowledge, dump_ontology)
        return srconto, tgtonto

    def load_ontology(self, addr, add_knowledge=None, dump_ontology=False):
        logging.info('loading ontology for %s', addr)
        if addr.endswith('.pkl'):
            with open(addr,'rb') as file:
                onto = pickle.load(file)
        else:
            graph = self.load_rdflib_graph(addr, add_knowledge)
            owlready2onto = self.load_owlready2_ontology(graph)
            onto = Ontology(addr, ontology=owlready2onto, graph=graph, skip_labels=False)
            if dump_ontology:
                self.dump(addr, onto)

        logging.info('finished loading ontology for %s', addr)
        return onto

    def load_owlready2_ontology(self, graph):
        # TODO-AE This is a hack to convert the rdflib graph into owlready2
        # unfortunately, owlready2 only allows an URL or file name as parameter for loading, no stream parameter
        tmp_file='../logs/tmp_rdflib_onto.owl'
        graph.serialize(tmp_file, format="xml")
        onto = get_ontology(tmp_file).load()
        return onto

    def load_rdflib_graph(self, addr, add_knowledge=None):

        frmt = 'xml'
        if addr.endswith('.ttl'):
            frmt = 'turtle'

        graph = rdflib.Graph()
        graph.parse(addr, format=frmt)
        if add_knowledge:
            logging.info('adding knowledge for %s', addr)
            self.add_knowledge_fct(graph, add_knowledge)
            logging.info('finished adding knowledge for %s', addr)
        return graph

    def dump(self, addr, onto):
        onto.ontology = None
        onto.graph = None
        pklname = addr.replace('rdf','pkl').replace('owl','pkl').replace('xml','pkl').replace('ttl', 'pkl')
        logging.info('dumping ontology to file=%s', pklname)
        with open(pklname,'wb') as file:
            pickle.dump(onto, file, -1)

    def add_knowledge_fct(self, graph, agent_name):

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

        if agent_name == 'knowledge.geocoding':
            geocoding_agent = knowledge.geocoding.Agent()
        elif agent_name == 'knowledge.geoNames':
            #TODO-AE URGENT 211101 configure country for geoNames
            #geocoding_agent = knowledge.geoNames.Agent(country="Germany")
            geocoding_agent = knowledge.geoNames.Agent(country="UnitedKingdom")
        else:
            logging.error('not found geocoding agent with name=%s', agent_name)

        geo = rdflib.Namespace('http://www.w3.org/2003/01/geo/wgs84_pos#')
        graph.bind('geo', geo )

        count_total = 0
        count_geo = 0
        for row in tqdm(graph.query(query)):
            count_total += 1
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
                if agent_name == 'knowledge.geocoding':
                    #latitude, longitude = geocoding_agent.query(location, zipcode)
                    latitude, longitude = geocoding_agent.query(location, None)
                else:
                    if location is None:
                        continue
                    latitude, longitude = geocoding_agent.query(location)
                #print('coord=', latitude, longitude)
                if latitude and longitude:

                    latitude = rdflib.Literal(latitude, datatype=rdflib.namespace.XSD.float)
                    graph.add((row.subj, geo['lat'], latitude ))
                    longitude = rdflib.Literal(longitude, datatype=rdflib.namespace.XSD.float)
                    graph.add((row.subj, geo['long'], longitude ))
                    count_geo += 1
                else:
                    #print('no coordinates found for ', row.subj.n3())
                    pass

        logging.info('finished adding geographic coordinates, enhanced individuals=%s, total individuals=%s', count_geo, count_total)

    def start(self, params):

        try:
            params_for_loading = {
                'srcaddr': params['dataset']['src'],
                'tgtaddr': params['dataset']['tgt'],
                'add_knowledge': params['pre_processing']['add_knowledge'],
                'dump_ontology': params['pre_processing']['pickle_dump'],
            }

            srconto, tgtonto = self.load(**params_for_loading)

            params_blocking = params['blocking']
            matching_name = params['matching']['name']
            params_model_specific = params['matching']['model_specific']

            if matching_name == 'matchManager.matchManager':
                return self.__start_match_manager(params_model_specific, params_blocking, srconto, tgtonto)
            elif matching_name == 'instancematching.InstanceMatcherWithAutoCalibration':
                params_mapping = params['mapping']
                return self.__start_matching_with_auto_calibration(srconto, tgtonto, params_blocking, params_mapping)
            elif matching_name == 'instancematching.InstanceMatcherMaxSVC':
                params_mapping = params['mapping']
                classif_name = params['classification']['name']
                if  classif_name != 'SVC':
                    raise ValueError('unknown classification method', classif_name)
                params_classification = params['classification']['model_specific']
                return self.__start_matching_max_svc(srconto, tgtonto, params_blocking, params_mapping, params_classification)
            elif matching_name == 'instancematching.InstanceMatcherWithScoringWeights':
                params_mapping = params['mapping']
                return self.__start_matching_with_scoring_weights(srconto, tgtonto, params_model_specific, params_blocking, params_mapping)
            else:
                raise RuntimeError('unknown matcher', matching_name)

        except:
            logging.fatal('finished with exception')
            full_traceback = traceback.format_exc()
            print(full_traceback)
            logging.fatal(full_traceback)
            raise

    def __start_matching_with_auto_calibration(self, srconto, tgtonto, params_blocking, params_mapping):
        matcher = instancematching.InstanceMatcherWithAutoCalibration()
        #TODO-AE URGENT 211023 Must be continued ... all matchers has to write back matching results ...
        _, df_total_best_scores = matcher.start(srconto, tgtonto, params_blocking, params_mapping)
        #return df_total_best_scores
        #TODO-AE URGENT 211103
        return matcher

    def __start_matching_max_svc(self, srconto, tgtonto, params_blocking, params_mapping, params_classification):
        matcher = instancematching.InstanceMatcherMaxSVC()
        #TODO-AE URGENT 211023 Must be continued ... all matchers has to write back matching results ...
        matcher.start(srconto, tgtonto, params_blocking, params_mapping, params_classification)
        return matcher

    def __start_matching_with_scoring_weights(self, srconto, tgtonto, params_model_specific, params_blocking, params_mapping):
        matcher = instancematching.InstanceMatcherWithScoringWeights()
        df_scores = matcher.start(srconto, tgtonto, params_blocking, params_mapping, None)
        prop_prop_sim_triples = scoring.create_prop_prop_sim_triples_from_params(params_mapping)
        prop_column_names = [ c for c in range(len(prop_prop_sim_triples)) ]
        scoring_weights = params_model_specific['weights']
        instancematching.add_total_scores(df_scores, props=prop_column_names, scoring_weights=scoring_weights,
                        missing_score=None, aggregation_mode='sum', average_min_prop_count=2)
        #return df_scores
        return matcher

    def __start_match_manager(self, params_model_specific, params_blocking, srconto, tgtonto):

        match_steps  = params_model_specific['steps']
        match_weights = params_model_specific['weights']
        additional_match_params = params_model_specific['params']
        threshold = params_model_specific['threshold']

        # TODO-AE Do we need the penalize object for instance matching?
        '''
        clist = [('PowerStation', 'PowerPlant', 0.9)]
        sublist = ['RenewablePlant', 'FossilFuelPlant', 'HydroelectricPlant', 'HydrogenPlant', 'NuclearPlant', 'CogenerationPlant', 'GeothermalPlant', 'MarinePlant', 'BiomassPlant', 'WindPlant', 'SolarPlant','WastePlant','PowerPlant']
        for subc in sublist:
            #for subc in sublist:
            clist.append((subc,subc,0.9))
        '''
        clist = []
        sublist = ['PowerStation', 'PowerPlant', 'RenewablePlant', 'FossilFuelPlant', 'HydroelectricPlant', 'HydrogenPlant', 'NuclearPlant', 'CogenerationPlant', 'GeothermalPlant', 'MarinePlant', 'BiomassPlant', 'WindPlant', 'SolarPlant','WastePlant']
        for subc in sublist:
            for subc2 in sublist:
                #for subc in sublist:
                clist.append((subc,subc2,0.9))

        penalize = {'class':True,'align':Alignment(clist)}


        # TODO-AE move more config params to dictionary / config file
        # TODO-AE configuration of useAttrFinder
        match_manager = matchManager(match_steps, srconto, tgtonto, thre=threshold,
                weight=match_weights, paras=additional_match_params,
                matchIndividuals=True, penalize=penalize, useAttrFinder=False)

        alignment, df_scores = match_manager.runMatch("matchWrite2Matrix", to1=False, rematch=False, params_blocking=params_blocking)
        #match_manager.showResult(match_manager.A,'individualList')
        #match_manager.renderResult(" http://dbpedia.org/resource", "http://www.theworldavatar.com", '2109xx.owl', True)

        # convert alignment to dataframe with indices and score function
        return df_scores

'''
def init(config_dev=None):
    print('current working directory=', os.getcwd())
    print('sys.argv=', sys.argv)

    parser = argparse.ArgumentParser()
    parser.add_argument('--config', type=str)
    parser.add_argument('--logconfdir', type=str, default='./conf')
    parser.add_argument('--logdir', type=str, default='../logs')
    args = parser.parse_args()
    print('args = ', args)

    if args.config:
        with open(args.config) as json_config:
            config = json.load(json_config, object_pairs_hook=collections.OrderedDict)
    else:
        config = config_dev

    util.init_logging(args.logconfdir, args.logdir)
    logging.info('current working directory=%s', os.getcwd())
    logging.info('args=%s', args)
    logging.info('config=%s', config)

    seed = config['numerical_settings'].get('seed')
    logging.info('setting random seed=%s', seed)
    np.random.seed(seed)
    random.seed(seed)

    return config
'''

def postprocess(config, matcher, dump=None):

    if dump:
        dir_name = dump + '_' + str(time.time())
        logging.info('dumping results to %s', dir_name)
        os.mkdir(dir_name)
        sm = matcher.score_manager
        sm.data1.to_csv(dir_name + '/data1.csv')
        sm.data2.to_csv(dir_name + '/data2.csv')

        if isinstance(matcher, instancematching.InstanceMatcherWithScoringWeights) or isinstance(matcher, instancematching.InstanceMatcherMaxSVC):
            sm.df_scores.to_csv(dir_name + '/total_scores.csv')
        elif isinstance(matcher, instancematching.InstanceMatcherWithAutoCalibration):
            if sm.df_max_scores_1 is not None:
                sm.df_max_scores_1.to_csv(dir_name + '/max_scores_1.csv')
            if sm.df_max_scores_2 is not None:
                sm.df_max_scores_2.to_csv(dir_name + '/max_scores_2.csv')
            matcher.df_total_scores.to_csv(dir_name + '/total_scores.csv')
            matcher.df_total_best_scores.to_csv(dir_name + '/total_best_scores.csv')
            sm.df_scores.to_csv(dir_name + '/scores.csv')
        else:
            raise RuntimeError('unsupported type of matcher', type(matcher))

    matchfile = config['post_processing']['evaluation_file']
    index_set_matches = evaluate.read_match_file_as_index_set(matchfile, linktypes = [1, 2, 3, 4, 5])
    logging.info('ground truth matches=%s', len(index_set_matches))

    df_scores = matcher.get_scores()
    logging.info('length of scores=%s', len(df_scores))
    result = evaluate.evaluate(df_scores, index_set_matches)
    return result

def start(config_dev=None):
    config = util.init(config_dev)
    starttime = time.time()
    agent = Agent()
    matcher = agent.start(config)
    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)
    dump = config['post_processing'].get('dump')
    result = postprocess(config, matcher, dump)
    return matcher, result

if __name__ == '__main__':
    start()
