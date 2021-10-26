import logging
import pickle
import traceback

import numpy as np
import pandas as pd
from owlready2 import get_ontology, prop
import rdflib
import rdflib.namespace
from tqdm import tqdm

from alignment import Alignment
import knowledge.geocoding
import knowledge.geoNames
from matchManager import matchManager
from ontologyWrapper import Ontology
import scoring


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
            onto = Ontology(addr, ontology=owlready2onto, graph=graph)
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
            geocoding_agent = knowledge.geoNames.Agent()
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
                #latitude, longitude = geocoding_agent.query(location, zipcode)
                latitude, longitude = geocoding_agent.query(location, None)
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
            params_mapping = params['mapping']
            matching_name = params['matching']['name']
            params_model_specific = params['matching']['model_specific']


            if matching_name == 'matchManager.matchManager':
                self.__start_match_manager(params_model_specific, params_blocking, srconto, tgtonto)
            elif matching_name == 'coordinator.InstanceMatcherWithAutoCalibration':
                self.__start_matching_with_auto_calibration(srconto, tgtonto, params_blocking, params_mapping)
            else:
                raise RuntimeError('unknown matcher', matching_name)

        except:
            full_traceback = traceback.format_exc()
            print(full_traceback)
            logging.fatal(full_traceback)
            raise

    def __start_matching_with_auto_calibration(self, srconto, tgtonto, params_blocking, params_mapping):
        matcher = InstanceMatcherWithAutoCalibrationAgent()
        #TODO-AE URGENT 211023 Must be continued ... matcher has to write back matching results ...
        matcher.start(srconto, tgtonto, params_blocking, params_mapping)

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

        alignment = match_manager.runMatch("matchWrite2Matrix", to1=False, rematch=False, params_blocking=params_blocking)
        #match_manager.showResult(match_manager.A,'individualList')
        match_manager.renderResult(" http://dbpedia.org/resource", "http://www.theworldavatar.com", '2109xx.owl', True)


#TODO-AE move instance matcher to another module
class InstanceMatcherWithAutoCalibrationAgent():

    def __init__(self):
        self.score_manager = None

    def start(self, srconto, tgtonto, params_blocking, params_mapping=None, prop_prop_sim_tuples=None):

        self.score_manager = scoring.create_score_manager(srconto, tgtonto, params_blocking)

        # TODO-AE: We start with automatic property mapping
        # --> configurable, also: fixed property mapping (e.g. geo coordinates)
        # TODO-AE: symmetrical mapping (with max value for entities of dataset 2)

        if params_mapping:
            mode = params_mapping['mode']
            logging.info('starting InstanceMatcherWithAutoCalibrationAgent with mode=%s', mode)

            if mode == 'auto':
                params_sim_fcts = params_mapping['similarity_functions']
                sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)
                property_mapping = scoring.find_property_mapping(self.score_manager, sim_fcts)
            else:
                raise RuntimeError('unknown mode', mode)

        else:
            logging.info('starting InstanceMatcherWithAutoCalibrationAgent with prop_prop_sim_tumples=%s', len(prop_prop_sim_tuples))
            property_mapping = []
            for pos, t in enumerate(prop_prop_sim_tuples):
                prop1, prop2, sim_fct = t
                self.score_manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)
                row = {
                    'key': str(pos) + '_max',
                    'prop1': prop1,
                    'prop2': prop2,
                    'score_fct': sim_fct
                }
                property_mapping.append(row)
            logging.info('added prop prop sim tuples, number=%s', len(self.score_manager.get_prop_prop_fct_tuples()))

            self.score_manager.calculate_similarities_between_datasets()
            self.score_manager.calculate_maximum_scores()

        df_scores = self.score_manager.get_scores()
        #TODO-AE asymmetry
        df_max_scores = self.score_manager.get_max_scores_1()
        df_total_scores, df_total_best_scores = self.calculate_auto_calibrated_total_scores(df_scores, df_max_scores, property_mapping)
        return df_total_scores, df_total_best_scores

    def calculate_auto_calibrated_total_scores_for_index(self, df_scores, df_max_scores, property_mapping, idx_1, skip_column_number = 1):
        best_score = 0
        best_pos = None
        total_score_rows = []

        for pos, (idx_2, row) in enumerate(df_scores.loc[idx_1].iterrows()):
            score = 0
            number_columns = len(property_mapping)
            prop_score = {}
            for propmap  in property_mapping:

                c_max = propmap['key']
                c = int(c_max.split('_')[0])
                value = row[c]

                #TODO-AE changed at 210926
                #if (not value is None) and (type(value) is float and not np.isnan(value)):
                if not (value is None or type(value) is str or np.isnan(value)):
                    # TODO-AE check: <= in line 1 and 3 leads to worse results than <
                    # TODO-AE experimental idea: problem with just a few 'discrete values' (e.g. 0 and 1 for match and mismatch fuel, or 0, 1, 2 edit distance)
                    # is: matches are penalized when using <= (around 0) but when using < instead nonmatches benefit (around 1)
                    # idea: use <= around 0 and < around 1 and "interpolate" in between
                    # this idea should not have much effect if there are many 'discrete values' and there is no lumping on values around 0
                    mask = (df_scores[c] > value)
                    count_m_plus_n = len(df_scores[mask])
                    mask = (df_max_scores[c_max] > value)
                    count_m = len(df_max_scores[mask])
                    if count_m_plus_n == 0:
                        column_score = 1
                    else:

                        #TODO-AE URGENT 211022
                        column_score = count_m / count_m_plus_n

                        '''
                        mask = (df_scores[c] == value)
                        count_m_plus_n_equal = len(df_scores[mask])
                        mask = (df_scores[c] < value)
                        count_m_plus_n_greater = len(df_scores[mask])
                        if count_m_plus_n_equal == 1:
                            denom = count_m_plus_n
                        else:
                            denom = count_m_plus_n + count_m_plus_n_equal * (count_m_plus_n / (count_m_plus_n + count_m_plus_n_greater))


                        mask = (df_max_scores[c_max] == value)
                        count_m_equal = len(df_max_scores[mask])
                        mask = (df_max_scores[c_max] < value)
                        count_m_greater = len(df_max_scores[mask])
                        if count_m_equal == 1:
                            nom = count_m
                        else:
                            #TODO-AE: better (count_m_equal - 1)
                            nom = count_m + count_m_equal * (count_m / (count_m + count_m_greater))
                        column_score = nom / denom


                        column_score = count_m / denom
                        '''
                    '''
                    if log:
                        if count_m_plus_n == 0:
                            print(c, value, 'ZERO', column_score)
                        else:
                            print(c, value, count_m, count_m_plus_n, 'orginal score=', count_m / count_m_plus_n, 'new score=', column_score)
                            print('\t', count_m, count_m_equal, count_m_greater, 'nom=', nom)
                            print('\t', count_m_plus_n, count_m_plus_n_equal, count_m_plus_n_greater, 'denom=', denom)
                    '''
                    score += column_score


                    #TODO-AE 211015 calibrated score for each prop
                    prop_score.update({c: column_score})


                else:
                    #TODO-AE how to score missing data?
                    number_columns = number_columns - 1

            if number_columns <= skip_column_number:
                score = 0.
                print('score = 0 since number columns=', number_columns, idx_1, idx_2)
            else:
                score = score / number_columns

            total_score_row = {
                'idx_1': idx_1,
                'idx_2': idx_2,
                'score': score,
                'best': False,
                'pos_1': row['pos_1'],
                'pos_2': row['pos_2'],
            }

            total_score_row.update(prop_score)

            total_score_rows.append(total_score_row)

            # TODO-AE what about equality?
            if score > best_score or best_pos is None:
                best_score = score
                best_pos = pos

        total_score_rows[best_pos]['best'] = True

        return total_score_rows

    def calculate_auto_calibrated_total_scores(self, df_scores, df_max_scores, property_mapping):

        logging.info('calculating auto calibrated total scores')

        df_scores['score'] = 0.

        rows = []
        for idx_1, _ in tqdm(df_max_scores.iterrows()):
            #TODO-AE URGENT
            skip_column_number = 1
            total_score_rows = self.calculate_auto_calibrated_total_scores_for_index(df_scores, df_max_scores, property_mapping, idx_1, skip_column_number = skip_column_number)
            rows.extend(total_score_rows)

        df_total_scores = pd.DataFrame(rows)
        df_total_scores.set_index(['idx_1', 'idx_2'], inplace=True)
        mask = (df_total_scores['best'] == True)
        df_total_best_scores = df_total_scores[mask]

        logging.info('calculated auto calibrated total scores')

        return df_total_scores, df_total_best_scores