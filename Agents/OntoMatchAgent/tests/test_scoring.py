import logging
import os
import shutil
import time

import ontomatch.blocking
import ontomatch.scoring
import tests.utils_for_testing

class TestScoring(tests.utils_for_testing.TestCaseOntoMatch):

    def convert_to_dict(self, keys, values):
        return { k:v for k,v in zip(keys, values)}

    def convert_to_similarity_fcts(self, prop_prop_dist_tuples):
        prop_prop_sim_tuples = []
        for pos, (prop1, prop2, dist_fct) in enumerate(prop_prop_dist_tuples):
            prop_prop_sim_tuples.append((prop1, prop2, ontomatch.scoring.similarity_from_dist_fct(dist_fct), pos))
        return prop_prop_sim_tuples

    def get_params_blocking(self):
        return {
                "name": "TokenBasedPairIterator",
                "model_specific": {
                     "min_token_length": 3,
                     "max_token_occurrences_src": 20,
                     "max_token_occurrences_tgt": 20,
                     "blocking_properties": ["name", "owner"],
                    "reset_index": False,
                }
        }

    def test_add_similarity_functions(self):

        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = self.get_params_blocking()
        manager = ontomatch.scoring.create_score_manager(src_onto, tgt_onto, params_blocking)

        sim_fct =  ontomatch.scoring.similarity_from_dist_fct(ontomatch.scoring.dist_nltk_edit)

        # test case 1
        self.assertRaises(RuntimeError, manager.add_prop_prop_fct_tuples, 'name', 'some unknown property name', sim_fct)
        len_tuples = len(manager.get_prop_prop_fct_tuples())
        self.assertEqual(len_tuples, 0)

        # test case 2
        manager.add_prop_prop_fct_tuples('name', 'name', sim_fct)
        manager.add_prop_prop_fct_tuples('name', 'owner', sim_fct)
        len_tuples = len(manager.get_prop_prop_fct_tuples())
        self.assertEqual(len_tuples, 2)

    def test_configure_score_fct_nltk_edit(self):
        params_sim_fcts = [{
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed"
                }, {
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 2
                }, {
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 3
                }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)

        score = sim_fcts[0]('power station', 'power1 station2')
        self.assertEqual(score, 0)

        score = sim_fcts[1]('power station', 'power1 station2')
        self.assertEqual(score, 0)

        score = sim_fcts[2]('power station', 'power1 station2')
        self.assertAlmostEqual(score, 1/3, places=4)

        score = sim_fcts[2]('power station', 'power station')
        self.assertEqual(score, 1)

    def test_configure_score_fct_absolute(self):
        params_sim_fcts = [{
                    "name": "dist_absolute",
                    "cut_off_mode": "fixed"
                }, {
                    "name": "dist_absolute",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 10
                }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)

        score = sim_fcts[0](10.5, 10.8)
        self.assertAlmostEqual(score, 0.7, places=4)

        score = sim_fcts[0](10.5, 11.6)
        self.assertEqual(score, 0.)

        score = sim_fcts[1](10.5, 15.5)
        self.assertAlmostEqual(score, 0.5, places=4)

        score = sim_fcts[1](10.5, 25.5)
        self.assertEqual(score, 0.)

    def test_configure_score_fct_relative(self):
        params_sim_fcts = [{
                    "name": "dist_relative",
                    "cut_off_mode": "fixed"
                }, {
                    "name": "dist_relative",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 0.1
                }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)

        score = sim_fcts[0](10, 9)
        self.assertAlmostEqual(score, 0.9, places=4)

        score = sim_fcts[0](10, 9.7)
        self.assertAlmostEqual(score, 0.97, places=4)

        score = sim_fcts[0](10, 20)
        self.assertAlmostEqual(score, 0.5, places=4)

        score = sim_fcts[1](10, 9)
        self.assertAlmostEqual(score, 0., places=4)

        score = sim_fcts[1](10, 9.7)
        self.assertAlmostEqual(score, 0.7, places=4)

    def test_configure_score_fct_cosine_with_tfidf(self):
        params_sim_fcts = [{
                "name": "dist_cosine_with_tfidf",
                "cut_off_mode": "fixed",
                "maxidf": 30
            }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)

        # prepare df_index_tokens before using cosine_with_tfidf
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = self.get_params_blocking()
        ontomatch.blocking.create_iterator(src_onto, tgt_onto, params_blocking)

        df_index_tokens = ontomatch.blocking.TokenBasedPairIterator.df_index_tokens_unpruned
        logging.debug('number of index tokens=%s', len(df_index_tokens))
        logging.debug('token counts in dataset 1 and 2:')
        for t in ['altbach', 'berlin', 'power', 'station', 'kraftwerk', 'müllheizkraftwerk', 'wuppertal', 'offenbach']:
            try:
                row = df_index_tokens.loc[t]
                logging.debug('%s, %s, %s', t, row['count_1'], row['count_2'])
            except KeyError:
                logging.debug('%s Key Error', t)

        examples = [
            ('Altbach', 'Altbach'),
            ('power station', 'power station'),
            ('station', 'station'),
            ('Altbach power station', 'Altbach power station'),
            ('Berlin', 'Berlin'),
            ('Berlin power station', 'Berlin power station'),
            ('Altbach Berlin', 'Altbach'),
            ('Altbach Berlin', 'Berlin'),
            ('Altbach', 'Berlin'),
            ('Müllheizkraftwerk', 'Müllheizkraftwerk Wuppertal'),
            ('Müllheizkraftwerk', 'Müllheizkraftwerk Offenbach')
        ]

        # scores for n_max_idf = 30
        expected_similarity_scores = [1, 0, 0, 1, 1, 1, 0.956, 0.2933, 0, 0.4062, 0.5577]

        for i, (s1, s2) in enumerate(examples):
            score = sim_fcts[0](s1, s2)
            logging.debug('score for (%s, %s)=%s',s1, s2, score)
            self.assertAlmostEqual(score, expected_similarity_scores[i])

    def test_configure_score_fct_cosine_binary(self):
        params_sim_fcts = [{
                "name": "dist_cosine_binary",
                "cut_off_mode": "fixed"
            }
        ]
        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)

        examples = [
            ('Altbach', 'Altbach'),
            ('power station', 'power station'),
            ('Altbach power station Altbach', 'Altbach power station'),
            ('Berlin power station natural gas', 'Berlin gas'),
            ('Altbach Berlin', 'Altbach'),
            ('Berlin', 'Altbach Berlin'),
            ('Altbach', 'Berlin'),
            ('Müllheizkraftwerk', 'Müllheizkraftwerk Wuppertal'),
            ('Berlin Kraftwerk Müll', 'Müllheizkraftwerk Offenbach'),
            ('Berlin', 'unknown_token')
        ]

        # scores for n_max_idf = 30
        expected_similarity_scores = [1, 1, 1, 0.6325, 0.7071, 0.7071, 0., 0.7071, 0., 0.]

        for i, (s1, s2) in enumerate(examples):
            score = sim_fcts[0](s1, s2)
            logging.debug('score for (%s, %s)=%s',s1, s2, score)
            self.assertAlmostEqual(score, expected_similarity_scores[i])


    def test_calculate_between_entities_with_equal_values(self):

        c1 = ['name1', 'owner1', 'year1', 'capacity1', 'fuel1']
        v1 = ['Werkskraftwerk Sappi Alfeld', 'Sappi Alfeld GmbH', 2003, 13.5, 'coal']
        entity1 = self.convert_to_dict(c1, v1)

        c2 =  ['name2', 'owner2', 'year2', 'capacity2', 'fuel2']
        v2 = v1.copy()
        entity2 = self.convert_to_dict(c2, v2)

        prop_prop_dist_tuples = [
            ('name1', 'name2', ontomatch.scoring.dist_bounded_edit(0)),
            ('owner1', 'owner2', ontomatch.scoring.dist_nltk_edit),
            ('year1', 'year2', ontomatch.scoring.dist_absolute),
            ('capacity1', 'capacity2', ontomatch.scoring.dist_relative),
            ('fuel1', 'fuel2', ontomatch.scoring.dist_equal)
        ]

        prop_prop_sim_tuples = self.convert_to_similarity_fcts(prop_prop_dist_tuples)
        result = ontomatch.scoring.ScoreManager.calculate_between_entities(entity1, entity2,  prop_prop_sim_tuples)
        self.assertEqual(result, [1, 1, 1, 1, 1])

    def test_calculate_between_entities_with_different_values(self):

        c1 = ['name1', 'owner1', 'year1', 'capacity1', 'fuel1']
        v1 = ['Werkskraftwerk Sappi Alfeld', 'Sappi Alfeld GmbH', 2003, 10., 'coal']
        entity1 = self.convert_to_dict(c1, v1)

        c2 =  ['name2', 'owner2', 'year2', 'capacity2', 'fuel2']
        v2 =  ['12Werkskraftwerk Sappi Alfeld', 'Sappi1 Alfeld2 GmbH3', 2013, 8., 'wind']
        entity2 = self.convert_to_dict(c2, v2)

        prop_prop_dist_tuples = [
            ('name1', 'name2', ontomatch.scoring.dist_bounded_edit(2)),
            ('owner1', 'owner2', ontomatch.scoring.dist_nltk_edit),
            ('year1', 'year2', ontomatch.scoring.dist_absolute),
            ('capacity1', 'capacity2', ontomatch.scoring.dist_relative),
            ('fuel1', 'fuel2', ontomatch.scoring.dist_equal)
        ]

        prop_prop_sim_tuples = self.convert_to_similarity_fcts(prop_prop_dist_tuples)
        result = ontomatch.scoring.ScoreManager.calculate_between_entities(entity1, entity2, prop_prop_sim_tuples)
        expected = [1/3, 0, 0, 8/10, 0]
        for i, actual in enumerate(result):
            self.assertAlmostEqual(actual, expected[i], places=4)

    def test_calculate_similarities_between_datasets(self):

        prop_prop_dist_tuples = [
            ('name', 'name', ontomatch.scoring.dist_bounded_edit(2)),
            ('owner', 'owner', ontomatch.scoring.dist_nltk_edit),
            ('year', 'year', ontomatch.scoring.dist_absolute),
            ('capacity', 'capacity', ontomatch.scoring.dist_relative),
            ('fuel', 'fuel', ontomatch.scoring.dist_equal),
        ]

        prop_prop_sim_tuples = self.convert_to_similarity_fcts(prop_prop_dist_tuples)

        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = self.get_params_blocking()
        manager = ontomatch.scoring.create_score_manager(src_onto, tgt_onto, params_blocking)
        for prop1, prop2, sim_fct, pos in prop_prop_sim_tuples:
            manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct, pos)

        df_scores = manager.calculate_similarities_between_datasets()
        self.assertEqual(len(df_scores), 4719)

    def test_calculate_maximum_scores_and_assert_means(self):

        #prepare

        score_fct_1 = ontomatch.scoring.similarity_from_dist_fct(ontomatch.scoring.dist_bounded_edit(2))
        score_fct_2 = ontomatch.scoring.similarity_from_dist_fct(ontomatch.scoring.dist_nltk_edit, cut_off_value=10)
        score_fct_3 = ontomatch.scoring.similarity_from_dist_fct(ontomatch.scoring.dist_absolute, cut_off_value=10)
        score_fct_4 = ontomatch.scoring.similarity_from_dist_fct(ontomatch.scoring.dist_relative)

        prop_prop_sim_tuples = [
            ('name', 'name', score_fct_1),
            ('name', 'owner', score_fct_1),
            ('owner', 'owner', score_fct_2),
            ('owner', 'name', score_fct_2),
            ('year', 'year', score_fct_3),
            ('year', 'capacity', score_fct_3),
            ('capacity', 'capacity', score_fct_4),
            ('capacity', 'year', score_fct_4),
        ]

        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = self.get_params_blocking()
        manager = ontomatch.scoring.create_score_manager(src_onto, tgt_onto, params_blocking)
        for prop1, prop2, sim_fct in prop_prop_sim_tuples:
            manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)

        # run

        manager.calculate_similarities_between_datasets()
        manager.calculate_maximum_scores()
        df_max_scores_1 = manager.get_max_scores_1()

        # assert

        logging.info('\nmax scores for dataset 1:\n------------------')
        logging.info('\n%s', df_max_scores_1)
        logging.info('\n%s', df_max_scores_1.describe())

        self.assertEqual(len(df_max_scores_1), 1144)

        means = df_max_scores_1.describe().loc['mean']
        logging.info('means=\n%s', means)
        self.assertGreater(means[0], means[1])
        self.assertGreater(means[2], means[3])
        self.assertGreater(means[4], means[5])
        self.assertGreater(means[6], means[7])

    def test_property_mapping_props_name_owner(self):

        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_WEIGHT_CSV)
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = params['blocking']
        params_sim_fcts = params['mapping']['similarity_functions']

        manager = ontomatch.scoring.create_score_manager(src_onto, tgt_onto, params_blocking)

        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)
        props1 = ['name', 'owner']
        props2 = ['name', 'owner']
        property_mapping = ontomatch.scoring.find_property_mapping(manager, sim_fcts, props1, props2)

        logging.debug('property_mapping=%s', property_mapping)

        expected = {
            'name': ('name', 0.79400, 4),  # ('name', 0.87142, 4),
            'owner': ('owner', 0.75928, 5) # ('owner', 0.72531, 5)
        }

        self.assertEqual(len(property_mapping), len(expected))
        for mapping in property_mapping:
            prop1 = mapping['prop1']
            prop2, mean, pos_sfct = expected[prop1]
            self.assertEqual(mapping['prop2'], prop2)
            self.assertAlmostEqual(mapping['mean'], mean, places=2)
            self.assertEqual(mapping['pos_sfct'], pos_sfct)

    def test_property_mapping_props_name_fuel(self):

        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_WEIGHT_CSV)
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = params['blocking']
        params_sim_fcts = params['mapping']['similarity_functions']

        manager = ontomatch.scoring.create_score_manager(src_onto, tgt_onto, params_blocking)

        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)
        props1 = ['name', 'fuel']
        props2 = ['name', 'fuel']
        property_mapping = ontomatch.scoring.find_property_mapping(manager, sim_fcts, props1, props2)

        logging.debug('property_mapping=%s', property_mapping)

        expected = {
            'name': ('name', 0.79400, 4),   # ('name',  0.87142, 4),
            # score function 0 and 1 got the same mean result, thus the first score function wins
            'fuel': ('fuel', 0.85751, 0)    # ('fuel', 0.86066, 0)
        }

        self.assertEqual(len(property_mapping), len(expected))
        for mapping in property_mapping:
            prop1 = mapping['prop1']
            prop2, mean, pos_sfct = expected[prop1]
            self.assertEqual(mapping['prop2'], prop2)
            self.assertAlmostEqual(mapping['mean'], mean, places=2)
            self.assertEqual(mapping['pos_sfct'], pos_sfct)

    def xxx_test_property_mapping_5_props_5_score_fcts(self):

        params_sim_fcts = [{
                    "name": "dist_equal",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 3
                },{
                    "name": "dist_absolute",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 10
                },{
                    "name": "dist_relative",
                    "cut_off_mode": "fixed"
                },{
                    "name": "dist_cosine_with_tfidf",
                    "cut_off_mode": "fixed"
            }
        ]

        #src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = self.get_params_blocking()
        manager = ontomatch.scoring.create_score_manager(src_onto, tgt_onto, params_blocking)

        sim_fcts = ontomatch.scoring.create_similarity_functions_from_params(params_sim_fcts)
        property_mapping = ontomatch.scoring.find_property_mapping(manager, sim_fcts)

        expected = {
            'name' : ('name', 0.79721, 4),
            'hasYearOfBuilt/hasValue/numericalValue':  ('hasYearOfBuilt/hasValue/numericalValue', 0.99758, 3),
            'designCapacity/hasValue/numericalValue': ('designCapacity/hasValue/numericalValue', 0.70326, 3),
            'isOwnedBy/hasName': ('isOwnedBy/hasName', 0.64090, 4),
            'realizes/consumesPrimaryFuel': ('realizes/consumesPrimaryFuel', 0.85253, 1),
            #both fuel and type have been identified for property mapping
            'type': ('type', 0.87609, 0),
            #'iri': ('iri', 0.79721, 4),
            'iri': ('iri', 0.62338, 4),
            'http://www.w3.org/2000/01/rdf-schema#label': ('http://www.w3.org/2000/01/rdf-schema#label', 0.79595, 4)
            #'geo:wgs84_pos#long': ('hasGISCoordinateSystem/hasProjectedCoordinate_x/hasValue/numericalValue', 0.9708340665506993, 3),
            #'geo:wgs84_pos#lat': ('hasGISCoordinateSystem/hasProjectedCoordinate_y/hasValue/numericalValue', 0.9960711009327202, 3),
        }

        self.assertEqual(len(property_mapping), len(expected))
        for mapping in property_mapping:
            logging.debug('mapping=%s', mapping)
            prop1 = mapping['prop1']
            prop2, mean, pos_sfct = expected[prop1]
            self.assertEqual(mapping['prop2'], prop2)
            self.assertAlmostEqual(mapping['mean'], mean, places=2)
            self.assertEqual(mapping['pos_sfct'], pos_sfct)

    def test_scoringweightiterator(self):

        it = ontomatch.scoring.ScoringWeightIterator(3, 1)
        logging.debug(it.all_weight_arrays)
        self.assertEqual(len(it), 3)

        it = ontomatch.scoring.ScoringWeightIterator(10, 1)
        logging.debug(it.all_weight_arrays)
        self.assertEqual(len(it), 10)

        it = ontomatch.scoring.ScoringWeightIterator(3, 2)
        logging.debug(it.all_weight_arrays)
        self.assertEqual(len(it), 6)

        it = ontomatch.scoring.ScoringWeightIterator(3, 3)
        logging.debug(it.all_weight_arrays)
        self.assertEqual(len(it), 10)

        it = ontomatch.scoring.ScoringWeightIterator(4, 3)
        logging.debug(it.all_weight_arrays)
        self.assertEqual(len(it), 20)

    def test_scoringweightiterator_with_sample_count(self):
        it = ontomatch.scoring.ScoringWeightIterator(10, 1, sample_count=4)
        logging.debug(it.all_weight_arrays)
        self.assertEqual(len(it), 4)

    def test_create_prop_prop_sim_triples_from_params(self):
        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_WEIGHT_CSV)
        params_mapping = params['mapping']
        triples = ontomatch.scoring.create_prop_prop_sim_triples_from_params(params_mapping)
        self.assertEqual(len(triples), 5)
        self.assertEqual(triples[0][0], 'name')
        self.assertEqual(triples[2][1], 'year')

    def test_calculate_similarities_between_datasets_load_store(self):

        if not os.path.exists('../tmp/tests'):
            os.makedirs('../tmp/tests')
        sim_file = '../tmp/tests/scores_auto_10.csv'
        shutil.copy('./tests/data/scores_auto_10.csv', sim_file)

        params = self.read_params(tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO_CSV)
        src_onto, tgt_onto = self.read_kwl_gppd_tables()
        params_blocking = params['blocking']
        params_mapping = params['mapping']
        params_mapping['similarity_file'] = sim_file
        manager = ontomatch.scoring.create_score_manager(src_onto, tgt_onto, params_blocking, params_mapping)
        manager.add_prop_prop_fct_tuples_by_params(params_mapping)

        start_time = time.time()
        df_combined = manager.calculate_similarities_between_datasets()
        self.assertEqual(len(df_combined), len(manager.pair_iterator))
        manager.calculate_maximum_scores()

        logging.debug('elapsed time=%s', time.time() - start_time)