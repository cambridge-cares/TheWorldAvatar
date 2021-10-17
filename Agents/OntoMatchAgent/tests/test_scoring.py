import logging

import blocking
import scoring
import utils_for_testing

class TestScoring(utils_for_testing.TestCaseOntoMatch):

    def convert_to_dict(self, keys, values):
        return { k:v for k,v in zip(keys, values)}

    def convert_to_similarity_fcts(self, prop_prop_dist_tuples):
        prop_prop_sim_tuples = []
        for prop1, prop2, dist_fct in prop_prop_dist_tuples:
            prop_prop_sim_tuples.append((prop1, prop2, scoring.similarity_from_dist_fct(dist_fct)))
        return prop_prop_sim_tuples

    def get_params_blocking(self):
        return {
                "name": "TokenBasedPairIterator",
                "model_specific": {
                     "min_token_length": 3,
                     "max_token_occurrences_src": 20,
                     "max_token_occurrences_tgt": 20,
                     "blocking_properties": ["name", "isOwnedBy/hasName"],
                    "reset_index": False,
                }
        }

    def get_params_similarity_functions(self):
        return [{
                    "name": "dist_nltk_edit",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 3
                },{
                    "name": "dist_absolute",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 10
                },{
                    "name": "dist_relative",
                    "cut_off_mode": "fixed",
                    "cut_off_value": 0.1
                }
        ]

    def test_add_similarity_functions(self):

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params_blocking = self.get_params_blocking()
        manager = scoring.create_score_manager(src_onto, tgt_onto, params_blocking)

        sim_fcts = [
            scoring.similarity_from_dist_fct(scoring.dist_nltk_edit),
            scoring.similarity_from_dist_fct(scoring.dist_equal)
        ]

        # test case 1
        self.assertRaises(RuntimeError, manager.add_prop_prop_fct_tuples, 'name', 'some unknown property name', sim_fcts)
        len_tuples = len(manager.get_prop_prop_fct_tuples())
        self.assertEquals(len_tuples, 0)

        # test case 2
        manager.add_prop_prop_fct_tuples('name', 'name', sim_fcts)
        manager.add_prop_prop_fct_tuples('name', 'isOwnedBy/hasName', sim_fcts)
        len_tuples = len(manager.get_prop_prop_fct_tuples())
        self.assertEquals(len_tuples, 4)

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
        sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)

        score = sim_fcts[0]('power station', 'power1 station2')
        self.assertEquals(score, 0)

        score = sim_fcts[1]('power station', 'power1 station2')
        self.assertEquals(score, 0)

        score = sim_fcts[2]('power station', 'power1 station2')
        self.assertAlmostEquals(score, 1/3, places=4)

        score = sim_fcts[2]('power station', 'power station')
        self.assertEquals(score, 1)

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
        sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)

        score = sim_fcts[0](10.5, 10.8)
        self.assertAlmostEquals(score, 0.7, places=4)

        score = sim_fcts[0](10.5, 11.6)
        self.assertEquals(score, 0.)

        score = sim_fcts[1](10.5, 15.5)
        self.assertAlmostEquals(score, 0.5, places=4)

        score = sim_fcts[1](10.5, 25.5)
        self.assertEquals(score, 0.)

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
        sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)

        score = sim_fcts[0](10, 9)
        self.assertAlmostEquals(score, 0.9, places=4)

        score = sim_fcts[0](10, 9.7)
        self.assertAlmostEquals(score, 0.97, places=4)

        score = sim_fcts[0](10, 20)
        self.assertAlmostEquals(score, 0.5, places=4)

        score = sim_fcts[1](10, 9)
        self.assertAlmostEquals(score, 0., places=4)

        score = sim_fcts[1](10, 9.7)
        self.assertAlmostEquals(score, 0.7, places=4)

    def test_calculate_between_entities_with_equal_values(self):

        c1 = ['name1', 'owner1', 'year1', 'capacity1', 'fuel1']
        v1 = ['Werkskraftwerk Sappi Alfeld', 'Sappi Alfeld GmbH', 2003, 13.5, 'coal']
        entity1 = self.convert_to_dict(c1, v1)

        c2 =  ['name2', 'owner2', 'year2', 'capacity2', 'fuel2']
        v2 = v1.copy()
        entity2 = self.convert_to_dict(c2, v2)

        prop_prop_dist_tuples = [
            ('name1', 'name2', scoring.dist_bounded_edit(0)),
            ('owner1', 'owner2', scoring.dist_nltk_edit),
            ('year1', 'year2', scoring.dist_absolute),
            ('capacity1', 'capacity2', scoring.dist_relative),
            ('fuel1', 'fuel2', scoring.dist_equal)
        ]

        prop_prop_sim_tuples = self.convert_to_similarity_fcts(prop_prop_dist_tuples)
        result = scoring.ScoreManager.calculate_between_entities(entity1, entity2,  prop_prop_sim_tuples)
        self.assertEqual(result, [1, 1, 1, 1, 1])

    def test_calculate_between_entities_with_different_values(self):

        c1 = ['name1', 'owner1', 'year1', 'capacity1', 'fuel1']
        v1 = ['Werkskraftwerk Sappi Alfeld', 'Sappi Alfeld GmbH', 2003, 10., 'coal']
        entity1 = self.convert_to_dict(c1, v1)

        c2 =  ['name2', 'owner2', 'year2', 'capacity2', 'fuel2']
        v2 =  ['12Werkskraftwerk Sappi Alfeld', 'Sappi1 Alfeld2 GmbH3', 2013, 8., 'wind']
        entity2 = self.convert_to_dict(c2, v2)

        prop_prop_dist_tuples = [
            ('name1', 'name2', scoring.dist_bounded_edit(2)),
            ('owner1', 'owner2', scoring.dist_nltk_edit),
            ('year1', 'year2', scoring.dist_absolute),
            ('capacity1', 'capacity2', scoring.dist_relative),
            ('fuel1', 'fuel2', scoring.dist_equal)
        ]

        prop_prop_sim_tuples = self.convert_to_similarity_fcts(prop_prop_dist_tuples)
        result = scoring.ScoreManager.calculate_between_entities(entity1, entity2, prop_prop_sim_tuples)
        expected = [1/3, 0, 0, 8/10, 0]
        for i, actual in enumerate(result):
            self.assertAlmostEqual(actual, expected[i], places=4)

    def test_calculate_scores_between_datasets(self):

        prop_prop_dist_tuples = [
            ('name', 'name', scoring.dist_bounded_edit(2)),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', scoring.dist_nltk_edit),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', scoring.dist_absolute),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', scoring.dist_relative),
        ]

        prop_prop_sim_tuples = self.convert_to_similarity_fcts(prop_prop_dist_tuples)

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params_blocking = self.get_params_blocking()
        manager = scoring.create_score_manager(src_onto, tgt_onto, params_blocking)
        for prop1, prop2, sim_fct in prop_prop_sim_tuples:
            manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)

        df_scores = manager.calculate_scores_between_datasets()
        self.assertEquals(len(df_scores), 4666)

    def test_calculate_maximum_scores_and_assert_means(self):

        #prepare

        score_fct_1 = scoring.similarity_from_dist_fct(scoring.dist_bounded_edit(2))
        score_fct_2 = scoring.similarity_from_dist_fct(scoring.dist_nltk_edit, cut_off_value=10)
        score_fct_3 = scoring.similarity_from_dist_fct(scoring.dist_absolute, cut_off_value=10)
        score_fct_4 = scoring.similarity_from_dist_fct(scoring.dist_relative)

        prop_prop_sim_tuples = [
            ('name', 'name', score_fct_1),
            ('name', 'isOwnedBy/hasName', score_fct_1),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', score_fct_2),
            ('isOwnedBy/hasName', 'name', score_fct_2),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', score_fct_3),
            ('hasYearOfBuilt/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', score_fct_3),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', score_fct_4),
            ('designCapacity/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', score_fct_4),
        ]

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params_blocking = self.get_params_blocking()
        manager = scoring.create_score_manager(src_onto, tgt_onto, params_blocking)
        for prop1, prop2, sim_fct in prop_prop_sim_tuples:
            manager.add_prop_prop_fct_tuples(prop1, prop2, sim_fct)

        # run

        manager.calculate_scores_between_datasets()
        df_max_scores_1, df_max_scores_2 = manager.calculate_maximum_scores()

        # assert

        logging.info('\nmax scores for dataset 1:\n------------------')
        logging.info('\n%s', df_max_scores_1)
        logging.info('\n%s', df_max_scores_1.describe())

        self.assertEquals(len(df_max_scores_1), 1096)

        means = df_max_scores_1.describe().loc['mean']
        logging.info('means=\n%s', means)
        self.assertGreater(means[0], means[1])
        self.assertGreater(means[2], means[3])
        self.assertGreater(means[4], means[5])
        self.assertGreater(means[6], means[7])

    def test_property_mapping_2_props(self):

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params_blocking = self.get_params_blocking()
        manager = scoring.create_score_manager(src_onto, tgt_onto, params_blocking)

        params_sim_fcts = self.get_params_similarity_functions()
        sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)
        props1 = ['name', 'isOwnedBy/hasName']
        props2 = ['name', 'isOwnedBy/hasName']
        property_mapping = scoring.find_property_mapping(manager, sim_fcts, props1, props2)

        expected = {
            'name': ('name', 0.588, 0),
            'isOwnedBy/hasName': ('isOwnedBy/hasName', 0.577, 0)
        }

        self.assertEquals(len(property_mapping), len(expected))
        for mapping in property_mapping:
            prop1 = mapping['prop1']
            prop2, mean, pos_sfct = expected[prop1]
            self.assertEquals(mapping['prop2'], prop2)
            self.assertAlmostEquals(mapping['mean'], mean, places=2)
            self.assertEquals(mapping['pos_sfct'], pos_sfct)

    def test_property_mapping_6_props(self):

        params_sim_fcts = [{
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
                }
        ]

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        params_blocking = self.get_params_blocking()
        manager = scoring.create_score_manager(src_onto, tgt_onto, params_blocking)

        sim_fcts = scoring.create_similarity_functions_from_params(params_sim_fcts)
        property_mapping = scoring.find_property_mapping(manager, sim_fcts)

        expected = {
            'name' : ('name',  0.5886178861788618, 0),
            'hasYearOfBuilt/hasValue/numericalValue':  ('hasYearOfBuilt/hasValue/numericalValue', 0.9977108613593972, 2),
            'designCapacity/hasValue/numericalValue': ('designCapacity/hasValue/numericalValue', 0.7234157194326809, 2),
            'isOwnedBy/hasName': ('isOwnedBy/hasName', 0.5775978407557356, 0),
            #'geo:wgs84_pos#long': ('hasGISCoordinateSystem/hasProjectedCoordinate_x/hasValue/numericalValue', 0.9708340665506993, 2),
            #'geo:wgs84_pos#lat': ('hasGISCoordinateSystem/hasProjectedCoordinate_y/hasValue/numericalValue', 0.9960711009327202, 2)
        }

        self.assertEquals(len(property_mapping), len(expected))
        for mapping in property_mapping:
            prop1 = mapping['prop1']
            prop2, mean, pos_sfct = expected[prop1]
            self.assertEquals(mapping['prop2'], prop2)
            self.assertAlmostEquals(mapping['mean'], mean, places=2)
            self.assertEquals(mapping['pos_sfct'], pos_sfct)
