import logging

import blocking
import scoring
import utils_for_testing

class TestScoring(utils_for_testing.TestCaseOntoMatch):

    def convert_to_dict(self, keys, values):
        return { k:v for k,v in zip(keys, values)}

    def convert_to_score_fcts(self, dist_fcts):
        score_fcts = []
        for prop1, prop2, dist_fct in dist_fcts:
            score_fcts.append((prop1, prop2, scoring.convert_to_score_fct(dist_fct)))
        return score_fcts

    def init_dataframes_and_blocking(self):

        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        dframe1 = blocking.create_dataframe_from_ontology(src_onto)
        dframe2 = blocking.create_dataframe_from_ontology(tgt_onto)
        logging.info('columns of dataset 1 =%s', dframe1.columns)
        logging.info('columns of dataset 2 =%s', dframe2.columns)

        blocking_params = {
                #"name": "FullPairIterator",
                "name": "TokenBasedPairIterator",
                "model_specific": {
                     "min_token_length": 3,
                     "max_token_occurrences_src": 20,
                     "max_token_occurrences_tgt": 20,
                     "blocking_properties": ["name", "isOwnedBy/hasName"],
                     #"blocking_properties": ['dbp:name', 'dbp:owner', "name", "isOwnedBy/hasName"], #DBPedia
                    "reset_index": False,
                }
        }

        it = blocking.create_iterator(src_onto, tgt_onto, blocking_params)

        return dframe1, dframe2, it

    def test_add_score_fcts(self):
        src_onto, tgt_onto = self.load_kwl_gppd_ontologies()
        dframe1 = blocking.create_dataframe_from_ontology(src_onto)
        dframe2 = blocking.create_dataframe_from_ontology(tgt_onto)
        manager = scoring.ScoreManager(dframe1, dframe2, None)
        print(dframe1.columns)
        print(dframe2.columns)

        score_fcts = [
            scoring.convert_to_score_fct(scoring.dist_nltk_edit),
            scoring.convert_to_score_fct(scoring.dist_equal)
        ]

        # test case 1
        self.assertRaises(RuntimeError, manager.add_score_fcts, 'name', 'some unknown property name', score_fcts)
        len_score_fcts = len(manager.get_score_fcts())
        self.assertEquals(len_score_fcts, 0)

        # test case 2
        manager.add_score_fcts('name', 'name', score_fcts)
        manager.add_score_fcts('name', 'isOwnedBy/hasName', score_fcts)
        len_score_fcts = len(manager.get_score_fcts())
        self.assertEquals(len_score_fcts, 4)

    def test_calculate_between_entities_with_equal_values(self):

        c1 = ['name1', 'owner1', 'year1', 'capacity1', 'fuel1']
        v1 = ['Werkskraftwerk Sappi Alfeld', 'Sappi Alfeld GmbH', 2003, 13.5, 'coal']
        entity1 = self.convert_to_dict(c1, v1)

        c2 =  ['name2', 'owner2', 'year2', 'capacity2', 'fuel2']
        v2 = v1.copy()
        entity2 = self.convert_to_dict(c2, v2)

        distance_fcts = [
            ('name1', 'name2', scoring.dist_bounded_edit(0)),
            ('owner1', 'owner2', scoring.dist_nltk_edit),
            ('year1', 'year2', scoring.dist_absolute),
            ('capacity1', 'capacity2', scoring.dist_relative),
            ('fuel1', 'fuel2', scoring.dist_equal)
        ]

        score_fcts = self.convert_to_score_fcts(distance_fcts)
        result = scoring.ScoreManager.calculate_between_entities(entity1, entity2, score_fcts)
        self.assertEqual(result, [1, 1, 1, 1, 1])

    def test_calculate_between_entities_with_different_values(self):

        c1 = ['name1', 'owner1', 'year1', 'capacity1', 'fuel1']
        v1 = ['Werkskraftwerk Sappi Alfeld', 'Sappi Alfeld GmbH', 2003, 10., 'coal']
        entity1 = self.convert_to_dict(c1, v1)

        c2 =  ['name2', 'owner2', 'year2', 'capacity2', 'fuel2']
        v2 =  ['12Werkskraftwerk Sappi Alfeld', 'Sappi1 Alfeld2 GmbH3', 2013, 8., 'wind']
        entity2 = self.convert_to_dict(c2, v2)

        distance_fcts = [
            ('name1', 'name2', scoring.dist_bounded_edit(2)),
            ('owner1', 'owner2', scoring.dist_nltk_edit),
            ('year1', 'year2', scoring.dist_absolute),
            ('capacity1', 'capacity2', scoring.dist_relative),
            ('fuel1', 'fuel2', scoring.dist_equal)
        ]

        score_fcts = self.convert_to_score_fcts(distance_fcts)
        result = scoring.ScoreManager.calculate_between_entities(entity1, entity2, score_fcts)
        expected = [1/3, 0, 0, 8/10, 0]
        for i, actual in enumerate(result):
            self.assertAlmostEqual(actual, expected[i], places=4)

    def test_calculate_scores_between_datasets(self):

        dframe1, dframe2, it = self.init_dataframes_and_blocking()

        distance_fcts = [
            ('name', 'name', scoring.dist_bounded_edit(2)),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', scoring.dist_nltk_edit),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', scoring.dist_absolute),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', scoring.dist_relative),
        ]

        score_fcts = self.convert_to_score_fcts(distance_fcts)

        manager = scoring.ScoreManager(dframe1, dframe2, it)
        for prop1, prop2, score_fct in score_fcts:
            manager.add_score_fcts(prop1, prop2, score_fct)

        df_scores = manager.calculate_scores_between_datasets()
        self.assertEquals(len(df_scores), 4666)

    def test_property_mapping(self):

        #prepare

        dframe1, dframe2, it = self.init_dataframes_and_blocking()

        score_fct_1 = scoring.convert_to_score_fct(scoring.dist_bounded_edit(2))
        score_fct_2 = scoring.convert_to_score_fct(scoring.dist_nltk_edit, cut_off_value=10)
        score_fct_3 = scoring.convert_to_score_fct(scoring.dist_absolute, cut_off_value=10)
        score_fct_4 = scoring.convert_to_score_fct(scoring.dist_relative)

        score_fcts = [
            ('name', 'name', score_fct_1),
            ('name', 'isOwnedBy/hasName', score_fct_1),
            ('isOwnedBy/hasName', 'isOwnedBy/hasName', score_fct_2),
            ('isOwnedBy/hasName', 'name', score_fct_2),
            ('hasYearOfBuilt/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', score_fct_3),
            ('hasYearOfBuilt/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', score_fct_3),
            ('designCapacity/hasValue/numericalValue', 'designCapacity/hasValue/numericalValue', score_fct_4),
            ('designCapacity/hasValue/numericalValue', 'hasYearOfBuilt/hasValue/numericalValue', score_fct_4),
        ]

        manager = scoring.ScoreManager(dframe1, dframe2, it)
        for prop1, prop2, score_fct in score_fcts:
            manager.add_score_fcts(prop1, prop2, score_fct)

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
