from six import assertCountEqual
import knowledge.search
import utils_for_testing

class TestSearch(utils_for_testing.TestCaseOntoMatch):

    def test_create_index(self):
        addr = './data/municipalities_germany.ttl'
        properties = ['rdfs:label', 'sdo:postalCode']
        index = knowledge.search.create_index(addr, 'turtle', properties)
        key = knowledge.search.normalize('Alfhausen')
        iris = index[key]
        self.assertEquals(len(iris), 1)
        key = knowledge.search.normalize(49594)
        iris = index[key]
        self.assertEquals(len(iris), 1)
