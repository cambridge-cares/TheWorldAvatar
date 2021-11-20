from six import assertCountEqual
import ontomatch.knowledge.search
import tests.utils_for_testing

class TestSearch(tests.utils_for_testing.TestCaseOntoMatch):

    def test_create_index(self):
        addr = './data/municipalities_germany.ttl'
        properties = ['rdfs:label', 'sdo:postalCode']
        index = ontomatch.knowledge.search.create_index(addr, 'turtle', properties)
        key = ontomatch.knowledge.search.normalize('Alfhausen')
        iris = index[key]
        self.assertEquals(len(iris), 1)
        key = ontomatch.knowledge.search.normalize(49594)
        iris = index[key]
        self.assertEquals(len(iris), 1)
