import ontomatch.kgoperations.querykg as q
import unittest

class test_kg(unittest.TestCase):
    def test_something(self):
        query = '''
        PREFIX gn:<http://www.geonames.org/ontology#>
        SELECT distinct ?s ?n FROM <Geonames_Germany>
        { ?s a <http://www.geonames.org/ontology#Feature>.
         ?s gn:name ?n.
         } limit 1
        '''
        EP = "geonames"
        result = q.querykg(sparqlEndPoint=EP, queryStr=query)
        print(result)
        self.assertIsNotNone(result)  # add assertion here
if __name__ == '__main__':
    unittest.main()