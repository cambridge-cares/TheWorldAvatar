import ontomatch.kgoperations.querykg as q
import unittest

class test_kg(unittest.TestCase):
    def test_something(self):
        query = '''
        SELECT distinct * FROM <http://kwl>
        { ?a ?b ?c
         } limit 1
        '''
        EP = "powerplants"
        result = q.querykg(sparqlEndPoint=EP, queryStr=query)
        print(result)
        self.assertIsNotNone(result)  # add assertion here
if __name__ == '__main__':
    unittest.main()