
import unittest
from zeolite_db import *

class TestZeoliteDB(unittest.TestCase):

    def test__get_compound_iri(self):
        zdb = ZeoliteDB()
        zdb.load()
        mats = zdb.get_framework_materials("ACO")
        print(mats[0].data)
        #print
        pass

    def test_get_cod(self):
        zdb = ZeoliteDB()
        cod = zdb._get_cod(4393)

        self.assertEqual(cod, 2222)

if __name__ == "__main__":
    unittest.main()

