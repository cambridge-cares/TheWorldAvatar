""" 

Usage:
from the directory ONE LEVEL UPPER run:
python tests\test_crystalagent.py
"""

import unittest
import os

# Need to specify the path to load the module from a separate directory:
import sys
sys.path.append("python")

import tools
import crystalinfo

class TestCrystalAgent(unittest.TestCase):
    """ Block of tests to check the flags. 
    The flag is a command line argument, used to specify the content of csv.

    """
    def setUp(self):
        self.uuidfile = os.path.join("tests", "uuid-db.csv")

    def find_word_in_list2d(self, word, list2d):
        """
        """
        if not isinstance(list2d, list):
            self.fail("In find_word_in_list() expect a list argument, got '" +
                      str(type(list2d)) + "'.")
            return False

        for list1d in list2d:
            if not isinstance(list1d, list):
                self.fail("In find_word_in_list() expect argument 2d list, " +
                          "got '" + str(type(list1d)) + "'.")
                return False
            for cell in list1d:
                if word in str(cell):
                    return True

        return False
        # === end of TestCrystalAgent.find_word_in_list2d()

    def test_flag_X(self):
        """ Flag X is used to save XRD spectrum to the csv file.
        Here test ONLY the X flag, i.e. the CrystalInformation is not saved.
        Also check:
          - the XRD peak file is created in the subfolder, 
          - the cif_iri_list.csv file has an updated xrd_iri value,
          - 

        Currently the crystal agent cannot test the X option, because it is a command line argument.
        """
        uuidDB = tools.UuidDB(self.uuidfile)

        #cryst = crystalinfo.CrystalInfo(uuidDB=None, abox_prefix="https://abox/")
        #csv_arr = cryst.get_csv_arr_from_cif(

        #self.fail("Not implemented test")
        uuidDB.saveDB()
        # === end of test_flag_X()

    def test_flag_C(self):
        """ Flag C is used to save the basic crystal info data to the csv file.
        Here test ONLY the C flag, i.e. the CrystalInformation is saved.
        Also check:
          - the cif_iri_list.csv file has an updated cif_iri value,
          - 

        """
        uuidDB = tools.UuidDB(self.uuidfile)

        cryst = crystalinfo.CrystalInfo(uuidDB=uuidDB, abox_prefix="https://abox/")
        csv_arr = cryst.get_csv_arr_from_cif(os.path.join("tests", "1000000.cif"),
                  "COD_1000000", subject="", predicate="", flags="C")

        self.assertEqual(self.find_word_in_list2d("CrystalInformation", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Reciprocal", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("UnitCell", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("Transform", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("Atom", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("UnitCellVectorSet", csv_arr), False)

        uuidDB.saveDB()
        # === end of test_flag_C()

    def test_flag_A(self):
        """ Flag A is used to save the atomic structure to the csv file.
        Here test ONLY the A flag, i.e. the CrystalInformation is saved.
        Also check:
          - the cif_iri_list.csv file has an updated cif_iri value,
          - 

        """
        uuidDB = tools.UuidDB(self.uuidfile)

        cryst = crystalinfo.CrystalInfo(uuidDB=uuidDB, abox_prefix="https://abox/")
        csv_arr = cryst.get_csv_arr_from_cif(os.path.join("tests", "1000000.cif"),
                  "COD_1000000", subject="", predicate="", flags="A")

        self.assertEqual(self.find_word_in_list2d("CrystalInformation", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Reciprocal", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("UnitCell", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("Transform", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("Atom", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("AtomSite", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("UnitCellVectorSet", csv_arr), False)

        uuidDB.saveDB()
        # === end of test_flag_A()

    def test_flag_R(self):
        """ Flag R is used to save the atomic structure to the csv file.
        Here test ONLY the R flag, i.e. the CrystalInformation is saved.
        Also check:
          - the cif_iri_list.csv file has an updated cif_iri value,
          - 

        """
        uuidDB = tools.UuidDB(self.uuidfile)

        cryst = crystalinfo.CrystalInfo(uuidDB=uuidDB, abox_prefix="https://abox/")
        csv_arr = cryst.get_csv_arr_from_cif(os.path.join("tests", "1000000.cif"),
                  "COD_1000000", subject="", predicate="", flags="R")

        self.assertEqual(self.find_word_in_list2d("CrystalInformation", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Reciprocal", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("UnitCell", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Transform", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("Atom", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("AtomSite", csv_arr), False)

        uuidDB.saveDB()
        # === end of test_flag_R()

    def test_flag_T(self):
        """ Flag T is used to save the coordinate transformation to the csv file.
        Here test ONLY the T flag, i.e. the CrystalInformation is saved.
        Also check:
          - the cif_iri_list.csv file has an updated cif_iri value,
          - 

        """
        uuidDB = tools.UuidDB(self.uuidfile)

        cryst = crystalinfo.CrystalInfo(uuidDB=uuidDB, abox_prefix="https://abox/")
        csv_arr = cryst.get_csv_arr_from_cif(os.path.join("tests", "1000000.cif"),
                  "COD_1000000", subject="", predicate="", flags="T")

        self.assertEqual(self.find_word_in_list2d("CrystalInformation", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Reciprocal", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("UnitCell", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("Transform", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Atom", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("AtomSite", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("UnitCellVectorSet", csv_arr), False)

        uuidDB.saveDB()
        # === end of test_flag_T()

    def test_flag_V(self):
        """ Flag V is used to save the coordinate transformation to the csv file.
        Here test ONLY the V flag, i.e. the CrystalInformation is saved.
        Also check:
          - the cif_iri_list.csv file has an updated cif_iri value,
          - 

        """
        uuidDB = tools.UuidDB(self.uuidfile)

        cryst = crystalinfo.CrystalInfo(uuidDB=uuidDB, abox_prefix="https://abox/")
        csv_arr = cryst.get_csv_arr_from_cif(os.path.join("tests", "1000000.cif"),
                  "COD_1000000", subject="", predicate="", flags="V")

        self.assertEqual(self.find_word_in_list2d("CrystalInformation", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Reciprocal", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("UnitCell", csv_arr), True)
        self.assertEqual(self.find_word_in_list2d("Transform", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("Atom", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("AtomSite", csv_arr), False)
        self.assertEqual(self.find_word_in_list2d("UnitCellVectorSet", csv_arr), True)

        uuidDB.saveDB()
        # === end of test_flag_V()




    # === end of TestCrystalAgent

if __name__ == "__main__":
    unittest.main()
