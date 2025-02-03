
import unittest
import os

# from crystalinfo import *
import crystalinfo
import crystaldata
import tools

ontoCrystPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

class TestCifIriData(unittest.TestCase):
    def test_load(self):
        cifs = crystalinfo.CifIriData("test_cif_iri.csv")

        self.assertIsInstance(cifs.data, list)
        self.assertEqual(cifs.data, [])

    def test_save(self):
        # Do not save to avoid over-writing the database.
        pass

    def test_set_entry(self):
        cifs = crystalinfo.CifIriData("test_cif_iri.csv")

        self.assertEqual(cifs.data, [])

        cifs.set_entry("CIF/123.cif", 45, 0)


        self.assertEqual(len(cifs.data), 1)

        line = cifs.data[0]
        self.assertEqual(line[0], "CIF\\123.cif")
        self.assertEqual(line[1], 0)
        #self.assertEqual(line[2], "iri"]
        #self.assertEqual(line[3], "uuid")
        self.assertEqual(line[4], 45)
        self.assertEqual(line[5], 0)

        pass

    def test_get_entry_iri(self):
        cifs = crystalinfo.CifIriData("test_cif_iri.csv")

        self.assertEqual(cifs.data, [])

        cifs.set_entry("CIF/123.cif", 45, 0)

        self.assertEqual(len(cifs.data), 1)
        cifs.data[0][2] = "unique_iri_and_uuid"
        cifs.data[0][3] = "uuid"

        iri, uid = cifs.get_entry_iri("CIF/123.cif")
        self.assertEqual(iri, "unique_iri_and_uuid")

        iri, uid = cifs.get_entry_iri("CIF\\123.cif")
        self.assertEqual(iri, "unique_iri_and_uuid")

        iri, uid = cifs.get_entry_iri("CIF/AAA.cif")
        self.assertEqual(iri, None)

        iri, uid = cifs.get_entry_iri("CIF\\AAA.cif")
        self.assertEqual(iri, None)

        pass

    def test_del_entry(self):
        cifs = crystalinfo.CifIriData("test_cif_iri.csv")

        self.assertEqual(cifs.data, [])

        cifs.set_entry("CIF/123.cif", 45, 0)
        cifs.set_entry("CIF/456.cif", 45, 0)
        cifs.set_entry("CIF/789.cif", 47, 0)

        self.assertEqual(len(cifs.data), 3)

        # Delete non-existing entry: no effect
        cifs.del_entry("CIF/AAA.cif")
        self.assertEqual(len(cifs.data), 3)

        # Delete last entry
        cifs.del_entry("CIF/789.cif")
        self.assertEqual(len(cifs.data), 2)

        cifs.del_entry("CIF/123.cif")
        self.assertEqual(len(cifs.data), 1)

        # Delete non-existing entry:
        cifs.del_entry("CIF/123.cif")
        self.assertEqual(len(cifs.data), 1)

        # Delete the last entry
        cifs.del_entry("CIF/456.cif")
        self.assertEqual(len(cifs.data), 0)

        #self.fail("Not implemented delete entry")
        pass
    # === end of class TestCifIriData

class TestCrystalInfo(unittest.TestCase):
    def setUp(self):
        self.uuidDB = tools.UuidDB(filename="test_crystalinfo.csv")
        # === end of TestCrystalInfo.setUp()

    def tearDown(self):
        self.uuidDB.saveDB()
        # === end of TestCrystalInfo.tearDown()

    def test_cif_iri_list(self):
        """ Basic manipulations with cif_iri_list: add, get, del.
        """
        # Existing file
        # Non-existing file
        # Wrong side slash

        pass
        # === end of TestCrystalInfo.()

    def test_csv_arr_get_iri(self):
        """ Generate only one line abox: iri for the exising cif iri.
        """

        # 
        file = "LI-CIF\\Li-ABW.cif"

        cr_info = crystalinfo.CrystalInfo(uuidDB=self.uuidDB)

        arr = cr_info.get_csv_arr_from_cif(file,# "cif_name",
                                           subject="subj", #predicate="",
                                           #new_uuid="new_uuid"
                                           )
        #print(arr)
        self.assertEqual(len(arr), 1)

        #self.assertEqual(arr[0], 
        #                 ["subj", "Instance", "iri",
        #                  "hasCrystalInformation", "", ""])
        line = arr[0]
        self.assertEqual(len(line), 6)

        self.assertEqual(line[0], "subj")
        self.assertEqual(line[1], "Instance")
        #self.assertEqual(line[2].endswith("eee50"), True, msg=line[2])
        self.assertEqual(line[3], "hasCrystalInformation")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")
        pass
        # === end of TestCrystalInfo.test_csv_arr_get_iri()

    def test_csv_arr_for_abox_ABW(self):
        """ Create a list with cif data to be saved to file, for ABW.cif.
        """
        abox = ""
        abox = "abox_prefix/"

        file = "CIF/ABW.cif"

        cr_info = crystalinfo.CrystalInfo(uuidDB=self.uuidDB,
                                          abox_prefix="abox_prefix/")

        arr = cr_info.get_csv_arr_from_cif(file, "",
                                           subject="", predicate="",
                                           new_uuid="new_uuid")

        tools.writeCsv("tmp.tmp.tmp.csv", arr)

        # ontoCrystPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
        i = 0
        line = arr[i]
        self.assertTrue(line[0].startswith(abox + "CrystalInformation_"))
        self.assertTrue(line[0].endswith("_new_uuid"), msg=line[0])
        #self.assertEqual(line[0], ontoCrystPrefix + "CrystalInformation_new_uuid" )
        #self.assertEqual(line[0], abox + "CrystalInformation_new_uuid" )
        self.assertEqual(line[1:], ["Instance",
                                    ontoCrystPrefix + "CrystalInformation",
                                    "", "", "" ])
        i += 1
        line = arr[i]
        #self.assertTrue(line[0].startswith("CrystalInformation_"))
        #self.assertTrue(line[0].endswith("_new_uuid"))
        #self.assertEqual(line[0], ontoCrystPrefix + "CrystalInformation_new_uuid" )
        #self.assertEqual(line[0], "CrystalInformation_new_uuid" )
        self.assertEqual(line, [abox + "UnitCell_" + "new_uuid", "Instance",
                                ontoCrystPrefix + "UnitCell", "", "", "" ])
        i += 1
        line = arr[i]
        self.assertEqual(line, [abox + "CrystalInformation_" + "new_uuid",
                                "Instance", abox + "UnitCell_" + "new_uuid",
                                ontoCrystPrefix + "hasUnitCell", "", "" ])
 
        #self.fail("sssssssss")

        # === end of TestCrystalInfo.test_csv_arr_for_abox_ABW()

    def test_run_many(self):
        cif_list = [
              #'LI-CIF\\Li-RFE.cif',
              'LI-CIF\\Li-MRT.cif']
        """
              ,
              'LI-CIF\\Li-ATN.cif',
              'LI-CIF\\Li-AWO.cif',
              'LI-CIF\\Li-EUO.cif',
        ]
        """

        self.fail("Not implemented an error cif")
        #uuidDB = tools.UuidDB()

        for cif_path in cif_list:
            cryst = crystalinfo.CrystalInfo(uuidDB=self.uuidDB)
            arr = cryst.get_csv_arr_from_cif(cif_path, "cryst", "name_uuid")

        self.assertTrue(len(arr) > 100, msg=cif_path)
        # === end of TestCrystalInfo.atest_run_many()

    def test_load_cif(self):

        cif_list = [
              #'LI-CIF\\Li-RFE.cif',
              'LI-CIF\\Li-MRT.cif']

        for cif_path in cif_list:
            cryst = crystalinfo.CrystalInfo(uuidDB=self.uuidDB)
            cryst.load_cif_file(cif_path)

            self.assertIsInstance(cryst.cifValAndErr.unitCellLengths.comp_dict, dict)
            self.assertIsInstance(cryst.cifValAndErr.unitCellLengths.comp_dict["a"], dict)
            self.assertIsInstance(cryst.cifValAndErr.unitCellLengths.comp_dict["b"], dict)
            self.assertIsInstance(cryst.cifValAndErr.unitCellLengths.comp_dict["c"], dict)
            self.assertEqual(cryst.cifValAndErr.unitCellLengths.comp_dict["a"]["value"], "14.994")
            self.assertEqual(cryst.cifValAndErr.unitCellLengths.comp_dict["b"]["value"], "27.2289")
            self.assertEqual(cryst.cifValAndErr.unitCellLengths.comp_dict["c"]["value"], "13.6213")

            self.assertIsInstance(cryst.cifValAndErr.unitCellAngles.comp_dict, dict)
            self.assertIsInstance(cryst.cifValAndErr.unitCellAngles.comp_dict["alpha"], dict)
            self.assertIsInstance(cryst.cifValAndErr.unitCellAngles.comp_dict["beta" ], dict)
            self.assertIsInstance(cryst.cifValAndErr.unitCellAngles.comp_dict["gamma"], dict)
            self.assertEqual(cryst.cifValAndErr.unitCellAngles.comp_dict["alpha"]["value"], "90")
            self.assertEqual(cryst.cifValAndErr.unitCellAngles.comp_dict["beta" ]["value"], "90")
            self.assertEqual(cryst.cifValAndErr.unitCellAngles.comp_dict["gamma"]["value"], "90")

            # After assigning to cifOutput
            #self.assertEqual(cryst.cifOutput.unitCellLengths.comp_dict, 1)
            self.assertIsInstance(cryst.cifOutput.unitCellLengths.comp_dict, dict)
            self.assertIsInstance(cryst.cifOutput.unitCellLengths.comp_dict["a"], dict)
            self.assertIsInstance(cryst.cifOutput.unitCellLengths.comp_dict["b"], dict)
            self.assertIsInstance(cryst.cifOutput.unitCellLengths.comp_dict["c"], dict)
            self.assertEqual(cryst.cifOutput.unitCellLengths.comp_dict["a"]["value"], "14.994")
            self.assertEqual(cryst.cifOutput.unitCellLengths.comp_dict["b"]["value"], "27.2289")
            self.assertEqual(cryst.cifOutput.unitCellLengths.comp_dict["c"]["value"], "13.6213")
            self.assertEqual(cryst.cifOutput.unitCellLengths.comp_dict["a"]["label"], "a")
            self.assertEqual(cryst.cifOutput.unitCellLengths.comp_dict["b"]["label"], "b")
            self.assertEqual(cryst.cifOutput.unitCellLengths.comp_dict["c"]["label"], "c")
            #print("vol =", cryst.cifOutput.unitCellVolume.value)
            self.assertAlmostEqual(cryst.cifOutput.unitCellVolume.value,
                                   14.994 * 27.2289 * 13.6213)

            self.assertEqual(cryst.cifOutput.unitCellVectorA.comp_list, [14.994,  0., 0.])
            self.assertAlmostEqual(cryst.cifOutput.unitCellVectorB.comp_list[0], 0.)
            self.assertAlmostEqual(cryst.cifOutput.unitCellVectorB.comp_list[1], 27.2289)
            self.assertAlmostEqual(cryst.cifOutput.unitCellVectorB.comp_list[2], 0.)
            #self.assertEqual(cryst.cifOutput.unitCellVectorB.comp_list[2], [0., 27.2289, 0.])
            self.assertAlmostEqual(cryst.cifOutput.unitCellVectorC.comp_list[0], 0.)
            self.assertAlmostEqual(cryst.cifOutput.unitCellVectorC.comp_list[1], 0.)
            self.assertAlmostEqual(cryst.cifOutput.unitCellVectorC.comp_list[2], 13.6213)
            #self.assertEqual(cryst.cifOutput.unitCellVectorC.comp_list, [0., 0., 13.6213])

            self.assertIsInstance(cryst.cifOutput.unitCellAngles.comp_dict, dict)
            self.assertIsInstance(cryst.cifOutput.unitCellAngles.comp_dict["alpha"], dict)
            self.assertIsInstance(cryst.cifOutput.unitCellAngles.comp_dict["beta" ], dict)
            self.assertIsInstance(cryst.cifOutput.unitCellAngles.comp_dict["gamma"], dict)
            self.assertEqual(cryst.cifOutput.unitCellAngles.comp_dict["alpha"]["value"], "90")
            self.assertEqual(cryst.cifOutput.unitCellAngles.comp_dict["beta" ]["value"], "90")
            self.assertEqual(cryst.cifOutput.unitCellAngles.comp_dict["gamma"]["value"], "90")

            # Transformation matrix:

            self.assertIsInstance(cryst.cifValAndErr.listAtomRaw, list)
            n_atom = len(cryst.cifValAndErr.listAtomRaw)
            self.assertEqual( n_atom, 63, msg="actual len = " + str(n_atom))
            self.assertIsInstance(cryst.cifValAndErr.listAtomRaw[0], crystaldata.AtomInformation)

            atom = cryst.cifValAndErr.listAtomRaw[0]
            self.assertEqual(atom.frac, ["0.6091", "0.6925", "0.983"])
            self.assertEqual(atom.occupancy, 0.848)
            atom = cryst.cifValAndErr.listAtomRaw[2]
            self.assertEqual(atom.frac, ["0.7455", "1.0539", "1.0259"])
            atom = cryst.cifValAndErr.listAtomRaw[62]
            self.assertEqual(atom.frac, ["0.036", "0.0958", "-0.088"])
            self.assertEqual(atom.occupancy, 0.254)

            self.assertIsInstance(cryst.cifValAndErr.listAtomAll, list)
            n_atom = len(cryst.cifValAndErr.listAtomAll)
            self.assertTrue( n_atom > 3, msg="actual len = " + str(n_atom))
            self.assertEqual( n_atom, 63, msg="actual len = " + str(n_atom))
            self.assertIsInstance(cryst.cifValAndErr.listAtomAll[0], crystaldata.AtomInformation)

            atom = cryst.cifValAndErr.listAtomAll[0]
            self.assertEqual(atom.frac, ["0.6091", "0.6925", "0.983"])
            self.assertEqual(atom.occupancy, 0.848)
            atom = cryst.cifValAndErr.listAtomAll[2]
            self.assertEqual(atom.frac, ["0.7455", "1.0539", "1.0259"])
            atom = cryst.cifValAndErr.listAtomAll[62]
            self.assertEqual(atom.frac, ["0.036", "0.0958", "-0.088"])
            self.assertEqual(atom.occupancy, 0.254)

            self.assertEqual(0, 1)

        # === end of TestCrystalInfo.test_load_cif()

    def test_load_cif_reciprocal(self):

        #cif_list = [
        #      'LI-CIF\\Li-ABW.cif']
        cif_path = 'LI-CIF\\Li-ABW.cif'

        cryst = crystalinfo.CrystalInfo(uuidDB=self.uuidDB)
        cryst.load_cif_file(cif_path)

        self.assertIsInstance(cryst.cifValAndErr.unitCellLengths.comp_dict, dict)
        self.assertIsInstance(cryst.cifValAndErr.unitCellLengths.comp_dict["a"], dict)
        self.assertIsInstance(cryst.cifValAndErr.unitCellLengths.comp_dict["b"], dict)

        # Reciprocal:
        ref = crystaldata.CrystalData("PyMatGen", self.uuidDB)
        ref.loadData(cif_path, "cif_name")
        ref.evalPyMatGen()
        #print(ref.unitCellRecipLengths.comp_list)
        val = cryst.cifOutput
        self.assertAlmostEqual(float(val.unitCellRecipLengths.comp_dict["a"]["value"]),
                               float(ref.unitCellRecipLengths.comp_dict["a"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipLengths.comp_dict["b"]["value"]),
                               float(ref.unitCellRecipLengths.comp_dict["b"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipLengths.comp_dict["c"]["value"]),
                               float(ref.unitCellRecipLengths.comp_dict["c"]["value"]))

        self.assertAlmostEqual(float(val.unitCellRecipAngles.comp_dict["alpha"]["value"]),
                               float(ref.unitCellRecipAngles.comp_dict["alpha"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipAngles.comp_dict["beta" ]["value"]),
                               float(ref.unitCellRecipAngles.comp_dict["beta" ]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipAngles.comp_dict["gamma"]["value"]),
                               float(ref.unitCellRecipAngles.comp_dict["gamma"]["value"]))

        self.assertAlmostEqual(float(val.unitCellRecipVectorA.comp_dict["x"]["value"]),
                               float(ref.unitCellRecipVectorA.comp_dict["x"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipVectorA.comp_dict["y"]["value"]),
                               float(ref.unitCellRecipVectorA.comp_dict["y"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipVectorA.comp_dict["z"]["value"]),
                               float(ref.unitCellRecipVectorA.comp_dict["z"]["value"]))

        self.assertAlmostEqual(float(val.unitCellRecipVectorB.comp_dict["x"]["value"]),
                               float(ref.unitCellRecipVectorB.comp_dict["x"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipVectorB.comp_dict["y"]["value"]),
                               float(ref.unitCellRecipVectorB.comp_dict["y"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipVectorB.comp_dict["z"]["value"]),
                               float(ref.unitCellRecipVectorB.comp_dict["z"]["value"]))

        self.assertAlmostEqual(float(val.unitCellRecipVectorC.comp_dict["x"]["value"]),
                               float(ref.unitCellRecipVectorC.comp_dict["x"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipVectorC.comp_dict["y"]["value"]),
                               float(ref.unitCellRecipVectorC.comp_dict["y"]["value"]))
        self.assertAlmostEqual(float(val.unitCellRecipVectorC.comp_dict["z"]["value"]),
                               float(ref.unitCellRecipVectorC.comp_dict["z"]["value"]))

        # ------------------------------------
        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[0][0]),
                               float(ref.matrixFracToCart.comp_list[0][0]))
        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[0][1]),
                               float(ref.matrixFracToCart.comp_list[0][1]))
        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[0][2]),
                               float(ref.matrixFracToCart.comp_list[0][2]))

        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[1][0]),
                               float(ref.matrixFracToCart.comp_list[1][0]))
        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[1][1]),
                               float(ref.matrixFracToCart.comp_list[1][1]))
        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[1][2]),
                               float(ref.matrixFracToCart.comp_list[1][2]))

        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[2][0]),
                               float(ref.matrixFracToCart.comp_list[2][0]))
        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[2][1]),
                               float(ref.matrixFracToCart.comp_list[2][1]))
        self.assertAlmostEqual(float(val.matrixFracToCart.comp_list[2][2]),
                               float(ref.matrixFracToCart.comp_list[2][2]))

        # ------------------------------------
        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[0][0]),
                               float(ref.matrixCartToFrac.comp_list[0][0]))
        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[0][1]),
                               float(ref.matrixCartToFrac.comp_list[0][1]))
        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[0][2]),
                               float(ref.matrixCartToFrac.comp_list[0][2]))

        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[1][0]),
                               float(ref.matrixCartToFrac.comp_list[1][0]))
        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[1][1]),
                               float(ref.matrixCartToFrac.comp_list[1][1]))
        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[1][2]),
                               float(ref.matrixCartToFrac.comp_list[1][2]))

        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[2][0]),
                               float(ref.matrixCartToFrac.comp_list[2][0]))
        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[2][1]),
                               float(ref.matrixCartToFrac.comp_list[2][1]))
        self.assertAlmostEqual(float(val.matrixCartToFrac.comp_list[2][2]),
                               float(ref.matrixCartToFrac.comp_list[2][2]))

        # ------------------------------------
        self.assertAlmostEqual(float(val.vectorFracToCart.comp_list[0]),
                               float(ref.vectorFracToCart.comp_list[0]))
        self.assertAlmostEqual(float(val.vectorFracToCart.comp_list[1]),
                               float(ref.vectorFracToCart.comp_list[1]))
        self.assertAlmostEqual(float(val.vectorFracToCart.comp_list[2]),
                               float(ref.vectorFracToCart.comp_list[2]))

        self.assertAlmostEqual(float(val.vectorCartToFrac.comp_list[0]),
                               float(ref.vectorCartToFrac.comp_list[0]))
        self.assertAlmostEqual(float(val.vectorCartToFrac.comp_list[1]),
                               float(ref.vectorCartToFrac.comp_list[1]))
        self.assertAlmostEqual(float(val.vectorCartToFrac.comp_list[2]),
                               float(ref.vectorCartToFrac.comp_list[2]))

        #print(ref.unitCellRecipLengths.comp_dict)
        #print(cryst.cifOutput.unitCellRecipLengths.comp_dict)
        #self.fail("rrrrrrrrrrrrrrrrrrrrrrrrrr")

        # === end of TestCrystalInfo.test_load_cif_reciprocal()

        # === end of TestCrystalInfo.()

    # === end of class TestCrystalInfo

if __name__ == "__main__":
    unittest.main()
