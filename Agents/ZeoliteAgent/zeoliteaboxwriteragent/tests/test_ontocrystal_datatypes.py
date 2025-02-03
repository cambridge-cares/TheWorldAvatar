"""
Unit testing for Data Types defined in OntoCrystal ontology.

TODO:
- Measure:
  - different prefixes to the values
- OntoMatrix:
  - addComponent()
  - addComponent()
  - get_csv_arr()
- OntoPlot:
  - add() ???
  - get_csv_arr()
-

DONE:
- Unit tests for OntoMeasureWithUncertainty
  - setValue()
  - get_csv_arr()
- DONE OntoVector:
  - DONE addComponent()
  - DONE addComponentList()
  - DONE get_csv_arr()


"""

import unittest
from unittest.mock import patch, call

import sys
from io import StringIO
import logging

import tools
import ontocrystal_datatypes as ocdt

# from ontocrystal_datatypes import *
# from csv_maker import *

ONTOCRYSTAL_PREFIX = "http://www.theworldavatar.com/kg/ontocrystal/"

class TestMeasureWithUncertainty(unittest.TestCase):
    """
    Main functions:
    - assign value/error/unit,
    - get values as .csv row (i.e. one line in .csv file)
    """

    def test_setValue(self):
        # Example of use for Measure-With-Uncertainty
        # ================================================

        uuidDB = tools.UuidDB(filename="test_datatypes.csv")
        # ------------------------------------------------

        # ------------------------------------------------
        # Only value (no error, no unit)
        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4)

        self.assertEqual(vol.value, 123.4)
        self.assertEqual(vol.error, None)
        self.assertEqual(vol.unit,  None)

        # ------------------------------------------------
        # Only value + unit (no error)
        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4, unit="om:cubicAngstrom")

        self.assertEqual(vol.value, 123.4)
        self.assertEqual(vol.error,  None)
        self.assertEqual(vol.unit, "om:cubicAngstrom")

        # ------------------------------------------------
        # Only value + error (no unit)
        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4, error=5.6)

        self.assertEqual(vol.value, 123.4)
        self.assertEqual(vol.error,   5.6)
        self.assertEqual(vol.unit,  None)

        # ------------------------------------------------
        # Only value + error + unit
        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4, error=5.6, unit="om:cubicAngstrom")

        self.assertEqual(vol.value, 123.4)
        self.assertEqual(vol.error,   5.6)
        self.assertEqual(vol.unit, "om:cubicAngstrom")
        # ------------------------------------------------

        # self.assertEqual(,)

        uuidDB.saveDB()
        # === end of TestMeasureWithUncertainty.test_setValue()

    def test_get_csv_arr(self):
        uuidDB = tools.UuidDB(filename="test_datatypes.csv")

        prefix = "http://test/ontology/"
        # output += line

        # ------------------------------------------------
        # Set only value (no error, no units)

        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4)

        lines = vol.get_csv_arr("HotPot", prefix + "hasVolume")

        # for only value assigned:
        self.assertEqual(len(lines), 3)

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        new = line[0]  # The new entity (prefix + name + UUID)
        self.assertEqual(line[0].startswith("http://abox/Volume_Test_"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], "http://tbox/CellVolume")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], "HotPot")
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], prefix + "hasVolume")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocdt.omOntoPrefix + "hasNumericalValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 123.4)
        self.assertEqual(line[5], "xsd:decimal")

        # ------------------------------------------------
        # Set value and error (no units)

        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4, error=5.6)

        lines = vol.get_csv_arr("HotPot", prefix + "hasVolume")

        # for value + error assigned:
        self.assertEqual(len(lines), 4)

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        new = line[0]  # The new entity (prefix + name + UUID)
        self.assertEqual(line[0].startswith("http://abox/Volume_Test_"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], "http://tbox/CellVolume")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], "HotPot")
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], prefix + "hasVolume")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocdt.omOntoPrefix + "hasNumericalValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 123.4)
        self.assertEqual(line[5], "xsd:decimal")

        # Error assigned to the new entity
        line = lines[3]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocdt.crystOntoPrefix + "hasUncertaintyValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 5.6)
        self.assertEqual(line[5], "xsd:decimal")

        # ------------------------------------------------
        # Set value and units (no error)

        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4, unit="om:cubicAngstrom")

        lines = vol.get_csv_arr("HotPot", prefix + "hasVolume")

        # for value + unit assigned:
        self.assertEqual(len(lines), 4)

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        new = line[0]  # The new entity (prefix + name + UUID)
        self.assertEqual(line[0].startswith("http://abox/Volume_Test_"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], "http://tbox/CellVolume")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], "HotPot")
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], prefix + "hasVolume")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocdt.omOntoPrefix + "hasNumericalValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 123.4)
        self.assertEqual(line[5], "xsd:decimal")

        # Linking the unit of measure to the new entity
        line = lines[3]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[0], new)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], ocdt.omOntoPrefix + "cubicAngstrom")
        self.assertEqual(line[3], ocdt.omOntoPrefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # ------------------------------------------------
        # Set value, error and units
        vol = ocdt.OntoMeasureWithUncertainty(class_name="CellVolume",
                                              item_name="Volume_Test",
                                              tbox_prefix="http://tbox/",
                                              abox_prefix="http://abox/",
                                              uuidDB=uuidDB)

        vol.setValue(value=123.4, error=5.6, unit="om:cubicAngstrom")

        lines = vol.get_csv_arr("HotPot", prefix + "hasVolume")

        # for value + error + unit assigned:
        self.assertEqual(len(lines), 5)

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        new = line[0]  # The new entity (prefix + name + UUID)
        self.assertEqual(line[0].startswith("http://abox/Volume_Test_"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], "http://tbox/CellVolume")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], "HotPot")
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], prefix + "hasVolume")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocdt.omOntoPrefix + "hasNumericalValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 123.4)
        self.assertEqual(line[5], "xsd:decimal")

        # Error assigned to the new entity
        line = lines[3]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocdt.crystOntoPrefix + "hasUncertaintyValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[2], new)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 5.6)
        self.assertEqual(line[5], "xsd:decimal")

        # Linking the unit of measure to the new entity
        line = lines[4]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0].startswith("http://abox/Volume_Test_"),
                         True, msg=line[2])
        self.assertEqual(line[0], new)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], ocdt.omOntoPrefix + "cubicAngstrom")
        self.assertEqual(line[3], ocdt.omOntoPrefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        tools.writeCsv("TestOntoCrystal3.csv", lines)
        # ------------------------------------------------

        uuidDB.saveDB()
        # === end of TestMeasureWithUncertainty.get_csv_arr()

    # def test_a
    #    pass # TestMeasureWithUncertainty.
    #    pass # TestMeasureWithUncertainty.

    # === end of class TestMeasureWithUncertainty


class TestOntoVector(unittest.TestCase):
    """
    Main functions:
    - addComponent (i.e. assign value/error/unit),
    - get values as .csv row (i.e. one line in .csv file)
    """
    def setUp(self):
        self.uuidDB = tools.UuidDB(filename="test_datatypes_uuid.csv")
        # === end of TestOntoVector.()

    def tearDown(self):
        self.uuidDB.saveDB()
        # === end of TestOntoVector.()

    def test_constructor_default_all(self):

        #original_stderr = sys.stderr
        #sys.stderr = StringIO()
        vec1 = ocdt.OntoVector(item_name="pos_mol_C1",
                               uuidDB=self.uuidDB)
        #sys.stderr = original_stderr 

        self.assertEqual(vec1.class_name, "Vector")

        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)

        self.assertEqual(vec1.unit, None)
        self.assertEqual(vec1.vector_label, None )
        
        self.assertEqual(vec1.tbox_prefix, ONTOCRYSTAL_PREFIX)
        self.assertEqual(vec1.abox_prefix, "")

        self.assertIsInstance(vec1.uuidDB, tools.UuidDB)

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_dict), 0)  # Expect empty dict

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 0)  # Expect empty list
        # === end of TestOntoVector.()

    def test_constructor_set_tbox(self):
        vec1 = ocdt.OntoVector(item_name="pos_mol_C1",
                               tbox_prefix="http://tbox/",
                               uuidDB=self.uuidDB)

        self.assertEqual(vec1.class_name, "Vector")

        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)

        self.assertEqual(vec1.unit, None)
        self.assertEqual(vec1.vector_label, None )
        
        self.assertEqual(vec1.tbox_prefix, "http://tbox/")
        #self.assertEqual(vec1.abox_prefix, ONTOCRYSTAL_PREFIX)
        self.assertEqual(vec1.abox_prefix, "")

        self.assertIsInstance(vec1.uuidDB, tools.UuidDB)

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_dict), 0)  # Expect empty dict

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 0)  # Expect empty list
        # === end of TestOntoVector.()

    def test_constructor_set_abox(self):
        vec1 = ocdt.OntoVector(item_name="pos_mol_C1",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB)

        self.assertEqual(vec1.class_name, "Vector")

        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)

        self.assertEqual(vec1.unit, None)
        self.assertEqual(vec1.vector_label, None )
        
        self.assertEqual(vec1.tbox_prefix, ONTOCRYSTAL_PREFIX)
        self.assertEqual(vec1.abox_prefix, "http://abox/")

        self.assertIsInstance(vec1.uuidDB, tools.UuidDB)

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_dict), 0)  # Expect empty dict

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 0)  # Expect empty list
        # === end of TestOntoVector.()

    def test_constructor_set_class(self):

        vec1 = ocdt.OntoVector(item_name="pos_mol_C1",
                               class_name="my_class",
                               uuidDB=self.uuidDB)

        self.assertEqual(vec1.class_name, "my_class")

        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)

        self.assertEqual(vec1.unit, None)
        self.assertEqual(vec1.vector_label, None )
        
        self.assertEqual(vec1.tbox_prefix, ONTOCRYSTAL_PREFIX)
        self.assertEqual(vec1.abox_prefix, "")

        self.assertIsInstance(vec1.uuidDB, tools.UuidDB)

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_dict), 0)  # Expect empty dict

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 0)  # Expect empty list
        # === end of TestOntoVector.()

    def test_constructor_set_unit(self):

        vec1 = ocdt.OntoVector(item_name="pos_mol_C1",
                               unit="angstrom",
                               uuidDB=self.uuidDB)

        self.assertEqual(vec1.class_name, "Vector")

        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)

        self.assertEqual(vec1.unit, "angstrom")
        self.assertEqual(vec1.vector_label, None)
        
        self.assertEqual(vec1.tbox_prefix, ONTOCRYSTAL_PREFIX)
        self.assertEqual(vec1.abox_prefix, "")

        self.assertIsInstance(vec1.uuidDB, tools.UuidDB)

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_dict), 0)  # Expect empty dict

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 0)  # Expect empty list
        # === end of TestOntoVector.()

    def test_constructor_set_uuid(self):

        vec1 = ocdt.OntoVector(item_name="pos_mol_C1",
    #                           uuid="my_uuid",
                               uuidDB=self.uuidDB)

        self.assertEqual(vec1.class_name, "Vector")

        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)

        self.assertEqual(vec1.unit, None)
        self.assertEqual(vec1.vector_label, None )
        
        self.assertEqual(vec1.tbox_prefix, ONTOCRYSTAL_PREFIX)
        self.assertEqual(vec1.abox_prefix, "")

        self.assertIsInstance(vec1.uuidDB, tools.UuidDB)

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_dict), 0)  # Expect empty dict

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 0)  # Expect empty list
        # === end of TestOntoVector.()

    def test_constructor_set_label(self):

        vec1 = ocdt.OntoVector(item_name="pos_mol_C1",
                               vector_label="LL",
                               uuidDB=self.uuidDB)

        self.assertEqual(vec1.class_name, "Vector")

        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)

        self.assertEqual(vec1.unit, None)
        self.assertEqual(vec1.vector_label, "LL")
        
        self.assertEqual(vec1.tbox_prefix, ONTOCRYSTAL_PREFIX)
        self.assertEqual(vec1.abox_prefix, "")

        self.assertIsInstance(vec1.uuidDB, tools.UuidDB)
        self.assertEqual(vec1.vector_label, "LL")

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_dict), 0)  # Expect empty dict

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 0)  # Expect empty list
        # === end of TestOntoVector.()

    def test_addCompDict(self):
        self.fail("not implemented")
        # === end of TestOntoVector.()

    def test_addComponentDict(self):

        # ------------------------------------------------
        vec1 = ocdt.OntoVector(class_name="PositionVector",
                               item_name="pos_mol_C1",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom",
                               vector_label="L")

        vec1.addComponent(label="x", value="111.22", error="1.2",
                          unit="om:degree")
        # vec1.addComponent(label = "y", value = "111.22",
        #                  unit = "om:angstrom")
        # vec1.addComponent(label = "xx", value = 111.44, error = 1.4,
        #                  unit = "om:degree")
        # lines = vec1.get_csv_arr("HotPot", prefix + "hasVec1")

        # for value + error + unit assigned:
        # self.assertEqual(len(lines), 5)
        self.assertEqual(vec1.class_name, "PositionVector")
        self.assertEqual(vec1.item_name.startswith("pos_mol_C1"),
                         True, msg=vec1.item_name)
        self.assertEqual(vec1.unit, "om:angstrom")
        # Vector label:
        self.assertEqual(vec1.vector_label, "L")

        self.assertIsInstance(vec1.comp_dict, dict)
        self.assertEqual(len(vec1.comp_list), 0)  # Because assigned to compDict
        self.assertEqual(len(vec1.comp_dict.keys()), 1)  # Expect 1 component

        self.assertEqual("x" in vec1.comp_dict, True)
        self.assertEqual(vec1.comp_dict["x"]["value"], "111.22")
        self.assertEqual(vec1.comp_dict["x"]["error"], "1.2")
        self.assertEqual(vec1.comp_dict["x"]["unit"], "om:degree")

        # ------------------------------------------------
        vec2 = ocdt.OntoVector(class_name="PositionVector",
                               item_name="pos_mol_C2",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom")

        vec2.addComponent(label="x", value="111.22", error="1.2",
                          unit="om:degree")
        vec2.addComponent(label="y", value=111.44, error=1.4,
                          unit="om:cubicAngstrom")
        # vec2.addComponent(label = "z", value = "111.22",
        #                   unit = "om:angstrom")

        self.assertEqual(vec2.class_name, "PositionVector")
        self.assertEqual(vec2.item_name.startswith("pos_mol_C2"),
                         True, msg=vec1.item_name)
        self.assertEqual(vec2.unit, "om:angstrom")
        self.assertEqual(vec2.vector_label, None)

        self.assertIsInstance(vec2.comp_dict, dict)
        self.assertEqual(len(vec2.comp_list), 0)  # Because assigned to compDict
        self.assertEqual(len(vec2.comp_dict.keys()), 2)  # Expect 1 component

        self.assertEqual("x" in vec2.comp_dict, True)
        self.assertEqual(vec2.comp_dict["x"]["value"], "111.22")
        self.assertEqual(vec2.comp_dict["x"]["error"], "1.2")
        self.assertEqual(vec2.comp_dict["x"]["unit"], "om:degree")

        self.assertEqual("y" in vec2.comp_dict, True)
        self.assertEqual(vec2.comp_dict["y"]["value"], "111.44")
        self.assertEqual(vec2.comp_dict["y"]["error"], "1.4")
        self.assertEqual(vec2.comp_dict["y"]["unit"], "om:cubicAngstrom")

        # ------------------------------------------------
        # TODO add OntoVector with or without tPrefix and aPrefix

        # ------------------------------------------------
        # TODO OntoVector with some inputs skipped (value/error/unit)

        # ------------------------------------------------

        # ------------------------------------------------
        # ------------------------------------------------
        # ------------------------------------------------
        # === end of TestOntoVector.addComponentDict()

    def test_get_csv_arr_dict(self):

        om_prefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
        ocr_prefix = "http://www.theworldavatar.com/kg/ontocrystal/"
        prefix = "http://test/ontology/"

        #uuidDB = tools.UuidDB(filename="test_datatypes.csv")

        # ------------------------------------------------
        vec1 = ocdt.OntoVector(class_name="PositionVector",
                               item_name="pos_mol_C3",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom",
                               vector_label="L")

        vec1.addComponent(label="x", value="111.22", error="1.2",
                          unit="om:degree", index=3)
        # vec1.addComponent(label = "y", value = "111.22",
        #                  unit = "om:angstrom")
        # vec1.addComponent(label = "xx", value = 111.22, error = 1.2,
        #                  unit = "om:degree")
        lines = vec1.get_csv_arr("HotPot", prefix + "hasVec1")

        # For debugging:
        # tools.writeCsv("TestOntoCrystal3.csv", lines)

        # Defintion of the entity of the class
        il = 0
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        vec = line[0]  # The new entity (prefix + name + UUID)
        self.assertEqual(line[0].startswith("http://abox/pos_mol_C3_"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], "http://tbox/PositionVector")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Relation between the new entity and the subject
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], "HotPot")
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], vec)
        self.assertEqual(line[3], prefix + "hasVec1")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Global unit vector:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], vec)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], om_prefix + "angstrom")
        self.assertEqual(line[3], om_prefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Checking Vector label:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasVectorLabel")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], vec)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "L")
        self.assertEqual(line[5], "xsd:string")

        # Component 1:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        comp1 = line[0]
        self.assertEqual(line[0].startswith("http://abox/pos_mol_C3_comp_x_"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], ocr_prefix + "VectorComponent")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], vec, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], ocr_prefix + "hasVectorComponent")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], comp1)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], om_prefix + "degree")
        self.assertEqual(line[3], om_prefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[4], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentLabel")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "x")
        self.assertEqual(line[5], "xsd:string")

        # Reminder:
        # vec1.addComponent(label = "x", value = "111.22", error = "1.2",
        #                   unit = "om:degree")
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "111.22")
        self.assertEqual(line[5], "rdfs:Literal")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentUncertainty")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "1.2")
        self.assertEqual(line[5], "rdfs:Literal")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentIndex")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 3)
        self.assertEqual(line[5], "xsd:integer")

        # for value + error + unit assigned:
        # 2 lines : for vector definition
        # 1 line  : for global vector label
        # 1 line  : for global unit
        # 6 lines per component (2 + (label,value,error,unit))
        # +1 for index (optional argument)
        self.assertEqual(len(lines), 11)

        # tools.writeCsv("TestOntoCrystal4.csv", lines)

        #self.uuidDB.saveDB()

        # self.assertEqual(1, 2)
        # === end of TestOntoVector.get_csv_arr_dict()

    """
        #------------------------------------------------
        vec1 = ocdt.OntoVector(class_name = "PositionVector",
                               item_name = "pos_mol_C1", \
                               tbox_prefix = "http://tbox/",
                               abox_prefix = "http://abox/", \
                               uuidDB = uuidDB, unit = "om:angstrom",
                               vector_label = "L" \
                              )

        vec1.addComponent(label = "x", value = "111.22", error = "1.2",
                          unit = "om:degree")
        vec1.addComponent(label = "y", value = "111.22", unit = "om:angstrom")

        #------------------------------------------------
        '''
        vec = ocdt.OntoVector(class_name = "PositionVector",
                              item_name = "pos_mol_C1",
                              tbox_prefix = "http://tbox/",
                              abox_prefix = "http://abox/",
                              uuidDB = uuidDB, unit = "om:angstrom",
                              vector_label = "L"
                             )

        vec.addComponent(label = "x", value = "111.22", error = "1.2",
                         unit = "om:degree")
        '''
        #------------------------------------------------

        #------------------------------------------------
        vec2 = ocdt.OntoVector(class_name = "RadiusVector",
                               item_name = "pos_mol_C2",
                               tbox_prefix = "http://tbox/",
                               abox_prefix = "http://abox/",
                               uuidDB = uuidDB, unit = "om:angstrom",
                               vector_label = "L"
                              )

        self.assertEqual(1, 2)
        # FIXME the followint function does not work
        vec2.addComponentList(valList = [1.1, 2.2, 3.3 ],
                               errList = [0.2, 0.3, 0.4 ], unit = "degree")
        output += vec2.get_csv_arr("HotPot", prefix + "hasVec2")

        #------------------------------------------------
        vec3 = ocdt.OntoVector(class_name = "RadiusVector",
                               item_name = "pos_mol_C3",
                               tbox_prefix = "http://tbox/",
                               abox_prefix = "http://abox/",
                               uuidDB = uuidDB, unit = "om:angstrom",
                               vector_label = "L"
                              )

        vec3.addComponentList(valList = [6.1, 7.2, 8.3 ],
                               errList = [0.3, 0.2, 0.1 ], unit = "om:degree")
        output += vec3.get_csv_arr("HotPot", prefix + "hasVecList")

    """

    def test_addComponentList(self):

        uuidDB = tools.UuidDB(filename="test_datatypes.csv")
        # ------------------------------------------------

        vec2 = ocdt.OntoVector(class_name="RadiusVector",
                               item_name="pos_mol_C4",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=uuidDB, unit="om:angstrom",
                               vector_label="L")

        vec2.addComponentList(valList=[1.1, 2.2, 3.3],
                              errList=[0.2, 0.3, 0.4],
                              unit=["om:degree", "om:cubicAngstrom",
                              "om:dimensionOne"])

        # output += vec2.get_csv_arr("HotPot", prefix + "hasVec2")

        self.assertIsInstance(vec2.comp_list, list)
        self.assertEqual(len(vec2.comp_list), 3)
        self.assertEqual(len(vec2.comp_err_list), 3)
        self.assertEqual(len(vec2.comp_unit_list), 3)
        self.assertEqual(len(vec2.comp_dict), 0)

        self.assertEqual(vec2.unit, "om:angstrom")
        self.assertEqual(vec2.vector_label, "L")

        self.assertEqual(vec2.comp_list[0], 1.1)
        self.assertEqual(vec2.comp_list[1], 2.2)
        self.assertEqual(vec2.comp_list[2], 3.3)

        self.assertEqual(vec2.comp_err_list[0], 0.2)
        self.assertEqual(vec2.comp_err_list[1], 0.3)
        self.assertEqual(vec2.comp_err_list[2], 0.4)

        self.assertEqual(vec2.comp_unit_list[0], "om:degree")
        self.assertEqual(vec2.comp_unit_list[1], "om:cubicAngstrom")
        self.assertEqual(vec2.comp_unit_list[2], "om:dimensionOne")

        # ------------------------------------------------
        # ------------------------------------------------

        # uuidDB.saveDB()
        # self.assertEqual(1, 2)
        # === end of TestOntoVector.addComponentList()

    def test_get_csv_arr_list(self):

        om_prefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
        ocr_prefix = "http://www.theworldavatar.com/kg/ontocrystal/"

        prefix = "http://test/ontology/"
        uuidDB = tools.UuidDB(filename="test_datatypes.csv")

        # ------------------------------------------------
        vec1 = ocdt.OntoVector(class_name="RadiusVector",
                               item_name="pos_mol_C5",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom",
                               vector_label="L")

        vec1.addComponentList(valList=[6.1, 7.2, 8.3],
                              errList=[0.3, 0.2, 0.1], unit="om:degree")

        lines = vec1.get_csv_arr("HotPot", prefix + "hasVecList")

        # For debugging:
        # tools.writeCsv("TestOntoCrystal2.csv", lines)

        self.assertIsInstance(vec1.comp_list, list)
        self.assertEqual(len(vec1.comp_list), 3)  # Because assigned to compDict
        self.assertEqual(len(vec1.comp_dict.keys()), 0)  # Expect 1 component

        # Defintion of the entity of the class
        il = 0
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        vec = line[0] # The new entity of class Vector (prefix + name + UUID)
        self.assertEqual(line[0].startswith("http://abox/pos_mol_C5_"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], "http://tbox/RadiusVector")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Relation between the new entity and the subject
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], "HotPot")
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2].startswith("http://abox/pos_mol_C5_"),
                         True, msg=line[2])
        self.assertEqual(line[2], vec)
        self.assertEqual(line[3], prefix + "hasVecList")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Global unit vector:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], vec)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], om_prefix + "angstrom")
        self.assertEqual(line[3], om_prefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Checking Vector label:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasVectorLabel")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], vec)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "L")
        self.assertEqual(line[5], "xsd:string")

        # Component 1:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        comp1 = line[0]
        self.assertTrue(line[0].startswith("http://abox/pos_mol_C5_comp_1_"),
                        msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], ocr_prefix + "VectorComponent")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], vec, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], ocr_prefix + "hasVectorComponent")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], comp1)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], om_prefix + "degree")
        self.assertEqual(line[3], om_prefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[4], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentIndex")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 1)
        self.assertEqual(line[5], "xsd:integer")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 6.1)
        self.assertEqual(line[5], "rdfs:Literal")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentUncertainty")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 0.3)
        self.assertEqual(line[5], "rdfs:Literal")

        # Component 2:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        comp2 = line[0]
        self.assertTrue(line[0].startswith("http://abox/pos_mol_C5_comp_2_"),
                        msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], ocr_prefix + "VectorComponent")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], vec, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], comp2)
        self.assertEqual(line[3], ocr_prefix + "hasVectorComponent")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], comp2)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], om_prefix + "degree")
        self.assertEqual(line[3], om_prefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentIndex")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp2)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 2)
        self.assertEqual(line[5], "xsd:integer")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp2)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 7.2)
        self.assertEqual(line[5], "rdfs:Literal")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentUncertainty")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp2)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 0.2)
        self.assertEqual(line[5], "rdfs:Literal")

        # Component 3 (skipping 6 lines)
        il += 6

        # Reminder:
        # vec1.addComponentList(valList = [6.1, 7.2, 8.3 ],
        #                       errList = [0.3, 0.2, 0.1 ], unit = "om:degree")

        # Total number of lines for value + error + unit assigned:
        #   2 lines : for vector definition
        #   1 line  : for global unit
        #   1 line  : for vector label
        #   3 * 6   : 3 components with 6 lines each (2+index,value,error,unit)
        self.assertEqual(len(lines), 22)

        # ------------------------------------------------

        uuidDB.saveDB()
        # ------------------------------------------------

        # === end of TestOntoVector.get_csv_arr_list()

    # === end of class TestOntoVector


class TestOntoMatrix(unittest.TestCase):
    """
    FIXME
    Main functions:
    - addComponent (i.e. assign value/error/unit),
    - get values as .csv row (i.e. one line in .csv file)
    """
    def setUp(self):
        self.uuidDB = tools.UuidDB(filename="test_datatypes_uuid.csv")
        # === end of TestOntoMatrix.setUp()

    def tearDown(self):
        self.uuidDB.saveDB()
        # === end of TestOntoMatrix.tearDown()


    def test_addComponentList(self):

        #uuidDB = tools.UuidDB(filename="test_datatypes_uuid.csv")

        # ------------------------------------------------
        mat1 = ocdt.OntoMatrix(class_name="NewMatrix",
                               item_name="pos_mol_C1",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom"
                               # , vector_label = "L" \
                              )

        mat1.addComponentList(  # label = "xx",
                              valList=[[111.22, 222.33]],
                              errList=[[1.2,      2.3]], unit="om:degree")
        # mat1.addComponent(label = "y", value = "111.22",
        #                   unit = "om:angstrom")
        # mat1.addComponent(label = "xx", value = 111.44, error = 1.4,
        #                    unit = "om:degree")
        # lines = mat1.get_csv_arr("HotPot", prefix + "hasVec1")

        self.assertEqual(mat1.class_name, "NewMatrix")
        self.assertEqual(mat1.item_name.startswith("pos_mol_C1"),
                         True, msg=mat1.item_name)
        self.assertEqual(mat1.unit, "om:angstrom")

        # is there matrix label???:
        # self.assertEqual(mat1.vector_label, "L")

        self.assertIsInstance(mat1.comp_list, list)
        self.assertEqual(len(mat1.comp_list), 1)  # Expect 1 (array 1x2)
        self.assertEqual(len(mat1.comp_list[0]), 2)  # Expect 2 (array 1x2)
        self.assertEqual(len(mat1.comp_dict.keys()), 0)  # Because assigned to compList

        # Component 1
        self.assertEqual(mat1.comp_list    [0][0], 111.22)
        self.assertEqual(mat1.comp_err_list [0][0],   1.2)
        self.assertEqual(mat1.comp_unit_list[0][0], "om:degree")

        # Component 2
        self.assertEqual(mat1.comp_list    [0][1], 222.33)
        self.assertEqual(mat1.comp_err_list [0][1],   2.3)
        self.assertEqual(mat1.comp_unit_list[0][1], "om:degree")

        # ------------------------------------------------
        mat2 = ocdt.OntoMatrix(class_name="MeasureMatrix",
                               item_name="pos_mol_C1",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom"
                               # , vector_label = "L" \
                              )
        mat2.addComponentList(  # label = "xx",
                              valList=[[111.22,222.33]],
                              errList=[[1.2,  2.3]],
                              unit=[["om:degree","om:reciprocalAngstrom"]])
        # mat2.addComponent(label = "y", value = "111.22",
        #                   unit = "om:angstrom")
        # mat2.addComponent(label = "xx", value = 111.44, error = 1.4,
        #                   unit = "om:degree")
        # lines = mat2.get_csv_arr("HotPot", prefix + "hasVec1")

        self.assertEqual(mat2.class_name, "MeasureMatrix")
        self.assertEqual(mat2.item_name.startswith("pos_mol_C1"), True,
                         msg=mat2.item_name)
        self.assertEqual(mat2.unit, "om:angstrom")

        # is there matrix label???:
        # self.assertEqual(mat2.vector_label, "L")

        self.assertIsInstance(mat2.comp_list, list)
        self.assertEqual(len(mat2.comp_list), 1)  # Expect 1 (array 1x2)
        self.assertEqual(len(mat2.comp_list[0]), 2)  # Expect 2 (array 1x2)
        self.assertEqual(len(mat2.comp_dict.keys()), 0)  # Because assigned to compList

        # Component 1
        self.assertEqual(mat2.comp_list    [0][0], 111.22)
        self.assertEqual(mat2.comp_err_list [0][0],   1.2)
        self.assertEqual(mat2.comp_unit_list[0][0], "om:degree")

        # Component 2
        self.assertEqual(mat2.comp_list    [0][1], 222.33)
        self.assertEqual(mat2.comp_err_list [0][1],   2.3)
        self.assertEqual(mat2.comp_unit_list[0][1], "om:reciprocalAngstrom")

        # ------------------------------------------------
        """
        # This case is here for checking error/warning messages:

        mat3 = ocdt.OntoMatrix(class_name = "MeasureMatrix",
                               item_name = "pos_mol_C1", \
                               tbox_prefix = "http://tbox/",
                               abox_prefix = "http://abox/", \
                               uuidDB = self.uuidDB, unit = "om:angstrom"
                               #, vector_label = "L" \
                             )

        mat3.addComponentList(valList = [[111.22,222.33]], \
                               errList = [[  1.2 ,  2.3 ],[3.4]], \
                               unit = [["om:degree","om:reciprocalAngstrom"]])
        mat3.addComponentList(valList = [[111.22,222.33]], \
                               errList = [[  1.2 ,  2.3, 3.4]], \
                               unit = [["om:degree","om:reciprocalAngstrom"]])
        """
        # ------------------------------------------------

        # self.assertEqual(1, 2, msg="Not implemented test matrix addComponentList")
        #uuidDB.saveDB()
        # === end of TestOntoMatrix.addComponentList()

    def test_addComponentDict(self):

        # ------------------------------------------------
        mat1 = ocdt.OntoMatrix(class_name="PositionVector",
                               item_name="pos_mol_C1",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom"
                               # , vector_label = "L" \
                              )

        mat1.addComponent(label="xx", value="111.22", error="1.2",
                          unit="om:degree")
        mat1.addComponent(label="yy", value="222.33", error="2.3",
                          unit="om:cubicAngstrom")
        # mat1.addComponent(label = "xx", value = 111.44, error = 1.4,
        #                   unit = "om:degree")
        # lines = mat1.get_csv_arr("HotPot", prefix + "hasVec1")

        self.assertEqual(mat1.class_name, "PositionVector")
        self.assertTrue(mat1.item_name.startswith("pos_mol_C1"),
                        msg=mat1.item_name)
        self.assertEqual(mat1.unit, "om:angstrom")

        # is there matrix label???:
        # self.assertEqual(mat1.vector_label, "L")

        self.assertIsInstance(mat1.comp_dict, dict)
        self.assertEqual(len(mat1.comp_list), 0) # Because assigned to compDict
        self.assertEqual(len(mat1.comp_dict.keys()), 2) # Expect 1 component

        # Component 1
        self.assertEqual("xx" in mat1.comp_dict, True)
        self.assertEqual(mat1.comp_dict["xx"]["value"], "111.22")
        self.assertEqual(mat1.comp_dict["xx"]["error"],   "1.2")
        self.assertEqual(mat1.comp_dict["xx"]["unit"], "om:degree")

        # Component 2
        self.assertEqual("yy" in mat1.comp_dict, True)
        self.assertEqual(mat1.comp_dict["yy"]["value"], "222.33")
        self.assertEqual(mat1.comp_dict["yy"]["error"],   "2.3")
        self.assertEqual(mat1.comp_dict["yy"]["unit"], "om:cubicAngstrom")

        # ------------------------------------------------

        # ------------------------------------------------
        # ------------------------------------------------

        # self.assertEqual(1, 2, msg="Not implemented test matrix addComponentDict")
        #uuidDB.saveDB()
        # === end of TestOntoMatrix.addComponentDict()

    def test_get_csv_arr_list(self):

        om_prefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
        ocr_prefix = "http://www.theworldavatar.com/kg/ontocrystal/"

        prefix = "http://test/ontology/"

        mat1 = ocdt.OntoMatrix(class_name="TestMatrix", item_name="mat_v1",
                               tbox_prefix="http://tbox/",
                               abox_prefix="http://abox/",
                               uuidDB=self.uuidDB, unit="om:angstrom"
                               # , vector_label = "L" \
                              )

        mat1.addComponentList(# label = "xx",
                              valList=[[111.22, 222.33], [333.44, 444.55]],
                              errList=[[1.2, 2.3], [3.4, 4.5]],
                              unit=[["om:degree", "om:reciprocalAngstrom"],
                                    ["om:angstrom", "om:cubicAngstrom"]])

        lines = mat1.get_csv_arr("HotPot", prefix + "hasMat1", new_uuid="new_uuid")

        # For debugging:
        # tools.writeCsv("TestOntoCrystal6.csv", lines)

        self.assertIsInstance(mat1.comp_list, list)
        self.assertEqual(len(mat1.comp_list), 2)  # Because assigned 2 rows
        self.assertEqual(len(mat1.comp_list[0]), 2)  # Because assigned 2 components per row
        self.assertEqual(len(mat1.comp_dict.keys()), 0)  # Expect empty dictionary

        # Defintion of the entity of the class
        il = 0
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        vec = line[0]  # The new entity of class Vector (prefix + name + UUID)
        self.assertEqual(line[0].startswith("http://abox/mat_v1"),
                         True, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], "http://tbox/TestMatrix")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Relation between the new entity and the subject
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], "HotPot")
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2].startswith("http://abox/mat_v1"),
                         True, msg=line[2])
        self.assertEqual(line[2], vec)
        self.assertEqual(line[3], prefix + "hasMat1")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Global unit vector:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], vec)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], om_prefix + "angstrom")
        self.assertEqual(line[3], om_prefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        # Component 1:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        comp1 = line[0]
        #self.assertTrue(line[0].startswith("http://abox/mat_v1_comp_1_new_uuid"),
        self.assertTrue(line[0].startswith("http://abox/MatrixComponent_1_1_new_uuid"),
                        msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], ocr_prefix + "MatrixComponent")
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], vec, msg=line[0])
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], ocr_prefix + "hasMatrixComponent")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], comp1)
        self.assertEqual(line[1], "Instance")
        self.assertEqual(line[2], om_prefix + "degree")
        self.assertEqual(line[3], om_prefix + "hasUnit")
        self.assertEqual(line[4], "")
        self.assertEqual(line[5], "")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasRowIndex")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 1)
        self.assertEqual(line[5], "xsd:integer")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasColumnIndex")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 1)
        self.assertEqual(line[5], "xsd:integer")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentValue")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 111.22)
        self.assertEqual(line[5], "rdfs:Literal")

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual(str(line[i]).strip(), str(line[i]))
        self.assertEqual(line[0], ocr_prefix + "hasComponentUncertainty")
        self.assertEqual(line[1], "Data Property")
        self.assertEqual(line[2], comp1)
        self.assertEqual(line[3], "")
        self.assertEqual(line[4], 1.2)
        self.assertEqual(line[5], "rdfs:Literal")

        # Component 2:
        # TODO
        il += 1
        self.assertEqual(lines[il], ["http://abox/" + "MatrixComponent_1_2_new_uuid",
            "Instance", ocr_prefix + "MatrixComponent", "", "", ""])

        il += 1
        self.assertEqual(lines[il], ["http://abox/" + "mat_v1_new_uuid", "Instance",
                                     "http://abox/" + "MatrixComponent_1_2_new_uuid",
                                     ocr_prefix + "hasMatrixComponent", "", ""])

        il += 1
        self.assertEqual(lines[il], [ocr_prefix + "hasComponentUncertainty",
        "", ""])

        #self.assertEqual(1, 2, msg="Not implemented matrix get_csv_arr_list")
        # === end of TestOntoMatrix.get_csv_arr_list()


    def test_get_csv_arr_dict(self):
        #self.assertEqual(1, 2,
        #                 msg="Not implemented test matrix get_csv_arr_dict")

        prefix = "http://test/ontology/"

        mat1 = ocdt.OntoMatrix(class_name = "PositionVector",
                               item_name = "pos_mol_C1",
                               tbox_prefix = "http://tbox/",
                               abox_prefix = "http://abox/",
                               uuidDB = self.uuidDB, unit = "om:angstrom" #,
                               #vector_label = "L"
                              )

        mat1.addComponent(label = "xx", value = "111.22", error = "1.2",
                          unit = "om:degree")
        lines = mat1.get_csv_arr("HotPot", prefix + "hasMat1", new_uuid="new_uuid")

        for i_line in range(len(lines)):
            line = lines[i_line]
            for i in range(6):
                self.assertEqual(str(line[i]).strip(), str(line[i]), 
                    msg=f"i_line = {i_line}, col = {i}, val = '{line[i]}'.")

        i = 0
        line = lines[i]
        self.assertEqual(line, ["http://abox/pos_mol_C1_new_uuid", "Instance",
                                "http://tbox/PositionVector", "", "", ""])

        i = 1
        line = lines[i]
        self.assertEqual(line, ["HotPot", "Instance",
                                "http://abox/pos_mol_C1_new_uuid",
                                "http://test/ontology/hasMat1", "", ""])

        # For debugging:
        #tools.writeCsv("TestOntoCrystal7.csv", lines)

    """
        uuidDB.saveDB()
    """
        # === end of TestOntoMatrix.get_csv_arr_dict()

    # === end of class TestOntoMatrix


class TestOntoPlot(unittest.TestCase):

    def test_a(self):

        pass  # === end of TestOntoPlot.()

    # === end of class TestOntoPlot()


class TestOntoCrystal(unittest.TestCase):
    # Moved to separate module test_crystalinfo.

    def test_a(self):
        self.assertEqual(1, 1)
        uuidDB = tools.UuidDB(filename="test_datatypes.csv")
        output = []
        prefix = "http://test/ontology/"

        # print("Missing arrInitCsv() function ")

        # line = get_csv_arr_t("OntoTest",
        #                   "http://temp/t-box.owl",
        #                   "http://temp/a-box")
        # output += line
        # self.assertEqual(line[0], None)

        #self.fail("not implemented OntoCrystal test")

        uuidDB.saveDB()
        # === end of TestOntoCrystal.

    def test_get_csv_arr(self):
        uuidDB = tools.UuidDB(filename="test_datatypes.csv")

        #self.fail("not implemented OntoCrystal test")

        #uuidDB.saveDB()
        # === end of TestOntoCrystal.get_csv_arr()

    # === end of class TestOntoCrystal


if __name__ == "__main__":

    unittest.main()

    """
    for i,x in enumerate([1,2,3,4]):
        pass
    print(i)
    1/0
    """

    # ================================================
