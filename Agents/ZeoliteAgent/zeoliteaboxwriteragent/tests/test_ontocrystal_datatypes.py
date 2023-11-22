"""
Unit testing for Data Types defined in OntoCrystal ontology.

TODO:
- Measure:
  - different prefixes to the values
- OntoVector
  - addComponent()
  - addComponentList()
  - getCsvArr()
- OntoMatrix
  - addComponent()
  - addComponent()
  - getCsvArr()
- OntoPlot
  - add() ???
  - getCsvArr()
- 

DONE:
- Unit tests for OntoMeasureWithUncertainty
  - setValue()
  - getCsvArr()


"""

import unittest

from csv_maker import *
#import tools

class TestMeasureWithUncertainty( unittest.TestCase ):
    """
    Main functions:
    - assign value/error/unit, 
    - get values as .csv row (i.e. one line in .csv file)
    """

    def test_setValue( self ):
        # Example of use for Measure-With-Uncertainty
        #================================================

        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )
        #------------------------------------------------
        output = []


        #------------------------------------------------
        # Only value (no error, no unit)
        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4 )

        self.assertEqual( vol.value, 123.4 )
        self.assertEqual( vol.error, None )
        self.assertEqual( vol.unit,  None )

        #------------------------------------------------
        # Only value + unit (no error)
        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4, unit = "om:cubicAngstrom" )

        self.assertEqual( vol.value, 123.4 )
        self.assertEqual( vol.error,  None )
        self.assertEqual( vol.unit, "om:cubicAngstrom" )


        #------------------------------------------------
        # Only value + error (no unit)
        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4, error = 5.6 )

        self.assertEqual( vol.value, 123.4 )
        self.assertEqual( vol.error,   5.6 )
        self.assertEqual( vol.unit,  None )


        #------------------------------------------------
        # Only value + error + unit
        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4, error = 5.6, unit = "om:cubicAngstrom" )

        self.assertEqual( vol.value, 123.4 )
        self.assertEqual( vol.error,   5.6 )
        self.assertEqual( vol.unit, "om:cubicAngstrom" )
        #------------------------------------------------

        #self.assertEqual( , )

        uuidDB.saveDB()
        pass # TestMeasureWithUncertainty.test_setValue()

    def test_getCsvArr( self ):
        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        prefix = "http://test/ontology/"
        #print( "hello" )
        #output += line

        #------------------------------------------------
        # Set only value (no error, no units)

        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4 )

        lines = vol.getCsvArr( "HotPot", prefix + "hasVolume" )

        # for only value assigned:
        self.assertEqual( len(lines), 3 )

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        new = line[0] # The new entity (prefix + name + UUID)
        self.assertEqual( line[0].startswith( "http://abox/Volume_Test_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://tbox/CellVolume" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "HotPot" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], prefix + "hasVolume" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], omOntoPrefix + "hasNumericalValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 123.4 )
        self.assertEqual( line[5], "xsd:decimal" )

        #------------------------------------------------
        # Set value and error (no units)

        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4, error = 5.6 )

        lines = vol.getCsvArr( "HotPot", prefix + "hasVolume" )

        # for value + error assigned:
        self.assertEqual( len(lines), 4 )

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        new = line[0] # The new entity (prefix + name + UUID)
        self.assertEqual( line[0].startswith( "http://abox/Volume_Test_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://tbox/CellVolume" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "HotPot" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], prefix + "hasVolume" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], omOntoPrefix + "hasNumericalValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 123.4 )
        self.assertEqual( line[5], "xsd:decimal" )


        # Error assigned to the new entity
        line = lines[3]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], crystOntoPrefix + "hasUncertaintyValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 5.6 )
        self.assertEqual( line[5], "xsd:decimal" )

        #------------------------------------------------
        # Set value and units (no error)

        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4, unit = "om:cubicAngstrom" )

        lines = vol.getCsvArr( "HotPot", prefix + "hasVolume" )

        # for value + unit assigned:
        self.assertEqual( len(lines), 4 )

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        new = line[0] # The new entity (prefix + name + UUID)
        self.assertEqual( line[0].startswith( "http://abox/Volume_Test_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://tbox/CellVolume" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "HotPot" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], prefix + "hasVolume" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], omOntoPrefix + "hasNumericalValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 123.4 )
        self.assertEqual( line[5], "xsd:decimal" )

        # Linking the unit of measure to the new entity
        line = lines[3]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[0], new )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], omOntoPrefix + "cubicAngstrom" )
        self.assertEqual( line[3], omOntoPrefix + "hasUnit" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        #------------------------------------------------
        # Set value, error and units

        vol = OntoMeasureWithUncertainty( className = "CellVolume", 
                                          itemName  = "Volume_Test", 
                                          tPrefix   = "http://tbox/",
                                          aPrefix   = "http://abox/",
                                          uuidDB    = uuidDB
                                        )

        vol.setValue( value = 123.4, error = 5.6, unit = "om:cubicAngstrom" )

        lines = vol.getCsvArr( "HotPot", prefix + "hasVolume" )

        # for value + error + unit assigned:
        self.assertEqual( len(lines), 5 )

        # Defintion of the entity of the class
        line = lines[0]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        new = line[0] # The new entity (prefix + name + UUID)
        self.assertEqual( line[0].startswith( "http://abox/Volume_Test_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://tbox/CellVolume" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Relation between the new entity and the subject
        line = lines[1]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "HotPot" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], prefix + "hasVolume" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Values assigned to the new entity
        line = lines[2]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], omOntoPrefix + "hasNumericalValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 123.4 )
        self.assertEqual( line[5], "xsd:decimal" )

        # Error assigned to the new entity
        line = lines[3]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], crystOntoPrefix + "hasUncertaintyValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], new )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 5.6 )
        self.assertEqual( line[5], "xsd:decimal" )

        # Linking the unit of measure to the new entity
        line = lines[4]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[0], new )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], omOntoPrefix + "cubicAngstrom" )
        self.assertEqual( line[3], omOntoPrefix + "hasUnit" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        #------------------------------------------------

        uuidDB.saveDB()
        pass # TestMeasureWithUncertainty.getCsvArr()

    #def test_a
    #    pass # TestMeasureWithUncertainty.
    #    pass # TestMeasureWithUncertainty.

    pass #class TestMeasureWithUncertainty

class TestOntoVector( unittest.TestCase ):
    """
    Main functions:
    - addComponent (i.e. assign value/error/unit), 
    - get values as .csv row (i.e. one line in .csv file)
    """

    def test_addComponentDict( self ):
        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        #------------------------------------------------
        vec1 = OntoVector( className = "PositionVector", itemName = "pos_mol_C1", \
                           tPrefix = "http://tbox/", aPrefix = "http://abox/", \
                           uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L" \
                         )

        vec1.addComponent( label = "x", value = "111.22", error = "1.2", unit = "om:degree" )
        #vec1.addComponent( label = "y", value = "111.22", unit = "om:angstrom" )
        #vec1.addComponent( label = "xx", value = 111.44, error = 1.4, unit = "om:degree" )
        #lines = vec1.getCsvArr( "HotPot", prefix + "hasVec1" )

        
        # for value + error + unit assigned:
        #self.assertEqual( len(lines), 5 )
        self.assertEqual( vec1.className, "PositionVector" )
        self.assertEqual( vec1.itemName.startswith("pos_mol_C1"), True, msg = vec1.itemName )
        self.assertEqual( vec1.unit, "om:angstrom" )
        # Vector label:
        self.assertEqual( vec1.vectorLabel, "L" )

        self.assertEqual( len(vec1.compList), 0 ) # Because assigned to compDict
        self.assertEqual( len(vec1.compDict.keys()), 1 ) # Only 1 component

        self.assertEqual( "x" in vec1.compDict, True )
        self.assertEqual( vec1.compDict["x"]["value"], "111.22" )
        self.assertEqual( vec1.compDict["x"]["error"], "1.2" )
        self.assertEqual( vec1.compDict["x"]["unit" ], "om:degree" )


        #------------------------------------------------
        vec2 = OntoVector( className = "PositionVector", itemName = "pos_mol_C2", \
                           tPrefix = "http://tbox/", aPrefix = "http://abox/", \
                           uuidDB = uuidDB, unit = "om:angstrom" \
                         )

        vec2.addComponent( label = "x", value = "111.22", error = "1.2", unit = "om:degree" )
        vec2.addComponent( label = "y", value = 111.44, error = 1.4, unit = "om:cubicAngstrom" )
        #vec1.addComponent( label = "z", value = "111.22", unit = "om:angstrom" )

        self.assertEqual( vec2.className, "PositionVector" )
        self.assertEqual( vec2.itemName.startswith("pos_mol_C2"), True, msg = vec1.itemName )
        self.assertEqual( vec2.unit, "om:angstrom" )
        self.assertEqual( vec2.vectorLabel, None )

        self.assertEqual( len(vec2.compList), 0 ) # Because assigned to compDict
        self.assertEqual( len(vec2.compDict.keys()), 2 ) # Only 1 component

        self.assertEqual( "x" in vec2.compDict, True )
        self.assertEqual( vec2.compDict["x"]["value"], "111.22" )
        self.assertEqual( vec2.compDict["x"]["error"], "1.2" )
        self.assertEqual( vec2.compDict["x"]["unit" ], "om:degree" )

        self.assertEqual( "y" in vec2.compDict, True )
        self.assertEqual( vec2.compDict["y"]["value"], "111.44" )
        self.assertEqual( vec2.compDict["y"]["error"], "1.4" )
        self.assertEqual( vec2.compDict["y"]["unit" ], "om:cubicAngstrom" )


        #------------------------------------------------
        # TODO add OntoVector with or without tPrefix and aPrefix


        #------------------------------------------------
        # TODO OntoVector with some inputs skipped (value/error/unit)

        #------------------------------------------------


        #------------------------------------------------
        #------------------------------------------------
        #------------------------------------------------
        uuidDB.saveDB()
        #self.assertEqual( 1, 2 )
        pass # TestOntoVector.addComponentDict()


    def test_getCsvArrDict( self ):

        prefix = "http://test/ontology/"
        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        #------------------------------------------------
        vec1 = OntoVector( className = "PositionVector", itemName = "pos_mol_C3", \
                           tPrefix = "http://tbox/", aPrefix = "http://abox/", \
                           uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L" \
                         )

        vec1.addComponent( label = "x", value = "111.22", error = "1.2", unit = "om:degree" )
        #vec1.addComponent( label = "y", value = "111.22", unit = "om:angstrom" )
        # FIXME crashes:
        #vec.addComponent( label = "xx", value = 111.22, error = 1.2, unit = "om:degree" )
        lines = vec1.getCsvArr( "HotPot", prefix + "hasVec1" )

        # For debugging:
        #tools.writeCsv( "TestOntoCrystal3.csv", lines )

        # Defintion of the entity of the class
        il = 0
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        vec = line[0] # The new entity (prefix + name + UUID)
        self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://tbox/PositionVector" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Relation between the new entity and the subject
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "HotPot" )
        self.assertEqual( line[1], "Instance" )
        #self.assertEqual( line[2].startswith( "http://abox/Volume_Test_"), True, msg=line[2] )
        self.assertEqual( line[2], vec )
        self.assertEqual( line[3], prefix + "hasVec1" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Global unit vector:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], vec )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )
        self.assertEqual( line[3], "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Checking Vector label:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasVectorLabel" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], vec )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "L" )
        self.assertEqual( line[5], "xsd:string" )
        

        # Component 1:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        comp1 = line[0]
        self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_x_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.theworldavatar.com/kg/ontocrystal/VectorComponent" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], vec, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "http://www.theworldavatar.com/kg/ontocrystal/hasVectorComponent" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], comp1 )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )
        self.assertEqual( line[3], "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[4], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentLabel" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "x" )
        self.assertEqual( line[5], "xsd:string" )

        # Reminder:
        #vec1.addComponent( label = "x", value = "111.22", error = "1.2", unit = "om:degree" )
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "111.22" )
        self.assertEqual( line[5], "rdfs:Literal" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentUncertainty" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "1.2" )
        self.assertEqual( line[5], "rdfs:Literal" )


        # for value + error + unit assigned:
        # 2 lines : for vector definition
        # 1 line  : for global vector label
        # 1 line  : for global unit
        # 6 lines per component ( 2 + (label,value,error,unit) )
        self.assertEqual( len(lines), 10 )



    """
        #------------------------------------------------
        vec1 = OntoVector( className = "PositionVector", itemName = "pos_mol_C1", \
                           tPrefix = "http://tbox/", aPrefix = "http://abox/", \
                           uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L" \
                         )

        vec1.addComponent( label = "x", value = "111.22", error = "1.2", unit = "om:degree" )
        vec1.addComponent( label = "y", value = "111.22", unit = "om:angstrom" )
 
        #------------------------------------------------
        '''
        vec = OntoVector( className = "PositionVector", itemName = "pos_mol_C1", 
                          tPrefix = "http://tbox/", aPrefix = "http://abox/", 
                          uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L"
                        )

        vec.addComponent( label = "x", value = "111.22", error = "1.2", unit = "om:degree" )
        '''
        #------------------------------------------------
 
        #------------------------------------------------
        vec2 = OntoVector( className = "RadiusVector", itemName = "pos_mol_C2", 
                          tPrefix = "http://tbox/", aPrefix = "http://abox/", 
                          uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L"
                         )

        self.assertEqual( 1, 2 )
        # FIXME the followint function does not work
        vec2.addComponentList( valList = [ 1.1, 2.2, 3.3 ], 
                               errList = [ 0.2, 0.3, 0.4 ], unit = "degree" )
        output += vec2.getCsvArr( "HotPot", prefix + "hasVec2" )

        #------------------------------------------------
        vec3 = OntoVector( className = "RadiusVector", itemName = "pos_mol_C3", 
                          tPrefix = "http://tbox/", aPrefix = "http://abox/", 
                          uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L"
                         )

        vec3.addComponentList( valList = [ 6.1, 7.2, 8.3 ], 
                               errList = [ 0.3, 0.2, 0.1 ], unit = "om:degree" )
        output += vec3.getCsvArr( "HotPot", prefix + "hasVecList" )
 
        tools.writeCsv( "TestOntoCrystal.csv", output )


        uuidDB.saveDB()
        self.assertEqual( 1, 2 )
        pass # TestOntoVector.getCsvArrDict()
    """

    def test_addComponentList( self ):

        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )
        #------------------------------------------------
        vec2 = OntoVector( className = "RadiusVector", itemName = "pos_mol_C4", 
                           tPrefix = "http://tbox/", aPrefix = "http://abox/", 
                           uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L"
                         )

        vec2.addComponentList( valList = [ 1.1, 2.2, 3.3 ], 
                               errList = [ 0.2, 0.3, 0.4 ], 
                               unit = [ "om:degree", "om:cubicAngstrom", "om:dimensionOne"] )

        #output += vec2.getCsvArr( "HotPot", prefix + "hasVec2" )

        self.assertEqual( len(vec2.compList), 3 )
        self.assertEqual( len(vec2.compErrList), 3 )
        self.assertEqual( len(vec2.compUnitList), 3 )
        self.assertEqual( len(vec2.compDict), 0 )

        self.assertEqual( vec2.unit, "om:angstrom" )
        self.assertEqual( vec2.vectorLabel, "L" )
        #self.assertEqual( vec2.unit, "om:angstrom" )

        self.assertEqual( vec2.compList[0], 1.1 )
        self.assertEqual( vec2.compList[1], 2.2 )
        self.assertEqual( vec2.compList[2], 3.3 )

        self.assertEqual( vec2.compErrList[0], 0.2 )
        self.assertEqual( vec2.compErrList[1], 0.3 )
        self.assertEqual( vec2.compErrList[2], 0.4 )

        self.assertEqual( vec2.compUnitList[0], "om:degree" )
        self.assertEqual( vec2.compUnitList[1], "om:cubicAngstrom" )
        self.assertEqual( vec2.compUnitList[2], "om:dimensionOne" )

        #self.assertEqual( 1, 2 )

        #------------------------------------------------
        #------------------------------------------------

        uuidDB.saveDB()
        #self.assertEqual( 1, 2 )
        pass # TestOntoVector.addComponentList()

    def test_getCsvArrList( self ):

        prefix = "http://test/ontology/"
        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        #------------------------------------------------
        vec1 = OntoVector( className = "RadiusVector", itemName = "pos_mol_C5", 
                           tPrefix = "http://tbox/", aPrefix = "http://abox/", 
                           uuidDB = uuidDB, unit = "om:angstrom", vectorLabel = "L"
                         )

        vec1.addComponentList( valList = [ 6.1, 7.2, 8.3 ], 
                               errList = [ 0.3, 0.2, 0.1 ], unit = "om:degree" )

        lines = vec1.getCsvArr( "HotPot", prefix + "hasVecList" )

        # For debugging:
        #tools.writeCsv( "TestOntoCrystal222.csv", lines )

        self.assertEqual( len(vec1.compList), 3 ) # Because assigned to compDict
        self.assertEqual( len(vec1.compDict.keys()), 0 ) # Only 1 component

        # Defintion of the entity of the class
        il = 0
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        vec = line[0] # The new entity of class Vector (prefix + name + UUID)
        self.assertEqual( line[0].startswith( "http://abox/pos_mol_C5_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://tbox/RadiusVector" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Relation between the new entity and the subject
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "HotPot" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2].startswith( "http://abox/pos_mol_C5_"), True, msg=line[2] )
        self.assertEqual( line[2], vec )
        self.assertEqual( line[3], prefix + "hasVecList" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Global unit vector:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], vec )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )
        self.assertEqual( line[3], "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Checking Vector label:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasVectorLabel" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], vec )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "L" )
        self.assertEqual( line[5], "xsd:string" )


        # Component 1:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        comp1 = line[0]
        self.assertEqual( line[0].startswith( "http://abox/pos_mol_C5_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.theworldavatar.com/kg/ontocrystal/VectorComponent" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], vec, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "http://www.theworldavatar.com/kg/ontocrystal/hasVectorComponent" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], comp1 )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )
        self.assertEqual( line[3], "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[4], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentIndex" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 1 )
        self.assertEqual( line[5], "xsd:integer" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 6.1 )
        self.assertEqual( line[5], "rdfs:Literal" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentUncertainty" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp1 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 0.3 )
        self.assertEqual( line[5], "rdfs:Literal" )

        # Component 2:
        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        comp2 = line[0]
        self.assertEqual( line[0].startswith( "http://abox/pos_mol_C5_comp_2_"), True, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.theworldavatar.com/kg/ontocrystal/VectorComponent" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], vec, msg = line[0] )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], comp2 )
        self.assertEqual( line[3], "http://www.theworldavatar.com/kg/ontocrystal/hasVectorComponent" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        self.assertEqual( line[0], comp2 )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )
        self.assertEqual( line[3], "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentIndex" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp2 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 2 )
        self.assertEqual( line[5], "xsd:integer" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentValue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp2 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 7.2 )
        self.assertEqual( line[5], "rdfs:Literal" )

        il += 1
        line = lines[il]
        for i in range(6):
            self.assertEqual( str(line[i]).strip(), str(line[i]) )
        #self.assertEqual( line[0].startswith( "http://abox/pos_mol_C3_comp_1_"), True, msg = line[0] )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasComponentUncertainty" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], comp2 )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 0.2 )
        self.assertEqual( line[5], "rdfs:Literal" )

        # Component 3 (skipping 6 lines)
        il += 6


        # Reminder:
        #vec1.addComponentList( valList = [ 6.1, 7.2, 8.3 ], 
        #                       errList = [ 0.3, 0.2, 0.1 ], unit = "om:degree" )

        # Total number of lines for value + error + unit assigned:
        #   2 lines : for vector definition
        #   1 line  : for global unit
        #   1 line  : for vector label
        #   3 * 6   : 3 components with 6 lines each (2+index,value,error,unit) 
        self.assertEqual( len(lines), 22 )

        #------------------------------------------------

        uuidDB.saveDB()
        #------------------------------------------------

        pass # TestOntoVector.getCsvArrList()

    pass # class TestOntoVector

"""
class TestOntoMatrix( unittest.TestCase ):

    def test_addComponent( self ):

        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        uuidDB.saveDB()
        self.assertEqual( 1, 2 )
        pass # TestOntoMatrix.addComponent()

    def test_addComponentList( self ):

        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        uuidDB.saveDB()
        self.assertEqual( 1, 2 )
        pass # TestOntoMatrix.addComponentList()

    def test_getCsvArr( self ):

        prefix = "http://test/ontology/"
        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        uuidDB.saveDB()
        self.assertEqual( 1, 2 )
        pass # TestOntoMatrix.getCsvArr()
    pass # class TestOntoMatrix

"""

class TestOntoCrystal( unittest.TestCase ):

    def test_a( self ):
        self.assertEqual( 1, 1 )
        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )
        output = []
        prefix = "http://test/ontology/"

        #print( "Missing arrInitCsv() function " )

        #line = getCsvInit( "OntoTest",
        #                   "http://temp/t-box.owl",
        #                   "http://temp/a-box" )
        #output += line
        #self.assertEqual( line[0], None )




        uuidDB.saveDB()
        pass # TestOntoCrystal.

    def test_getCsvArr( self ):
        uuidDB = tools.UuidDB( filename = "test_datatypes.csv" )

        uuidDB.saveDB()
        pass # TestOntoCrystal.getCsvArr()


    pass # class TestOntoCrystal



if __name__ == "__main__":

    unittest.main()

    """
    for i,x in enumerate([1,2,3,4]):
        pass
    print( i )
    1/0
    """

    #================================================


