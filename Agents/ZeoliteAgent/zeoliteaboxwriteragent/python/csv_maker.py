"""
Creator of abox .csv file for zeolite data from CIF file and other sources.

This ontology uses the OntoCrystal and OntoZeolite TBox structures.
Extra features supported:
- Save the uncertainties for some of the crystal parameters,
- Divide a concatenated CIF file and save such files separately.

There are different lists of Atom Sites:
1) original atoms as they are written in CIF (fractional coordinates, 
   removed symmetrical copies, with uncertainties)
   This is done by TODO
2) all atoms in the unit cell (original with symmetry applied, with uncertainties).
   This is done by TODO
3) all atoms in the unit cell (original with symmetry applied, no uncertainties).
   This is done by defailt in PyMatGen package.
All of these lists can be converted to the cartesian coordinates 
with or without uncertainties.

It is also possible to save only the CIF data.

Usage:
python csv_maker.py 
- No flag  : use all available data to create a .csv file for an individual zeolite.
--cif,-c path : process only the .cif file to populate the CristalInformation 
             class instance with its subclasses and save the result in a .csv file.
             Such .csv file can be concatenated with other files.
             'path' may be a file or a directory. In the latter case all .cif files 
             in that directory and its sub-directories will be processed.
             Each .cif file is converted to a separate .csv file.
             The output is written to the ???? TODO
--out,-o   : The name of the output file(s). Each   
-??? 

"""


import csv
import os

import logging
#logging.basicConfig( level = logging.DEBUG )
#logging.basicConfig( level = logging.INFO )
#logging.basicConfig( level = logging.WARNING )
logging.basicConfig( level = logging.ERROR )

import tools
import zeolist
import tilesignature
import math
import numpy
import argparse

from pymatgen.core.structure import Structure, Lattice
import pymatgen


# FIXME
#http://www.w3.org/2000/01/rdf-schema#Literal
class CommandLine:
  #__slots__ = []

  def __init__(self):
    parser = argparse.ArgumentParser( description='what is this?' )
    parser.add_argument( '--cif', type=str, default='', help='file name of the CIF file' )

    args = parser.parse_args()
    #print( args.accumulate, args.cif )
    print( "Input CIF file ='" + args.cif + "'." )
    pass # __init__()

  def a(self):


    pass

  pass # class CommandLine

class OntoValueWithUncertainty:
  __slots__ = ["uuidDB"]
  def __init__( self, prefix = "", uuidDB = None,
                myName = "" ):
                  
    if "" == myName:
      self.name = "Unknown"
      logging.error( " Empty name in '" + self.name + "' class OntoValueWithUncertainty." ) 
    else:   
      self.name = myName

    if None == uuidDB:
      logging.error( " Empty uuidDB in '" + self.name + "' class OntoValueWithUncertainty." ) 
    else:   
      #self.name = myName
      pass

     
    pass # __init__()


  def setValue( self, value = "", unit = "", error = "" ):
    if "" == value:
      logging.error( " Empty value in '" + self.name + "' " ) 

    if "" == unit:
      logging.error( " Empty unit in '" + self.name + "' " ) 

    if "" == error:
      logging.error( " Empty error in '" + self.name + "' " ) 


    pass # setValue()

  def arrValue( self, subject, predicate ):
    """
    subject   - Is the full hame of instance of class, 
                which contains this entity of ValueWithUncertainty class.
    predicate - Is the Object Property linking the Subject and 
                the current entity of ValueWithUncertainty." 

    # These property values are not guaranteed:
    if subject.find( "CrystalInformation" ) < 0:
      logging.warning( " Subject in arrUnitCell() is '" + subject + "'," +
                       " expecting the name to contain '" + "CrystalInfromation" + "'." ) 

    """
    if not predicate.startswith("has"):
      logging.warning( " Predicate in arrValue() is '" + predicate + "'," +
                       " but expecting it to have '" + "has" + "'." ) 


    output = []
    logging.error( " arrValue() is not implemented yet" )

    return output
    pass

  pass # OntoValueWithUncertainty

class OntoVector:
  __slots__ = [ "uuidDB", "value", "ontoPrefix", "uuid_error" ]

  def __init__( self, prefix = "", uuidDB = None, myClass = "", 
                      myName = "", myUnit = "",   myLabel = "" ):
    self.value = dict()

    if "" == myName:
      logging.error( " In OntoVector entity name is not specified.")
      self.value["name"] = "Unknown"
    else:
      self.value["name"] = myName

    if None == uuidDB:
      logging.error( " In OntoVector uuidDB is not specified" + 
                     " for '" + self.value["name"] + "'."  )
    else:
      self.uuidDB = uuidDB

    if "" == myClass:
      logging.error( " In OntoVector class name is not specified" +
                     " for '" + self.value["name"] + "'." )
      self.value["class"] = "Unknown"
    else:
      self.value["class"] = myClass


    if "" == myUnit:
      logging.error( " In OntoVector unit is not specified" +
                     " for '" + self.value["name"] + "'." )
      self.value["unit"] = "Unknown"
    else:
      self.value["unit"] = myUnit


    if "UnitCellLatticeVector" == self.value["class"]:
      if "" == myLabel:
        logging.error( " In OntoVector label is not specified" +
                       " for '" + self.value["name"] + "'." )
        self.value["label"] = "Unknown"
      else:
        self.value["label"] = myLabel
    else:
      pass

    # The vector class is defined in the OntoCrystal ontology:
    self.ontoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
    #if "" == prefix:
    #  logging.error( " In OntoVector ontology prefix is not specified" +
    #                 " for '" + self.value["name"] + "'." )
    #else:
    #  self.ontoPrefix = prefix
    
    self.value["comp" ] = dict()

    self.uuid_error = None      # This is used to avoid double creation of the Uncertainty Vector
    #self.error = dict()

    pass # __init__()
  def addComponentList( self, list, unit = "", error = "" ):
    logging.error( "in CsvMaker addComponentList() is not implemented" )

    pass # addComponentList()

  def addComponent( self, label, value = "", unit = "", error = "" ):
    logging.warning( "Starting addComponent '" + self.value["name"] + "' label: '" + label + "'" )
    """
    Adding a component to a vector.
    TODO: how to create a vector made of multiple values as a list?
          probably it should be a separate function addComponentList( self, list, unit = "", error = "" )

    label - required input argument. It defines the component subscript.
    value - required input argument. The value of this component.
    unit  - Optional. An instance defined in ontology OM-2. 
            Accepted values either full path, or only the instance name.
            The OM-2 prefix will be appended in getArrVector().
    error - Optional. The component's error-bar (uncertainty). It has the same unit.
    """
    errCount = 0
    if isinstance( value, str ):
      if "" == value:
        logging.error( " Not specified value for vector component in '" + self.value["name"] + "'." )
        errCount += 1
    else:
      logging.error( "value = '" + str( value ) + "' is not a string in '" + self.value["name"] + "'." )

    if isinstance( label, str ):
      if "" == label:
        logging.error( " Not specified label for vector component in '" + self.value["name"] + "'." )
        errCount += 1

      if label in list(self.value["comp"].keys()):
        logging.warning( " Repeatedly adding (overwriting) a component '" + 
                         label + "' in vector '" + self.value["name"] + "'." )
        logging.warning( "Components-2 of vector =" + str(self.value["comp"]) + " value = '" + value + "'."  )
        #errCount += 1 # No need to count as error. It may be intensional.

    else:
      logging.error( "label = '" + str( label ) + "' is not a string in '" + self.value["name"] + "'." )

    if 0 == errCount:
        self.value["comp"][label] = dict()
        self.value["comp"][label]["value"] = value
        if "" == unit:
          logging.warning( " Not specified label for vector component in '" + self.value["name"] + "'." )
        else:
          self.value["comp"][label]["unit"]  = unit

        if "" != error:
          self.value["comp"][label]["error"] = error
          logging.warning( "Components of vector = ", self.value["comp"], " value = '" + value + "'."  )
          if "" != unit:
            # Generally speaking the error bars may have different units, 
            # though such situation looks strange:
            print( " Assigned unit to", label )
            self.value["comp"][label]["errorunit"]  = unit
            pass
            
    pass

  def getArrVector( self, subject, predicate ):
    """
    subject   - Is the full hame of instance of class, 
                which contains this Vector class.
    predicate - Is the Object Property linking the Subject and the current UnitCell.
                Typically is should be contain "has".

    # Value is not guaranteed:
    if subject.find( "CrystalInformation" ) < 0:
      logging.warning( " Subject in arrUnitCell() is '" + subject + "'," +
                       " expecting the name to contain '" + "CrystalInfromation" + "'." ) 
    """

    #if "has" != predicate:
    if not predicate.startswith("has"):
      logging.warning( " Predicate in getArrVector() is '" + predicate + "'," +
                       " but expecting it to have '" + "has" + "'." ) 


    """
    Create a vector with specified values.
    'subject' - is the full proper name of the parent class pointing to the new vector,
    'preidcate' - is the full object property to link the parent and the new vector,
    The input 'value' is a dictionary with values:
    ["class"] : The class of the newly created vector. Depending on the class name 
                some properties may not be available.
    ["name"] : The instance name (without UUID, UUID will be generated automatically)
               Internally, all components and other classes (if any) 
               will be using the same UUID.
               The 'name' should be saved somewhere internally to prevent 
               creation of vectors with repeating names.
    ["comp"]["x"]     : value
    ["comp"]["alpha"] : value
    ["comp"][0]       : value
    ["unit"]          : (optional) unit for the entire vector
    ["comp"]["x"]["value"]  : value of the individual component
    ["comp"]["x"]["unit"]  : (optional) for individual component
    ["comp"]["list"]  : the value is a list(), defines the vector using integer index
    ["comp"]["start"] : if ["comp"] is a "list" this is the index of the first component. Default is 1.
    ["+/-"]           : appends another vector 'uncertainty' to this file.
    ["label"]         : A label assigned to the entire vector (valid only for UnitCellLatticeVector class)

    Other keys are also possible. There will be warnings for unknown/unsupported keys.
    """
    output = []
    #logging.error( " getArrVector() is not implemented yet" )
    # FIXME TODO

    keys = self.value.keys()
    if "name" not in keys:
      logging.error( " Creating a vector without the vector['name'] specified. I skip it." )
      return []

    if "class" in keys:
      myClass = self.value["class"]
    else:
      logging.error( " Missing vector['class'] for vector '" + self.value["name"] + 
                     "', will use default 'Vector'."  )
      myClass = "Vector" 


    if "comp" not in keys:
      logging.error( " Missing vector['comp'] for vector '" + self.value["name"] + "'"  )

    # Vector for Unit Cell length parameters (a,b,c):
    uuid_vector = tools.getUUID( self.uuidDB, myClass, self.value["name"] )
    output.append( [ uuid_vector, "Instance", myClass, "", "", "" ] )
    output.append( [ subject, "Instance", uuid_vector, predicate, "", "" ] )

    # Verification of the class and its components:
    if myClass in [ "Vector" ]:
      # Classes derived from the basic 'Vector':
      if "error" in keys:
        logging.error( " Vector class '" + myClass + "' has an error value: " + 
                       self.value["error"] + ". Use class 'VectorWithUncertainty' " + 
                       "instead or a class derived from it." )
       
      pass
    elif myClass in [ "UnitCellAngles", "UnitCellLengths", "UnitCellAngles" ]:
      # Classes derived from 'VectorWithUncertainty':

      self.value["error"] = dict() # the error bars a.k.a. uncertainty. Optional parameter.
      pass
    elif myClass in [ "UnitCellLatticeVector" ]:
      # Classes derived from 'UnitCellLatticeVector':

      self.value["error"] = dict() # the error bars a.k.a. uncertainty. Optional parameter.
                    
      if "label" in keys:
        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_vector, "", self.value["label"], "string" ] )

      else:
        logging.warning( " Label is not defined for '" + self.value["name"] + 
                         "' of class '" + myClass + "'." )

      pass
      """
    elif "Vector" == myClass: 
      pass
    elif "UnitCellLengths" == myClass: 
      pass
    elif "UnitCellAngles"  == myClass: 
      # Do nothing
      pass
    elif "VectorWithUncertainty" == myClass:
      # Do nothing
      pass
    elif "UnitCellLatticeVector" == myClass:
      # Do nothing
      pass
      """

    else:
      logging.error( " Unknown vector class '" + myClass + "' "
                      "for vector '" + self.value["name"] + "'." )
      pass

    if   isinstance( self.value["comp"], dict):
      comp_keys = self.value["comp"].keys()  # comp_keys means 'KEYS of the COMPonents'.
      for ck in comp_keys:
        uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", self.value["name"] + "_comp_" + ck  )
        output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

        output.append( [ uuid_vector, "Instance", uuid_comp,  
                         self.ontoPrefix + "hasVectorComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_comp, "", ck, "string" ] )

        output.append( [ self.ontoPrefix + "hasComponentValue", "Data Property", 
                         uuid_comp, "", self.value["comp"][ck]["value"], "rdfs:Literal" ] )
 
        if "unit" in list(self.value["comp"][ck].keys()): # and "Unknown" != self.value["comp"][ck]["unit"]:
          unit = self.value["comp"][ck]["unit"]
          if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
            logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
                             self.value["name"] + "'. " + 
                             "Expecting an instance of OM-2 ontology class Unit, but got '" +
                             self.value["comp"][ck]["unit"] )

          output.append( [ uuid_comp, "Instance", unit,
                         "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                          "", "" ] )
 
        if "error" in list(self.value["comp"][ck].keys()):
          if None == self.uuid_error:
            logging.info( " Creating a new instance for uuid_error for vector '" + self.value["name"] + "' " )

            self.uuid_error = tools.getUUID( self.uuidDB, self.ontoPrefix + "Vector", self.value["name"] + "_error" )
            output.append( [ self.uuid_error, "Instance", self.ontoPrefix + "Vector", "", "", "" ] )
 
            output.append( [ uuid_vector, "Instance", self.uuid_error,  
                             self.ontoPrefix + "hasUncertaintyVector", "", "" ] )
 
          uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", self.value["name"] + "_err_" + ck  )
          output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

          output.append( [ self.uuid_error, "Instance", uuid_comp,  
                           self.ontoPrefix + "hasVectorComponent", "", "" ] )

          output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                           uuid_comp, "", ck, "string" ] )

          output.append( [ self.ontoPrefix + "hasComponentValue", "Data Property", 
                           uuid_comp, "", self.value["comp"][ck]["error"], "rdfs:Literal" ] )
 
          #print( "eeewwwwwwwwweeeeeeeeeeee" )
          # FIXME here there is a mess between the units.
          if "errorunit" in list(self.value["comp"][ck].keys()):
            unit = self.value["comp"][ck]["errorunit"]
          elif "unit" in list(self.value["comp"][ck].keys()):
            unit = self.value["comp"][ck]["unit"]
          else:
            unit = ""

          if "" != unit: 
            if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
              logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
                             self.value["name"] + "'. " + 
                             "Expecting an instance of OM-2 ontology class Unit." )

            output.append( [ uuid_comp, "Instance", unit,
                           "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                            "", "" ] )


    elif isinstance( self.value["comp"], list):
      logging.error( " Not implemented yet list component 2222222 " )

    else:
      logging.error( " Unknown type of vector['comp'] = '" + str(type(self.value["comp"])) + 
                     "' in vector '" + self.value["name"] + "' reter." )

    if "unit" in keys:
#"http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom"
      unit = self.value["unit"]
      if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
        logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
                         self.value["name"] + "'. " + 
                         "Expecting an instance of OM-2 ontology class Unit, but got '" +
                         self.value["unit"] + "'" 
                         )

      output.append( [ uuid_vector, "Instance", unit,
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    #print( "==============================" )
    return output
    pass # getArrVector()


  pass # class OntoVector

class CrystalInformation:
  """
  A single CsvMaker can have several different data-sets from a CIF file:
  1) As they are loaded by PyMatGen,
  2) As they appear in the CIF file (symmetry + reduced sites)
  3) Data with uncertainty.

  I could store these sets of data in different CrystalInformation classes
  and use them as needed. But probably I will not do it.
  I can store the data to the same variables inside readWithUncertainties()
  Whatever function was called last - it will be written to the output.

  """

  def __init__( self ):
    pass







  pass # class CrystalInformation

class CsvMaker:
  """
  An adjustable constructor of a .csv file for the ABox of a crystal or zeolite.
  Algorithm for operation of this class:

  0) Prepare the header lines (standard).
  1) For CIF related data:
    a) Load CIF data and store internally.
       by loadCifZeolite()   - zeolite-specific, calls the next funtion:
                               Some CIF files may be a merge of several files ??? TODO

          loadCifStructure() - this function does not know about zeolites, 
                               it can be used for any kind of CIF file.
                               It saves data from CIF to internal variables.
          
    b) Modify CIF data (if necessary).
       by ???
    c) Save data related to CIF (it can be in different structure, 
       according to the settings).
       arrUnitCell()
       arrAtomSite()
       arrTransform()
       etc
  2) For Tile related data:
    a) 

  ) Finally dump all data to a .csv file.

  """
  __slots__ = [ "zeoList",  "zeoOption", "zeoOntoPrefix", "crystOntoPrefix",  "ontoBase", 
                "inputDir", "outputDir", "cifStandard", "uuidDB",    
                "entriesWithUncertainties", "loopHeaders", #"", "", 
                "unitCellLengths", "unitCellRecipLengths", 
                "unitCellAngles",  "unitCellRecipAngles",
                "cifPyMatGen", 
                "unitCellVectorA",      "unitCellVectorB",      "unitCellVectorC",
                "unitCellRecipVectorA", "unitCellRecipVectorB", "unitCellRecipVectorC",


                #"", "", "", 
                ]

  def __init__( self ):
    self.zeoOption = "main"
    #self.zeoOption = "test"
    self.zeoList = []

    self.ontoBase   = "OntoZeolite"
    self.zeoOntoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"
    self.crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

    self.inputDir  = os.path.join( "ontozeolite", "data" )
    self.outputDir = os.path.join( "ontozeolite", "zeocsv" )

    self.cifStandard = []
    self.cifStandard = self.readStandardFile( "CIF_standard_2.4.5.txt" )

    self.unitCellLengths = None
    self.unitCellAngles = None
    self.unitCellRecipLengths = None
    self.unitCellRecipAngles = None

    self.unitCellVectorA = None
    self.unitCellVectorB = None
    self.unitCellVectorC = None

    self.unitCellRecipVectorA = None
    self.unitCellRecipVectorB = None
    self.unitCellRecipVectorC = None


    #baseName = self.ontoBase + "-" + zeoname
    #self.ontoHttp = ""

    pass # CsvMaker.__init__()

  def cleanString( self, line ):
    if not isinstance( line, str ):
      logging.error( "Input line '" + str(line) + "' i snot a string in cleanString()" )
      return str(line)
    pos = line.find( "#" )
    if pos < 0:
      tmp = line.strip()
    else:
      tmp = line[:pos].strip()

    return tmp

  def loadCifZeolite( self, zeoname ):
    """
    - zeolite-specific, calls the next funtion:
    """

    filePath = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

    #print( "Using a test case 913885.cif for the UnitCell class" )
    #path = os.path.join( "test", "913885.cif" ) 
    #dataIn = tools.readCsv( path )
    if not os.path.isfile( filePath ):
      logging.error( "File not found '" + filePath + "'." )
      return 

    self.loadCifStructure( filePath, zeoname )

    nBracket = self.readWithUncertainties( filePath, zeoname, save = True )
    print( "Found brackets:",  nBracket, "in '" + zeoname + "'." )
 
    return
    pass # loadCifZeolite()


  def loadCifStructure( self, filePath, cifName ):
    """
    This function does not know about zeolites, it can be used for any CIF file.
    Loads CIF file and stores the data in internal variable(s) 
    self.structure ?

    ??? cifName is a unique name for the CIF structure, as it should appear in the ontology abox.
    """

    self.cifPyMatGen = Structure.from_file( filePath )

    # The Unit Cell Lengths:
    self.unitCellLengths = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLengths", 
                                       myName  = "UnitCellLengths_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )

    self.unitCellLengths.addComponent( label = "a", 
         value = str(round(self.cifPyMatGen.lattice.a, 12)), error = "" )

    self.unitCellLengths.addComponent( label = "b", 
         value = str(round(self.cifPyMatGen.lattice.b, 12)), error = "" )

    self.unitCellLengths.addComponent( label = "c", 
         value = str(round(self.cifPyMatGen.lattice.c, 12)), error = "" )

    # The Unit Cell Angles:
    self.unitCellAngles = OntoVector( uuidDB = self.uuidDB, 
                                      myClass = "UnitCellAngles", 
                                      myName  = "UnitCellAngles_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )

    self.unitCellAngles.addComponent( label = "alpha", 
         value = str(round(self.cifPyMatGen.lattice.alpha, 12)), error = "" )

    self.unitCellAngles.addComponent( label = "beta", 
         value = str(round(self.cifPyMatGen.lattice.beta , 12)), error = "" )

    self.unitCellAngles.addComponent( label = "gamma", 
         value = str(round(self.cifPyMatGen.lattice.gamma, 12)), error = "" )

    # The Reciprocal Unit Cell Lengths:
    self.unitCellRecipLengths = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLengths", 
                                       myName  = "UnitCellReciprocalLengths_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom" )

    self.unitCellRecipLengths.addComponent( label = "a*", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.a, 12)), error = "" )

    self.unitCellRecipLengths.addComponent( label = "b*", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.b, 12)), error = "" )

    self.unitCellRecipLengths.addComponent( label = "c*", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.c, 12)), error = "" )

    # The Reciprocal Unit Cell Angles:
    self.unitCellRecipAngles = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellAngles", 
                                       myName  = "UnitCellReciprocalAngles_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )

    self.unitCellRecipAngles.addComponent( label = "alpha*", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.alpha, 12)), error = "" )

    self.unitCellRecipAngles.addComponent( label = "beta*", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.beta , 12)), error = "" )

    self.unitCellRecipAngles.addComponent( label = "gamma*", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.gamma, 12)), error = "" )

    # Vectors to keep three Unit Cell vectors (a,b,c):
    self.unitCellVectorA = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLatticeVector", 
                                       myName  = "UnitCellVectorA_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                                       myLabel = "a" )

    self.unitCellVectorA.addComponent( label = "x", 
         value = str(round(self.cifPyMatGen.lattice.matrix[0][0], 12)), error = "" )

    self.unitCellVectorA.addComponent( label = "y", 
         value = str(round(self.cifPyMatGen.lattice.matrix[0][1], 12)), error = "" )

    self.unitCellVectorA.addComponent( label = "z", 
         value = str(round(self.cifPyMatGen.lattice.matrix[0][2], 12)), error = "" )

    self.unitCellVectorB = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLatticeVector", 
                                       myName  = "UnitCellVectorB_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                                       myLabel = "b" )

    self.unitCellVectorB.addComponent( label = "x", 
         value = str(round(self.cifPyMatGen.lattice.matrix[1][0], 12)), error = "" )

    self.unitCellVectorB.addComponent( label = "y", 
         value = str(round(self.cifPyMatGen.lattice.matrix[1][1], 12)), error = "" )

    self.unitCellVectorB.addComponent( label = "z", 
         value = str(round(self.cifPyMatGen.lattice.matrix[1][2], 12)), error = "" )


    self.unitCellVectorC = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLatticeVector", 
                                       myName  = "UnitCellVectorC_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                                       myLabel = "c" )

    self.unitCellVectorC.addComponent( label = "x", 
         value = str(round(self.cifPyMatGen.lattice.matrix[2][0], 12)), error = "" )

    self.unitCellVectorC.addComponent( label = "y", 
         value = str(round(self.cifPyMatGen.lattice.matrix[2][1], 12)), error = "" )

    self.unitCellVectorC.addComponent( label = "z", 
         value = str(round(self.cifPyMatGen.lattice.matrix[2][2], 12)), error = "" )


    # Vectors to keep three Reciprocal Unit Cell vectors (a,b,c):
    self.unitCellRecipVectorA = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLatticeVector", 
                                       myName  = "UnitCellReciprocalLatticeVectorA_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                                       myLabel = "a" )

    self.unitCellRecipVectorA.addComponent( label = "x", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[0][0], 12)), error = "" )

    self.unitCellRecipVectorA.addComponent( label = "y", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[0][1], 12)), error = "" )

    self.unitCellRecipVectorA.addComponent( label = "z", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[0][2], 12)), error = "" )

    self.unitCellRecipVectorB = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLatticeVector", 
                                       myName  = "UnitCellReciprocalLatticeVectorB_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                                       myLabel = "a" )

    self.unitCellRecipVectorB.addComponent( label = "x", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[1][0], 12)), error = "" )

    self.unitCellRecipVectorB.addComponent( label = "y", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[1][1], 12)), error = "" )

    self.unitCellRecipVectorB.addComponent( label = "z", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[1][2], 12)), error = "" )



    self.unitCellRecipVectorC = OntoVector( uuidDB = self.uuidDB, 
                                       myClass = "UnitCellLatticeVector", 
                                       myName  = "UnitCellReciprocalLatticeVectorC_" + cifName,
         myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                                       myLabel = "a" )

    self.unitCellRecipVectorC.addComponent( label = "x", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[2][0], 12)), error = "" )

    self.unitCellRecipVectorC.addComponent( label = "y", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[2][1], 12)), error = "" )

    self.unitCellRecipVectorC.addComponent( label = "z", 
         value = str(round(self.cifPyMatGen.lattice.reciprocal_lattice.matrix[2][2], 12)), error = "" )




    pass # loadCifStructure()

  def readStandardFile( self, path ):
    output = []
    if not os.path.isfile( path ):
      logging.error( "CIF standard file does not exist: '" + path + "'" )
      return []
    f = open( path, encoding="utf8" )
    for line in f:
      short = self.cleanString( line )
      pos = short.find( "_" )
      if 0 == pos:
        pos = short.find( "_[]" )
        if pos < 0:
          output.append(short)
        else:
          pass

    f.close()
    return output

  def prepare( self ):
    self.zeoList = zeolist.getZeoList( self.zeoOption )
    #self.zeoList = [ os.path.join( "test", "913885.cif" ) ]
    self.uuidDB  = tools.loadUUID( )

    # May be also command line arguments here:

  def __del__( self ):
    #self.finalize()
    pass

  def finalize( self ):
    tools.saveUUID( self.uuidDB )

  '''
  def getArrVector( self, subject, predicate, vector ):
    """
    Create a vector with specified values.
    'subject' - is the full proper name of the parent class pointing to the new vector,
    'preidcate' - is the full object property to link the parent and the new vector,
    The input 'value' is a dictionary with values:
    ["class"] : The class of the newly created vector. Depending on the class name 
                some properties may not be available.
    ["name"] : The instance name (without UUID, UUID will be generated automatically)
               Internally, all components and other classes (if any) 
               will be using the same UUID.
               The 'name' should be saved somewhere internally to prevent 
               creation of vectors with repeating names.
    ["comp"]["x"]     : value
    ["comp"]["alpha"] : value
    ["comp"][0]       : value
    ["unit"]          : (optional) unit for the entire vector
    ["comp"]["x"]["value"]  : value of the individual component
    ["comp"]["x"]["unit"]  : (optional) for individual component
    ["comp"]["list"]  : the value is a list(), defines the vector using integer index
    ["comp"]["start"] : if ["comp"] is a "list" this is the index of the first component. Default is 1.
    ["+/-"]           : appends another vector 'uncertainty' to this file.
    ["label"]         : A label assigned to the entire vector (valid only for UnitCellLatticeVector class)

    Other keys are also possible. There will be warnings for unknown/unsupported keys.
    """
    output = []
    #logging.error( " getArrVector() is not implemented yet" )
    # FIXME TODO

    keys = vector.keys()
    if "name" not in keys:
      logging.error( " Creating a vector without the vector['name'] specified. I skip it." )
      return []

    if "class" in keys:
      myClass = vector["class"]
    else:
      logging.error( " Missing vector['class'] for vector '" + vector["name"] + 
                     "', will use default 'Vector'."  )
      myClass = "Vector" 

    if "comp" not in keys:
      logging.error( " Missing vector['comp'] for vector '" + vector["name"] + "'"  )

    if myClass in ["Vector", "UnitCellLengths", "UnitCellAngles"]:
        # Do nothing
        pass
    elif "VectorWithUncertainty" == myClass:
        # Do nothing
        pass
    elif "UnitCellLatticeVector" == myClass:
        # Do nothing
        pass
    else:
        logging.error( " Unknown vector class '" + myClass + "' "
                       "for vector '" + vector["name"] + "'." )

    # Vector for Unit Cell length parameters (a,b,c):
    uuid_vector = tools.getUUID( self.uuidDB, myClass, vector["name"] )
    output.append( [ uuid_vector, "Instance", myClass, "", "", "" ] )
 
    if "UnitCellLatticeVector" == myClass:
      if "label" in keys:
        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_vector, "", vector["label"], "string" ] )

      else:
        logging.warning( " Label is not defined for '" + vector["name"] + 
                         "' of class '" + myClass + "'." )


    if   isinstance( vector["comp"], dict):
      comp_keys = vector["comp"].keys()  # comp_keys means 'KEYS of the COMPonents'.
      for ck in comp_keys:
        uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", vector["name"] + "_comp_" + ck  )
        output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

        output.append( [ uuid_vector, "Instance", uuid_comp,  
                         self.ontoPrefix + "hasComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_comp, "", ck, "string" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                         uuid_comp, "", vector["comp"][ck]["value"], "rdfs:Literal" ] )
 
        if "unit" in list(vector["comp"][ck].keys()):
          unit = vector["comp"][ck]["unit"]
          if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
            logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
                             vector["name"] + "'. " + 
                             "Expecting an instance of OM-2 ontology class Unit." )

          output.append( [ uuid_comp, "Instance", unit,
                         "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                          "", "" ] )


    if "unit" in keys:
#"http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom"
      unit = vector["unit"]
      if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
        logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
                         vector["name"] + "'. " + 
                         "Expecting an instance of OM-2 ontology class Unit." )

      output.append( [ uuid_vector, "Instance", unit,
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    elif isinstance( vector["comp"], list):
      logging.error( " Not implemented yet list component 2222222 " )

    else:
      logging.error( " Unknown type of vector['comp'] = '" + str(type(vector["comp"])) + 
                     "' in vector '" + vector["name"] + "'." )

    #print( "==============================" )
    return output
    pass # getArrVector()
  '''   

  def arrInit( self, zeoname ):
    output = [ ]
    baseName = self.ontoBase # + "-" + zeoname

    output.append( [ "Source", "Type", "Target", "Relation", "Value", "Data Type" ] )

    output.append( [ baseName, "Ontology", 
                     "http://www.theworldavatar.com/kg/ontozeolite/OntoZeolite.owl", 
                     "http://www.w3.org/2002/07/owl#imports", "", "" ] )

    output.append( [ baseName, "Ontology", 
                     "http://www.theworldavatar.com/kg/ontozeolite",
                     "base", "", "" ] )

    # The framework should be initialized only once.
    #uuidDB = tools.loadUUID( )
    #uuidDB = self.uuidDB

    uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    output.append( [ uuid_zeolite, "Instance", "ZeoliteFramework", "", "", "" ] )
 
    output.append( [ self.zeoOntoPrefix + "hasZeoliteCode", "Data Property", 
                     uuid_zeolite, "", zeoname.strip(' "'), "string" ] )


    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrInit()

  def arrTransform( self, subject, predicate, zeoname ):
    """
    subject   - Is the full hame of instance of class CrystalInformation, 
                which contains this CoordinateTransformation class.
    predicate - Is the Object Property linking the Subject and the current CoordinateTransformation.
                Typically is should be equal to "hasCoordinateTransformation".
    """

    if subject.find( "CrystalInformation" ) < 0:
      logging.warning( " Subject in arrCoordinateTransformation() is '" + subject + "'," +
                       " expecting the name to contain '" + "CrystalInfromation" + "'." ) 

    if "hasCoordinateTransformation" != predicate:
      logging.warning( " Predicate in arrTransform() is '" + predicate + "'," +
                       " but expecting '" + "hasCoordinateTransformation" + "'." ) 


    #print( "arrTransform started1 )
    TWOPI = 2 * math.pi
    DIRS = "xyz"

    output = []

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

    #dataIn = tools.readCsv( path )
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return

    structure = Structure.from_file( path )
    #uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    uuid_cif = tools.getUUID( self.uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )

    uuid_cif_core_trans = tools.getUUID( self.uuidDB, "CIFCoreTransform", "ZeoliteCIFCoreTransform_" + zeoname )
    output.append( [ uuid_cif_core_trans, "Instance", "CIFCoreTransform", "", "", "" ] )
    output.append( [ uuid_cif, "instance", uuid_cif_core_trans,  
                     self.crystOntoPrefix + "hasCifCoreTransformation", "", "" ] )

    ################# Fractional to Cartesian ########################
    uuid_m_frac_to_cart = tools.getUUID( self.uuidDB, "CIFCoreTransformationMatrixToCartesian", 
                        "ZeoliteCIFTransformationMatrixToCartesian_" + zeoname )
    output.append( [ uuid_m_frac_to_cart, "Instance", 
                           "Matrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_m_frac_to_cart,  
                     self.crystOntoPrefix + "hasTransformationMatrixToCartesian", "", "" ] )

    output.append( [ uuid_m_frac_to_cart, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for iy in range(3):
        for ix in range(3):
            uuid_m_comp = tools.getUUID( self.uuidDB, "MatrixComponent", 
                        "MatrixComponentToCartesian"+str(ix)+str(iy)+"_" + zeoname )
            output.append( [ uuid_m_comp, "Instance", 
                           "MatrixComponent", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
            output.append( [ uuid_m_frac_to_cart, "instance", uuid_m_comp,  
                     self.crystOntoPrefix + "hasMatrixComponent", "", "" ] )


            output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.crystOntoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "integer" ] )

            output.append( [ self.crystOntoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "integer" ] )

            output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )

    uuid_v_frac_to_cart = tools.getUUID( self.uuidDB, "Vector", 
                        "ZeoliteCIFTransformationVectorToCartesian_" + zeoname )
    output.append( [ uuid_v_frac_to_cart, "Instance", 
                           "CIFCoreTransformationVectorToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_v_frac_to_cart,  
                     self.crystOntoPrefix + "hasTransformationVectorToCartesian", "", "" ] )

    output.append( [ uuid_v_frac_to_cart, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for ix in range(3):
        uuid_v_comp = tools.getUUID( self.uuidDB, "VectorComponent", 
                    "VectorComponentToCartesian" + str(ix) + "_" + zeoname )
        output.append( [ uuid_v_comp, "Instance", 
                       "VectorComponent", "", "", "" ] )
                       #"CIFCoreTransformationVectorToCartesian", "", "", "" ] )
        output.append( [ uuid_v_frac_to_cart, "instance", uuid_v_comp,  
                         self.crystOntoPrefix + "hasMatrixComponent", "", "" ] )

        output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.crystOntoPrefix + "hasIndex", "Data Property", 
                         uuid_v_comp, "", ix, "integer" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )


    ################# Cartesian to Fractional ########################
    uuid_m_cart_to_frac = tools.getUUID( self.uuidDB, "CIFCoreTransformationMatrixToFractional", 
                        "ZeoliteCIFTransformationMatrixToFractional_" + zeoname )
    output.append( [ uuid_m_cart_to_frac, "Instance", 
                           "Matrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_m_cart_to_frac,  
                     self.crystOntoPrefix + "hasTransformationMatrixToFractional", "", "" ] )

    output.append( [ uuid_m_cart_to_frac, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for iy in range(3):
        for ix in range(3):
            uuid_m_comp = tools.getUUID( self.uuidDB, "MatrixComponent", 
                        "MatrixComponentToFractional"+str(ix)+str(iy)+"_" + zeoname )
            output.append( [ uuid_m_comp, "Instance", 
                           "MatrixComponent", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
            output.append( [ uuid_m_cart_to_frac, "instance", uuid_m_comp,  
                     self.crystOntoPrefix + "hasComponent", "", "" ] )

            output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.crystOntoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "integer" ] )

            output.append( [ self.crystOntoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "integer" ] )

            output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.reciprocal_lattice.matrix[iy][ix]/TWOPI, 12), "decimal" ] )

    uuid_v_cart_to_frac = tools.getUUID( self.uuidDB, "Vector", 
                        "ZeoliteCIFTransformationVectorToFractional" + zeoname )
    output.append( [ uuid_v_cart_to_frac, "Instance", 
                           "Vector", "", "", "" ] )
                           #"CIFCoreTransformationVectorToFractional", "", "", "" ] )

    output.append( [ uuid_cif_core_trans, "instance", uuid_v_cart_to_frac,  
                     self.crystOntoPrefix + "hasTransformationVectorToFractional", "", "" ] )

    output.append( [ uuid_v_cart_to_frac, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for ix in range(3):
        uuid_v_comp = tools.getUUID( self.uuidDB, "VectorComponent", 
                    "VectorComponentToFractional" + str(ix) + "_" + zeoname )
        output.append( [ uuid_v_comp, "Instance", 
                       "VectorComponent", "", "", "" ] )
                       #"CIFCoreTransformationVectorToCartesian", "", "", "" ] )
        output.append( [ uuid_v_cart_to_frac, "instance", uuid_v_comp,  
                         self.crystOntoPrefix + "hasComponent", "", "" ] )

        output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.crystOntoPrefix + "hasIndex", "Data Property", 
                         uuid_v_comp, "", ix, "integer" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )
                         #round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )

    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrTransform()
 
  def arrAtomSite( self, subject, predicate, zeoname ):
    """
    subject   - Is the full hame of instance of class CrystalInformation, 
                which contains this UnitCell class.
    predicate - Is the Object Property linking the Subject and the current UnitCell.
                Typically is should be equal to "hasUnitCell".
    """

    if subject.find( "CrystalInformation" ) < 0:
      logging.warning( " Subject in arrAtomSite() is '" + subject + "'," +
                       " expecting the name to contain '" + "CrystalInfromation" + "'." ) 

    if predicate.find( "hasAtomicStructure" ) < 0:
      logging.warning( " Predicate in arrAtomSite() is '" + predicate + "'," +
                       " but expecting '" + "hasAtomicStructure" + "'." ) 


    logging.warning( "arrAtomSite() is not implemented yet" )

    output = []

    #uuidDB = tools.loadUUID( )
    #uuidDB = self.uuidDB

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")
    #dataIn = tools.readCsv( path )
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return []

    structure = Structure.from_file( path )

    uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )

    uuid_cif = tools.getUUID( self.uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )
 

    return output
    pass # CsvMaker.arrAtomSite()


  def arrUnitCell( self, subject, predicate, zeoname ):
    """
    subject   - Is the full hame of instance of class CrystalInformation, 
                which contains this UnitCell class.
    predicate - Is the Object Property linking the Subject and the current UnitCell.
                Typically is should be equal to "hasUnitCell".
    """

    #print( "'" + subject + "'", "'" + predicate + "'" )
    if subject.find( "CrystalInformation" ) < 0:
      logging.warning( " Subject in arrUnitCell() is '" + subject + "'," +
                       " expecting the name to contain '" + "CrystalInfromation" + "'." ) 

    if predicate.find( "hasUnitCell" ) < 0:
      logging.warning( " Predicate in arrUnitCell() is '" + predicate + "'," +
                       " but expecting '" + "hasUnitCell" + "'." ) 

    output = []
    TWOPI = 2 * math.pi

    # TODO to add get_crystal_system(), get_space_group_number(), 
    #      https://pymatgen.org/pymatgen.symmetry.html

    #uuidDB = tools.loadUUID( )
    #uuidDB = self.uuidDB

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

    #print( "Using a test case 913885.cif for the UnitCell class" )
    #path = os.path.join( "test", "913885.cif" ) 
    #dataIn = tools.readCsv( path )
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return

    structure = Structure.from_file( path )

    #print( zeoname )
    #print( "  ", structure.lattice ) # These are 3 unit cell vectors 
    #print( "  ", structure.lattice.gamma ) # angles, similarly a,b,c
    #print( "  ", structure.lattice.volume ) # volume
    #print( "  ", structure.lattice.reciprocal_lattice.a ) # reciprocal parameters
    #print( "  ", structure.lattice.reciprocal_lattice.matrix[0][0] ) # reciprocal parameters
    #print( "  ", structure.lattice.reciprocal_lattice.matrix ) # reciprocal parameters
    #print( "  ", structure.lattice.matrix ) # reciprocal parameters
    #print( TWOPI * numpy.linalg.inv( structure.lattice.matrix ) )

    
    #uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", subject )
    # Define class instances:

    #uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    #uuid_zeolite = subject

    #output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
    #                 uuid_zeolite, "", zeoname.strip(' "'), "string" ] )

    uuid_cif = tools.getUUID( self.uuidDB, "CrystalInformation", "ZeoliteCIF_" + zeoname )
    output.append( [ uuid_cif, "Instance", "CrystalInformation", "", "", "" ] )
 
    uuid_cif_uc = tools.getUUID( self.uuidDB, "UnitCell", "UnitCell_" + zeoname )
    output.append( [ uuid_cif_uc, "Instance", "UnitCell", "", "", "" ] )

    # Define relation between the class instances:
    output.append( [ subject, "instance", uuid_cif,  
    #output.append( [ uuid_zeolite, "instance", uuid_cif,  
                     self.crystOntoPrefix + "hasCrystalInformation", "", "" ] )

    output.append( [ uuid_cif, "Instance", uuid_cif_uc,  
                     predicate, "", "" ] )
                     #self.crystOntoPrefix + "hasUnitCell", "", "" ] )

    ###########################################
    if not None == self.unitCellLengths:
      output += self.unitCellLengths.getArrVector( uuid_cif_uc, 
                self.crystOntoPrefix + "hasUnitCellLengths" ) 

    if not None == self.unitCellAngles:
      output += self.unitCellAngles.getArrVector(  uuid_cif_uc, 
                self.crystOntoPrefix + "hasUnitCellAngles" ) 

    if not None == self.unitCellRecipLengths:
      output += self.unitCellRecipLengths.getArrVector( uuid_cif_uc, 
                self.crystOntoPrefix + "hasReciprocalUnitCellLengths" )

    if not None == self.unitCellRecipAngles:
      output += self.unitCellRecipAngles.getArrVector( uuid_cif_uc, 
                self.crystOntoPrefix + "hasReciprocalUnitCellAngles" )

    # Vector to keep three Unit Cell vectors (a,b,c):
    uuid_uc_vec_abc = tools.getUUID( self.uuidDB, "UnitCellVectorSet", "UnitCellVectorSet_" + zeoname )
    output.append( [ uuid_uc_vec_abc, "Instance", "UnitCellVectorSet", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_vec_abc,  
                     self.crystOntoPrefix + "hasUnitCellVectorSet", "", "" ] )

    output += self.unitCellVectorA.getArrVector( uuid_uc_vec_abc, self.crystOntoPrefix + "hasLatticeVector" )
    output += self.unitCellVectorB.getArrVector( uuid_uc_vec_abc, self.crystOntoPrefix + "hasLatticeVector" )
    output += self.unitCellVectorC.getArrVector( uuid_uc_vec_abc, self.crystOntoPrefix + "hasLatticeVector" )

    # Vector to keep three Reciprocal Unit Cell vectors (a,b,c):
    uuid_uc_r_vec_abc = tools.getUUID( self.uuidDB, "UnitCellVectorSet", "ReciprocalUnitCellVectorSet_" + zeoname )
    output.append( [ uuid_uc_r_vec_abc, "Instance", "ReciprocalUnitCellVectorSet", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_r_vec_abc,  
                     self.crystOntoPrefix + "hasReciprocalUnitCellVectorSet", "", "" ] )

    output += self.unitCellRecipVectorA.getArrVector( uuid_uc_r_vec_abc, self.crystOntoPrefix + "hasLatticeVector" )
    output += self.unitCellRecipVectorB.getArrVector( uuid_uc_r_vec_abc, self.crystOntoPrefix + "hasLatticeVector" )
    output += self.unitCellRecipVectorC.getArrVector( uuid_uc_r_vec_abc, self.crystOntoPrefix + "hasLatticeVector" )

    ###########################################

    # Unit Cell volume, single value, not vector:

    uuid_uc_volume = tools.getUUID( self.uuidDB, 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure", 
                     "UnitCellVolume_" + zeoname )

    output.append( [ uuid_uc_volume, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure",
                     "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_volume,  
                     self.crystOntoPrefix + "hasUnitCellVolume", "", "" ] )

    output.append( [ "http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", 
                     "Data Property", uuid_uc_volume, "", 
                     round(structure.lattice.volume, 12) , "decimal" ] )

    output.append( [ uuid_uc_volume, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/cubicAngstrom", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    ###########################################

    # The symmetry information of the unit cell.
    # TODO to add get_crystal_system(), get_space_group_number(), 
    #      https://pymatgen.org/pymatgen.symmetry.html
 
    sga = pymatgen.symmetry.analyzer.SpacegroupAnalyzer( structure )

    #print( "Zeo Name = ", zeoname )
    if isinstance( sga._space_group_data, dict ):
      if "number" in sga._space_group_data.keys():
        """
        print( "SG number:", sga._space_group_data["number"], sga.get_space_group_number(), sga.get_crystal_system() )
        print( "   ", #sga._get_symmetry(), 
                      sga.get_hall(), 
                      sga.get_lattice_type(), #sga.get_symmetry_dataset() 
                      #sga.int_symbol()
                      #sga._abc_impl
                      )
        """
        #if isinstance( sga.get_crystal_system(), str) :
        output.append( [ self.crystOntoPrefix + "hasLatticeSystem", 
                           "Data Property", uuid_cif_uc, "", 
                           sga.get_crystal_system() , "string" ] )
        output.append( [ self.crystOntoPrefix + "hasSymmetryNumber", 
                           "Data Property", uuid_cif_uc, "", 
                           sga.get_space_group_number() , "integer" ] )
   


    return output
    pass # CsvMaker.arrUnitCell()

  def arrTiles( self, subject, predicate, zeoname ):
    """
    subject   - Is the full hame of instance of class CrystalInformation, 
                which contains this TiledStructure class.
    predicate - Is the Object Property linking the Subject and the current TiledStructure.
                Typically is should be equal to "hasTiledStructure".
    """

    if subject.find( "TiledStructure" ) < 0:
      logging.warning( " Subject in arrTiles() is '" + subject + "'," +
                       " expecting the name to contain '" + "TiledStructure" + "'." ) 

    if predicate.find( "hasTiledStructure" ) < 0:
      logging.warning( " Predicate in arrTiles() is '" + predicate + "'," +
                       " but expecting '" + "hasTiledStructure" + "'." ) 

    output = []

    path   = os.path.join( self.inputDir, "Tile-signature-2023.csv" )
    dataIn = tools.readCsv( path )

    data = tilesignature.getDataByCode( dataIn, zeoname )

    uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )

    uuid_tstructure = tools.getUUID( self.uuidDB, "TiledStructure", "TiledStructure_" + zeoname )
    output.append( [ uuid_tstructure, "Instance", "TiledStructure",
                     "", "", "" ] )

    output.append( [ uuid_zeolite, "Instance", uuid_tstructure,  
                     self.crystOntoPrefix + "hasTiledStructure", "", "" ] )

    output.append( [ self.crystOntoPrefix + "hasTileSignature", "Data Property", 
                     uuid_tstructure, "", data[2].strip(' "'), "string" ] )

    ### Begin of transitivity
    uuid_tile_trans = tools.getUUID( self.uuidDB, "Vector", "TileNumber_" + zeoname + "_transitivity" )
    output.append( [ uuid_tile_trans, "Instance", "Vector",
                     "", "", "" ] )

    uuid_tile_transP = tools.getUUID( self.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityP" )
    output.append( [ uuid_tile_transP, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transQ = tools.getUUID( self.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityQ" )
    output.append( [ uuid_tile_transQ, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transR = tools.getUUID( self.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityR" )
    output.append( [ uuid_tile_transR, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transS = tools.getUUID( self.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityS" )
    output.append( [ uuid_tile_transS, "Instance", "VectorComponent",
                     "", "", "" ] )

    output.append( [ uuid_tstructure, "Instance", uuid_tile_trans ,  
                     self.crystOntoPrefix + "hasTransitivity", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transP,  
                     self.crystOntoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transQ,  
                     self.crystOntoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transR,  
                     self.crystOntoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transS,  
                     self.crystOntoPrefix + "hasComponent", "", "" ] )

    trans = self.getTransitivity( data[1] )

    output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transP, "", "p", "string" ] )
    output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                                        uuid_tile_transP, "", trans[0], "integer" ] )

    output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transQ, "", "q", "string" ] )
    output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                                        uuid_tile_transQ, "", trans[1], "integer" ] )

    output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transR, "", "r", "string" ] )
    output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                                        uuid_tile_transR, "", trans[2], "integer" ] )

    output.append( [ self.crystOntoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transS, "", "s", "string" ] )
    output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                                        uuid_tile_transS, "", trans[3], "integer" ] )
    ### End of transitivity

    cages = tilesignature.cellToCages( data[2] )
    signN = data[ 6].split( "+" )
    codeN = data[ 7].split( "+" )
    vertN = data[ 9].split( "+" )
    edgeN = data[10].split( "+" )
    faceN = data[11].split( "+" )

    #print( "faces =", faceN )
    for ic, cage in enumerate(cages):
      uuid_tile     = tools.getUUID( self.uuidDB, "Tile", "Tile_" + zeoname + "_cage" + str(ic+1) )
      uuid_tile_num = tools.getUUID( self.uuidDB, "TileNumber", "TileNumber_" + zeoname + "_cage" + str(ic+1) )

      output.append( [ uuid_tile, "Instance", "Tile", 
                       "", "", "" ] )

      output.append( [ uuid_tstructure, "Instance", uuid_tile, 
                       self.crystOntoPrefix + "hasTile", "", "" ] )

      output.append( [ self.crystOntoPrefix + "hasTileCode", "Data Property", 
                                         uuid_tile, "", codeN[ic], "string" ] )

      output.append( [ self.crystOntoPrefix + "hasNumberOfFaces", "Data Property", 
                                         uuid_tile, "", faceN[ic], "integer" ] )

      output.append( [ self.crystOntoPrefix + "hasNumberOfEdgges", "Data Property", 
                                         uuid_tile, "", edgeN[ic], "integer" ] )

      output.append( [ self.crystOntoPrefix + "hasNumberOfVertices", "Data Property", 
                                         uuid_tile, "", vertN[ic], "integer" ] )

      # TODO
      #output.append( [ self.ontoPrefix + "hasTileSymmetry", "Data Property", 
      #                                   uuid_tile, "", symmN[ic], "string" ] )

      output.append( [ self.crystOntoPrefix + "hasTileSignature", "Data Property", 
                                         uuid_tile, "", signN[ic], "string" ] )

      output.append( [ uuid_tile_num, "Instance", "TileNumber", 
                       "", "", "" ] )

      output.append( [ uuid_tstructure, "Instance", uuid_tile_num, 
                       self.crystOntoPrefix + "hasTileNumber", "", "" ] )

      output.append( [ uuid_tile_num, "Instance", uuid_tile, 
                       self.crystOntoPrefix + "isNumberOf", "", "" ] )

      output.append( [ self.crystOntoPrefix + "hasValue", "Data Property", 
                       #uuid_tile_num, "", cage[0], "" ] )
                       uuid_tile_num, "", cage[0], "integer" ] )
                       #uuid_tile_num, "", cage[0], 
                       #"http://www.w3.org/2001/XMLSchema#integer" ] )


      faces = tilesignature.cageToFaces( cage[1] )

      for f, face in enumerate(faces):
        #print( "face = ", face )
        
        suffix = "_" + zeoname + "_cage" + str(ic+1) + "_face" + str(f+1)
        uuid_tile_face     = tools.getUUID( self.uuidDB, "TileFace", "TileFace"       + suffix )
        uuid_tile_face_num = tools.getUUID( self.uuidDB, "TileFaceNumber", "TileFaceNumber" + suffix )

        output.append( [ uuid_tile_face,     "Instance", "TileFace", 
                         "" , "", "" ] )

        output.append( [ uuid_tile,   "Instance", uuid_tile_face,   
                         self.crystOntoPrefix + "hasTileFace" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", "TileFaceNumber",  
                         "" , "", "" ] )
        
        output.append( [ uuid_tile, "Instance", uuid_tile_face_num,  
                         self.crystOntoPrefix + "hasTileFaceNumber" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", uuid_tile_face,
                         self.crystOntoPrefix + "isNumberOf" , "", "" ] )

        output.append( [ self.crystOntoPrefix + "hasValue", "Data Property", 
                              #uuid_tile_face_num, "", face[1], "" ] )
                              uuid_tile_face_num, "", face[1], "integer" ] )
                              #uuid_tile_face_num, "", face[1], 
                              #"http://www.w3.org/2001/XMLSchema#integer" ] )

        output.append( [ self.crystOntoPrefix + "hasNumberOfEdges", "Data Property", 
                              #uuid_tile_face, "", int(face[0]), "" ] )
                              uuid_tile_face, "", int(face[0]), "integer" ] )
                              #uuid_tile_face, "", int(face[0]), 
                              #"http://www.w3.org/2001/XMLSchema#integer" ] )

        output.append( [ self.crystOntoPrefix + "hasFaceCode", "Data Property", 
                                       uuid_tile_face, "", str("UNDEFINED"), "string" ] )

        # TODO add face code (?), like 14a, 14b, etc
        #output.append( [ self.ontoBase + "#hasFaceCode", "Data Property", 
        #                               uuid_tile_face, "", int(cell[0]), "string" ] )


    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrTiles()

  def getTransitivity( self, strValue ):
    if not isinstance( strValue, str ):
      logging.error( " input must be string" )
      return None

    short = strValue.strip()
    if short[0] != "[" or short[-1] != "]":
      logging.error( " Transitivity must be in [] brackets" )

    trans = [None]*4
    i = 1   # position of character in the string
    #it = 0  # iterator for transitivity vector. 0 to 3.
    for it in range( 4 ):
      if short[i] != "(":
        trans[it] = short[i]
        i += 1
      else:
        i += 1
        trans[it] = ""
        while short[i] != ")":
          trans[it] += short[i]
          i += 1
        else:
          i += 1 # To skip the ")" bracket
      #it += 1
      #print( "it = ", it, ":", trans, ", i =", i )
    #print( trans )
    return trans

  # Remove error bars from the CIF file and save the result to a temporary file
  def splitErrorBarLoop( self, line, headers, file_line ):
    #print( "Running error bar loop", line )
    #words = line.split()
    nBracket = 0
    words = tools.strSplit( line )
    #words = tools.strSplit( line.relpace( "\t", " " ) ) # Some CIFs have a tab in a line
    #print ( ">>>>>>>>>>> ", words )

    if len(words) == 0:
      logging.error( " Empty line " + file_line )
      return line, 0

    if len(words) != len(headers):
      logging.error( " Number of elements on line is different from the " +
                     "definition of the loop: " + str(len(words)) +" vs " + 
                     str(len(headers)) + ": " + str(words) + " vs " + str(headers) +
                     " " + file_line + "." )
      return line, 0

    lineNew = line
    #print( self.entriesWithUncertainties )
    #for i in range( len(headers) ):
    for ih, h in enumerate(headers):
      #print( "=== '" + h + "' ===" )
      if h in self.entriesWithUncertainties:
        #print( "   need to process" )
        #logging.warning( " Found one of the entries in a loop" )

        vOut, eOut = self.splitErrorBar( words[ih].strip(), file_line )
        if eOut != "":
          nBracket += 1
          #self.setValueAndError( cifName, h, vOut, eOut )

        pos = line.find( words[ih] )
        lineNew = lineNew.replace( words[ih], vOut )

    #print( "lineNew =", lineNew )
    return lineNew, nBracket
    pass # splitErrorBarLoop()

  def setValueAndError( self, cifName, entry, value, error ):
    """

    """
    '''
    self.entriesWithUncertainties = [ 
                  "_cell_volume", "_cell_measurement_temperature",
                  "_diffrn_ambient_temperature",
                  "_atom_site_fract_x", "_atom_site_fract_y", "_atom_site_fract_z",
                  "_atom_site_U_iso_or_equiv",
                  "_atom_site_aniso_U_11", "_atom_site_aniso_U_22", 
                  "_atom_site_aniso_U_33", "_atom_site_aniso_U_23", 
                  "_atom_site_aniso_U_13", "_atom_site_aniso_U_12", 
                  "_geom_bond_distance", "_geom_angle", 
                  "_refine_ls_extinction_coef"
                  ]
    '''

#    #cifName = "QQQQQQ"
#    self.unitCellLengths = OntoVector( uuidDB = self.uuidDB, 
#                                       myClass = "UnitCellLengths", 
#                                       myName  = "UnitCellLengths_" + cifName,
#       #                                myUnit  = "angstrom" )
#       myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )

    logging.warning( "setValueAndError(): " + "cifName = '" + str(cifName) + "', " +
           "entry = '" + str(entry) + "', " + 
           "value = '" + str(value) + "', " + "error = '" + str(error) + "'." )

    if "_cell_length_a" == entry:
      self.unitCellLengths.addComponent( label = "a", value = value, error = error )

    elif "_cell_length_b" == entry:
      self.unitCellLengths.addComponent( label = "b", value = value, error = error )

    elif "_cell_length_c" == entry:
      self.unitCellLengths.addComponent( label = "c", value = value, error = error )

    elif "_cell_angle_alpha" == entry:
      self.unitCellAngles.addComponent( label = "alpha", value = value, error = error )

    elif "_cell_angle_beta" == entry:
      self.unitCellAngles.addComponent( label = "beta", value = value, error = error )

    elif "_cell_angle_gamma" == entry:
      self.unitCellAngles.addComponent( label = "gamma", value = value, error = error )



      pass
    else:
      logging.error( " Unknown entry to store data with error: '" + entry + "'." )

    pass # setValueAndError()


  def splitStr( self, value ):
    """
    Function splits a string of a form "12.345(6)" into two strings: "12.345" and "0.006".
    """

    pos1 = value.find( "(" )
    pos2 = value.find( ")" )
    vOut = value[:pos1] + value[pos2+1:]
    eOut = value[pos1+1:pos2]

    ''' This sometimes return value like 0.007000000001, last digit round effect
    n = len(eOut )
    vOut = vOut
    v = 0
    factor = ""
    for ix, x in enumerate(vOut):
      if ix == len(vOut) - 1:
        factor += "1"
      elif "." == x:
        factor += "."
      else:
        factor += "0"
    eOut = str( float(factor) * float(eOut) )
    '''

    #iv = len(vOut) - 1
    #ie = len(eOut) - 1
    ie = 1
    factor = []
    for ix,x in enumerate(vOut[::-1]):
      if "." == x:
        factor.insert( 0, "." )
      elif ie <= len( eOut ):
        factor.insert( 0, eOut[-ie] )
        ie += 1
      else:
        factor.insert( 0, "0" )
        ie += 1

    #print( factor )
    factor = "".join( factor )
    if factor.find( "." ) < 0:
      eOut = str( int(factor) )
    else:
      eOut = str( float(factor) )
    #for i in range( len( eOut ) ):
      #vOut[-i-1] = eOut[-i-1]
    return vOut, eOut
    pass

  def splitErrorBar( self, value, file_line ):
    #print( "Starting splitErrorBar() ================== " )
    vOut = value 
    eOut = ""

    if not isinstance( value, str ):
      logging.error( " Impossible error: not a string " + line + " " 
                     + file_line )
      return vOut,eOut

    pos1 = value.find( "(" )
    pos2 = value.find( ")" )
    if pos1 < 0 and pos2 < 0:
      #logging.info( " Brackets are not detected. " + file_line )
      pass
    elif pos1 >= 0 and pos2 >= 0 and pos1 < pos2:
      #print( "pos1 = ", pos1, " pos2 = ", pos2 )
      vOut,eOut = self.splitStr(value) 
    else:
      logging.error( " Something is wrong with brackets: '" + line + 
                     "' . " + file_line )

    #print( "value, vOut, eOut =", value, vOut, eOut )
    return vOut, eOut
    pass


  #def getCifLineRanges( self, fileIn ):
  #  return [ 0, 100, 400 ]

  def cleanCif( self, fileIn ):
    """ 
    There are two situations when CIF file is not directly readable:
    1) Single file contains several CIF data structures (concatenation of several files)
    2) CIF file contains uncertainties (a.k.a. error bars) 

    In both cases I read the file line-by-line and save new (corrected) file(s) 
    to a temporary directory. 
    The new file(s) is/are proper CIF file(s), and I can read it/them by the standard functions.

    Return a list containing new filename(s).
    If no correction was necessary the list is empty.

    There are two independent operations:
    - find the ranges of individual CIF files in a combined CIF file,
    - divide the combined CIF file and save to several files,
    - detect brackets.
    """

    # Here lineRanges is the list of the first lines of the individual CIF(s) 
    #    ??? and the last line appended behind.
    # Later the file can be divided into several using this information.
    lineRanges = tools.getCifLineRanges( fileIn )
    filesOut = []

    tmpDir = "tmp"
    if not os.path.isdir( tmpDir ):
      os.makedirs(  tmpDir )
    
    for i in range(len(lineRanges)-1):
      #print( "i =", i, ", ", lineRanges[i] )

      nBracket = self.readWithUncertainties( fileIn, zeoname, lineRanges[i], lineRanges[i+1] )
      if ( nBracket > 0 ) or ( len(lineRanges) - 1 > 1 ):
        fileBase, ext = os.path.splitext( os.path.basename(fileIn) )
        print( "Input file name and extension:", fileBase, ext )

        fileOut = os.path.join( tmpDir, fileBase + "_" + str(i+1) + ext )
        filesOut.append( fileOut )
        self.readWithUncertainties( fileIn, zeoname, lineRanges[i], lineRanges[i+1], fileOut = fileOut )

    #else:
    #fileOut = "after-913885.cif"

    return filesOut
    pass # cleanCif()

  def readWithUncertainties( self, fileIn, cifName, lineFr = None, lineTo = None, 
                             fileOut = "", save = False ):
    """
    This function can do three different operations:
    1) read fileIn and count the number of brackets (i.e. the uncertainties)
       The argument fileOut = "" or not specified.
    2) Save a modified file whith uncertanties removed, so that the file
       can be used for reading by standard libraries.
       Specify the fileOut a path.
    3) Read both value and uncertainty and assign to the internal variables,
       which later can be saved into an ABox formal (.csv).
       Flag save = True (default save = False)

      Parameters:
      fileIn  - the input CIF file to be read,
      cifName - is the unique identifier for the cif to be stored in abox
      lineFr  - 
      lineTo  - 
      fileOut - output file to save data with uncertainties removed,
      save    - boolean flag, whether to save the data to internal variables.
      Return: 
      In all 3 cases function returns the number of detected uncertainties.
      In case of error the function returns negative value.
    """

    self.entriesWithUncertainties = [ "_cell_length_a", "_cell_length_b", "_cell_length_c", 
                  "_cell_angle_alpha", "_cell_angle_beta", "_cell_angle_gamma",
                  "_cell_volume", "_cell_measurement_temperature",
                  "_diffrn_ambient_temperature",
                  "_atom_site_fract_x", "_atom_site_fract_y", "_atom_site_fract_z",
                  "_atom_site_U_iso_or_equiv",
                  "_atom_site_aniso_U_11", "_atom_site_aniso_U_22", 
                  "_atom_site_aniso_U_33", "_atom_site_aniso_U_23", 
                  "_atom_site_aniso_U_13", "_atom_site_aniso_U_12", 
                  "_geom_bond_distance", "_geom_angle", 
                  "_refine_ls_extinction_coef"
                  ]

    if not os.path.isfile( fileIn ):
      logging.error( " Input file '" + fileIn + "' does not exist in cleanCif()." )
      return -1

    fIn = open( fileIn )
    
    if len(fileOut) > 0:
      fOut = open( fileOut, "w", encoding="utf8" )

    inLoop = False
    countBrackets = 0

    for il, line in enumerate(fIn):
      file_line = "In file '" + fileIn + "' line " + str(il+1)
      #print( file_line )

      if il >= 220:
        logging.debug( " Break after 75 lines for testing. " + file_line )
        #break
        pass

      pos1 = line.find( "#" )
      pos2 = line.find( ";" )
      if   0 == pos1:
        logging.info( " Found a comment in string '" + line.strip() + "'. " + file_line )
        #short = line[ :pos] + "\n"
        if len(fileOut) > 0:
          fOut.write( line )
        continue

      elif 0 == pos2:
        logging.info( " Found a comment in string '" + line.strip() + "'. " + file_line )
        #short = line[ :pos] + "\n"
        if len(fileOut) > 0:
          fOut.write( line )
        continue

      elif pos1 > 0 or pos2 > 0:
        logging.warning( " Comment starts not from beginning of the line: '" + 
                       line.strip() + "' " + file_line + "."  
                       #+ " This is not supported by readWithUncertainties()." 
                     )
        if len(fileOut) > 0:
          fOut.write( line )
        continue

      #words = line.strip().split()
      words = tools.strSplit( line )
      #print( "on line", il, "words:", words )
      
      if len(words) == 0:
        #logging.info( " Empty string " + file_line )
        if len(fileOut) > 0:
          fOut.write( line )
        inLoop = False
        continue
        pass

      elif "loop_" == words[0].strip():
        #logging.info( " Found a 'loop_' option" )
        inLoop = True
        inLoopHead = True
        inLoopBody = False
        self.loopHeaders = []
        if len(fileOut) > 0:
          #fOut.write( "added 'loop_': " + line )
          fOut.write( line )
        continue

      elif inLoop:
        #logging.info( " In inLoop option" )
        #print( "Inside the loop" )
        
        if inLoopHead:
          #print( "Inside the loop head" )
          if "_" == words[0][0]:
            self.loopHeaders.append( words[0].strip() )
            if len(fileOut) > 0:
              fOut.write( line )
          else:
            inLoopHead = False
            inLoopBody = True

        if inLoopBody:
          if "_" == words[0][0]:
            inLoop = False
            #logging.info( " inLoop is set to False" )
            if len(fileOut) > 0:
              fOut.write( line )
            #self.loopHeaders = []
            
          else:
            lineNew, nbrac = self.splitErrorBarLoop( line, self.loopHeaders, file_line )
            countBrackets += nbrac
            #print( "after stplitErrorBarLoop():", lineNew, nbrac )
            if len(fileOut) > 0:
              fOut.write( lineNew )
            continue
            pass

          pass

        #print( "headers =", self.loopHeaders )

      elif len(words) > 2:
        #logging.info( " Length of string = " + str(len(words)) + " " + file_line )
        if len(fileOut) > 0:
          fOut.write( line )
        pass

      elif "_" == words[0][0]:
        #print( "Checking property", words[0], "is it in list of known?", words[0] in self.entriesWithUncertainties ) 
        if False == inLoop:
          if len(words) == 1:
            #logging.info( " Only 1 entry in '" + line.strip() + "' " + file_line + ". I skip this case." )
            if len(fileOut) > 0:
              fOut.write( line )
            continue
          elif len(words) > 2:
            #logging.info( " More than 2 entries in '" + line.strip() + "' " + file_line + ". I skip this case." )
            if len(fileOut) > 0:
              fOut.write( line )
            continue
            
          elif words[0] in self.entriesWithUncertainties:
            logging.info( " Found one of the entries:" + words[0]  )
            vOut, eOut = self.splitErrorBar( words[1], file_line )
            if "" != eOut:
              countBrackets += 1
              self.setValueAndError( cifName, words[0], vOut, eOut )

            pos = line.find( words[1] )

            newLine = line.replace( words[1], vOut )
            if len(fileOut) > 0:
              fOut.write( newLine )
              #fOut.write( line[:pos] + vOut + "\n" )
            continue

          elif words[0] in self.cifStandard:
            if len(fileOut) > 0:
              fOut.write( line )
          else:
            logging.warning( " Unknown situation. Line = '" + line.strip() + 
                             "'. " + file_line + "." )
            if len(fileOut) > 0:
              fOut.write( line )
        else:
          if len(fileOut) > 0:
            fOut.write( line )

      elif len(words) == 2:
        #logging.warning( " Length of string = " + str(len(words)) + " " + file_line )
        if len(fileOut) > 0:
          fOut.write( line )
        pass

      else:
        #logging.warning( " default else option." )
        if len(fileOut) > 0:
          fOut.write( line )
        pass

    fIn.close()
    if len(fileOut) > 0:
      fOut.close()

    #print( "Number of brackets =", countBrackets )
    return countBrackets
    pass # readWithUncertainties()


  def makeCsvs( self ):
    errCount = 0

    self.prepare()

    for z in self.zeoList:
      #print( "In zeolist z =", z )
      arr  = self.arrInit( z )

      self.loadCifZeolite( z ) 
      #self.evalCifData()

      uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + z )
     

      arr += self.arrUnitCell( uuid_zeolite, self.crystOntoPrefix + "hasUnitCell", z )

      arr += self.arrAtomSite( uuid_zeolite, self.crystOntoPrefix + "hasUnitCell", z )

      #arr += self.arrTiles( "", "", z )
      #arr += self.arrTransform( "", "", z )

      #print( "arr =", arr )
      #csvWrite( arr )
      path = os.path.join( self.outputDir, z + ".csv" )
      tools.writeCsv( path, arr )

      #errCount += 1
      #logging.warning( "Not implemented creating zeolite csv" )
      pass

    self.finalize() # << Important! Saves the current list of the UUIDs

    if errCount > 0:
      logging.warning( "Detected " + str(errCount) + " errors" )

    pass # CsvMaker.makeCsvs()

  pass # class CsvMaker


  """
  0) Prepare the header lines (standard).
  1) For CIF related data:
    a) Load CIF data and store internally.
       by loadCifZeolite()   - zeolite-specific, calls the next funtion:
                               Some CIF files may be a merge of several files ??? TODO

          loadCifStructure() - this function does not know about zeolites, 
                               it can be used for any kind of CIF file.
    b) Modify CIF data (if necessary).
       by ???
    c) Save data related to CIF (it can be in different structure, 
       according to the settings).
       arrUnitCell()
       arrAtomSite()
       arrTransform()
  """ 




if __name__ == "__main__":
  
  settings = CommandLine()

  a = CsvMaker()

  a.makeCsvs()

  #fileIn  = os.path.join( "test", "913885.cif" )
  #filesOut = a.cleanCif( fileIn )
  #print( "Created", len(filesOut), "file(s) after clean-up:", filesOut )

  #input_strings = [ "123450(16)", "123450(160)", "1234.5(16)", "123.45(16)", "12.345(16)"]
  #for input_string in input_strings:
  #  value, error = a.splitStr( input_string )
  #  print(f"Input: {input_string}, Value: {value}, Error: {error}")
    
  #input_strings = [ "1 2 3 4 5", "1   2  3   4  5\n", "1 '2 3' 4 5", "1 2 3 '4   5'"  ]
  #for input_string in input_strings:
  #  out = tools.strSplit(input_string )
  #  print( f"Input: {input_string}, Out: {str(out)}" )
  #  #print( out )


