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

TODO:
- Not urgent: report the number of warnings (like in entityfizer)

"""

# TODO Add the space group number to UnitCell
#'_space_group_IT_number'
#'_symmetry_Int_Tables_number'

#TODO
#'_symmetry_equiv_pos_site_id'
#This definition has been superseded and is retained here only for archival purposes. 
#Use instead '_space_group_symop_id' 

#TODO
#'_symmetry_space_group_name_H-M'
#This definition has been superseded and is retained here only for archival purposes. 
#Use instead '_space_group_name_H-M_alt' 

#TODO
#'_symmetry_space_group_name_Hall'
#This definition has been superseded and is retained here only for archival purposes. 
#Use instead '_space_group_name_Hall' 

#TODO
#'_symmetry_cell_setting'
#This definition has been superseded and is retained here only for archival purposes. 
#Use instead '_space_group_crystal_system' 



import os
import csv

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
import datetime

from pymatgen.core.structure import Structure, Lattice
from pymatgen.symmetry.analyzer import SpacegroupAnalyzer
import pymatgen

def splitStr( value ):
    """
    Function splits a string of a form "12.345(6)" into two strings: "12.345" and "0.006".
    TODO:
    Sometimes the uncertainty is displayed as 12.345,6 
              (see for example ccdcfiles\10.1002_anie.200704222.cif )
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


def cleanString( line ):
    """
    
    """
    if not isinstance( line, str ):
        logging.error( "Input line '" + str(line) + "' is not a string in cleanString()" )
        return str(line)
    pos = line.find( "#" )
    if pos < 0:
        tmp = line.strip()
    else:
        tmp = line[:pos].strip()

    return tmp

def is_http( value ):
    if value.strip().lower().startswith( "http://" ) or \
       value.strip().lower().startswith( "https://" ):
        return True
    return False

# Ontology for Crystal, 
crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

omOntoPrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
OM2_KEYWORDS = [ "angstrom", "cubicAngstrom", "reciprocalAngstrom", 
                 "degree", "dimensionOne",
                 "hasUnit", 
                 "Measure", "Unit" 
               ]

def omInitUnit( unit ):
    """
    A function specially for OM-2 ontology. Define the unit in abox.

    This initialization is redundant. Originally units are loaded from OM-2.
    Ideally this function is not necessary, but for debugging/testing it is 
    easier to define it here and don't load OM-2 .owl every time.
    """
    logging.warning( " Initializing unit '" + unit + "', it should be" + \
                     " removed in the production run." )

    if unit.strip().startswith( "om:" ):
        newUnit = unit.strip()[3:]
    else:
        logging.error( " omInitUnit() expects unit with prefix 'om:'," + \
                       " but got '" + unit + "'." )
        newUnit = unit

    output = []
    output.append( [ omOntoPrefix +  newUnit, "Instance", 
                     omOntoPrefix + "Unit", "", "", "" ] )
    output.append( [ "rdfs:label", "Data Property",
                     omOntoPrefix + newUnit, "", newUnit, "xsd:string" ] )

    return output
    pass # omInitUnit()
    
def omSetUnit( subject, unit ):
    """
    A function specially for OM-2 ontology. Assign the unit to given subject.

    Function expects an instance of class OM-2:Unit from units-of-measurement.
    It is user's responsibility to confirm that this unit exists in OM-2.
    Input must be with short prefix, i.e. for unit 'xxx' input is 'om:xxx'.
    This is to remind user that the unit is from the OM-2 ontology.
    Return:
    A row to be saved in the abox .csv file.
    """
    #print( ">>>>>>>>>> start omSetUnit ", subject, unit )
    if unit.strip().startswith( "om:" ):
        newUnit = unit.strip()[3:]
    else:
        logging.error( " omSetUnit() expects unit with prefix 'om:'," + \
                       " but got '" + unit + "'." )
        newUnit = unit

    if newUnit not in OM2_KEYWORDS:
        logging.error( " Unknown unit '" + unit + "'. You may update the" + \
                       " OM2_KEYWORDS array if unit is missing." )

    output = []
    output.append( [ subject, "Instance", omOntoPrefix + newUnit, 
                                          omOntoPrefix + "hasUnit", "", "" ] )
    return output
    pass # omSetUnit()

def omNameConvert( unitIn ):
    """
    A function specially for OM-2 ontology. Adds the full path in front.
    """
    if unitIn.startswith( "om:" ):
        if unitIn[3:] in OM2_KEYWORDS :
            unitOut = "http://www.ontology-of-units-of-measure.org/resource/om-2/" + \
                      unitIn[3:]
        else:
            logging.error( " Unknown unit '" + unitIn + "', require a full IRI." )
    else:
        unitOut = unitIn

    return unitOut
    pass # omNameConvert()



entriesWithUncertainties = [ "_cell_length_a", "_cell_length_b", "_cell_length_c", 
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


def splitErrorBar( value, file_line ):
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
      vOut,eOut = splitStr(value) 
    else:
      logging.error( " Something is wrong with brackets: '" + line + 
                     "' . " + file_line )

    #print( "value, vOut, eOut =", value, vOut, eOut )
    return vOut, eOut
    pass

def splitErrorBarLoop( line, headers, file_line ):
    """
    Remove error bars from the CIF file and save the result to a temporary file
    """
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
      if h in entriesWithUncertainties:
        #print( "   need to process" )
        #logging.warning( " Found one of the entries in a loop" )

        vOut, eOut = splitErrorBar( words[ih].strip(), file_line )
        if eOut != "":
          nBracket += 1
          #self.setValueAndError( cifName, h, vOut, eOut )

        pos = line.find( words[ih] )
        lineNew = lineNew.replace( words[ih], vOut )

    #print( "lineNew =", lineNew )
    return lineNew, nBracket
    pass # splitErrorBarLoop()


# FIXME
#http://www.w3.org/2000/01/rdf-schema#Literal
class CommandLine:
  #__slots__ = [ "", ""  ]

  def __init__(self):
    parser = argparse.ArgumentParser( description='what is this?' )
    parser.add_argument( '--cif', type=str, default='', help='file name of the CIF file' )

    args = parser.parse_args()
    #print( args.accumulate, args.cif )
    print( "Input CIF file = '" + args.cif + "'." )
    #self.file = 

    pass # __init__()

  def a(self):


    pass

  pass # class CommandLine

class OntoMeasureWithUncertainty:
    """
    self.uuid - is the unique name of the entity of this class.
    """

    __slots__ = [ "uuidDB", "uuid", "className", "itemName", \
                  "aPrefix", "tPrefix",    \
                  "value", "error", "unit" \
                ]
    def __init__( self, itemName, className = "", \
                  tPrefix = None,   aPrefix = None,   \
                  uuidDB = None ):
      """
      itemName  - (required) name of the new variable.

      className - (optional) the class name. If not specified, 
                  the basic OntoCrystal/MeasureWithUncertainty is used.

      tPrefix   - It is possible to assign prefix for TBox for the class and 
                  the properties. 
                  If not specified, then the default OntoCrystal prefix is used.
                  Otherwise whateven specified will be prefixed. 
                  In particular, if empty string is given then none will be 
                  prefixed and the prefix will be assigned later 
                  by the abox writer (see 'entityrdfizer'). 
                  User is responsible to provide the correct entry,
                  the correctness may also be checked by the tboxtools module.

                  #(optional) the prefix for TBox, where a class derived from this class is defined.
                  ###### If not specified OntoCrystal is used.
                  #If not specified, the entity will not have any prefix 
                  #      (i.e. will use the global tbox prefix from abox csv file)

      aPrefix   - (optional) the prefix for ABox, where this entiry should be stored.
                  If not specified, the entity will not have any prefix 
                        (i.e. will use the global abox prefix from csv file)

      uuidDB    - The database of uuid(s) where this Measure is saved.
                  If not specified, a default filename will be used (not recommended).

      """
 
      if "" == itemName:
          self.itemName = "Unknown"
          logging.error( " Empty id in '" + self.itemName + "'," + \
                         " in class OntoMeasureWithUncertainty." ) 
      else:   
          self.itemName = omNameConvert( itemName )
          self.itemName = itemName
                 
      if "" == className:
          self.className = "MeasureWithUncertainty"
          logging.warning( " Empty class in '" + self.itemName + "'," + \
                           " using class MeasureWithUncertainty." ) 
      else:   
          self.className = omNameConvert( className )
          self.className = className

      if None == tPrefix:
          self.tPrefix = crystOntoPrefix
      elif "" == tPrefix:
          # TODO This check is redundant, can usee the else option:
          self.tPrefix = ""
          #logging.warning( " Empty tPrefix in '" + self.itemName + "'," + \
          #                 " using global TBox from csv file." ) 
      else:
          self.tPrefix = tPrefix

      if None == aPrefix:
          self.aPrefix = ""
          #logging.warning( " Empty aPrefix in '" + self.itemName + "'," + \
          #                 " using global ABox from csv file." ) 
      else:
          self.aPrefix = aPrefix

      if None == uuidDB:
          logging.error( " Empty uuidDB in '" + self.itemName + "'" + \
                         " in class OntoMeasureWithUncertainty. Using default." ) 
      elif isinstance( uuidDB, tools.UuidDB ):
          self.uuidDB = uuidDB
      else:   
          logging.error( "Wrong type of uuidDB, expected 'UuidDB', got '" + str(type(uuid)) + "'." )
          pass

      self.value = None
      self.error = None
      self.unit  = None

      pass # OntoMeasureWithUncertainty.__init__()


    def setValue( self, value, unit = "", error = "" ):
        if "" == value:
            logging.info( " Empty value in '" + self.itemName + "' " ) 
        else:
            self.value = value

        if "" == error:
            logging.info( " Empty error in '" + self.itemName + "' " ) 
        else:
            self.error = error

        if "" == unit:
            logging.info( " Empty unit in '" + self.itemName + "' " ) 
        else:
            self.unit = unit

        pass # OntoMeasureWithUncertainty.setValue()

    #def getValue( self ):
    #  return self.value

    def getCsvArr( self, subject, predicate ):
        """
        subject   - Is the full hame of instance of class, 
                    which contains this entity of MeasureWithUncertainty class.
        predicate - Is the Object Property linking the Subject and 
                    the current entity of MeasureWithUncertainty." 

        # These property values are not guaranteed:
        if subject.find( "CrystalInformation" ) < 0:
          logging.warning( " Subject in arrUnitCell() is '" + subject + "'," +
                           " expecting the name to contain '" + "CrystalInfromation" + "'." ) 

        """
        if is_http( predicate ):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning( " Predicate in OntoMeasure.getCsvArr() is '" + \
                             predicate + "'," + " but expecting it to have '" + \
                             "has" + "'." ) 

        output = []

      #output += omInitUnit( "om:cubicAngstrom" )

      # FIXME ??? The tbox prefix should be OntoCrystal if the item is defined in OntoCrystal
      #           And can be overwritten for types defined in other ontologies.
      #           But this means I have to save somewhere a full list of classes defined in OntoCrystal.
      #
      #myClass = "MeasureWithUncertainty"
      #self.uuid = tools.getUUID( self.uuidDB, self.className, self.itemName ) #self.value["name"] )
        self.uuid,_ = self.uuidDB.addUUID( self.tPrefix + self.className, 
                                           self.aPrefix + self.itemName )
        output.append( [ self.uuid, "Instance", self.tPrefix + self.className, "", "", "" ] )
        output.append( [   subject, "Instance", self.uuid, predicate, "", "" ] )

        if self.value != None:
            output.append( [ omOntoPrefix + "hasNumericalValue", "Data Property", 
                             self.uuid, "", self.value, "xsd:decimal" ] )

        if self.error != None:
            output.append( [ crystOntoPrefix + "hasUncertaintyValue", "Data Property", 
                             self.uuid, "", self.error, "xsd:decimal" ] )

        if self.unit != None:
            output += omSetUnit( self.uuid, self.unit )

        #logging.error( " arrValue() is not implemented yet" )

        return output
        pass # OntoMeasureWithUncertainty.getCsvArr()

    pass # OntoMeasureWithUncertainty

class OntoVector:
  """
    itemName  - (required) name of the new variable.

    className - (optional) the class name. If not specified, Vector is used.

    tPrefix   - TODO Same as OntoMeasureWithUncertainty.

                (optional) the prefix for TBox, where a class derived from this class is defined.
                ###### If not specified OntoCrystal is used.
                If not specified, the entity will not have any prefix 
                      (i.e. will use the global tbox prefix from csv file)

    aPrefix   - TODO Same as OntoMeasureWithUncertainty.
                (optional) the prefix for ABox, where this entiry should be stored.
                If not specified, the entity will not have any prefix 
                      (i.e. will use the global abox prefix from csv file)

    uuidDB    - The database of uuids where all entities are saved.
    
    uuid4     - the unique UUID4 code of the entity of entity of this class.
     
    uuid      - the unique name of the entity of this class.
                Consists of the name and the uuid4: itemName_UUID4. 

    compList  - Components as a list (for components generated form array) 

    compDict  - Components as a dictionary (for components with label)


  """
  __slots__ = [ "uuidDB", "uuid", "uuid4", "value", "itemName", "className", 
                "tPrefix", "aPrefix",
                "ontoPrefix", "uuid_error", # <- old options, need to remove
                "unit", "vectorLabel",
                "compList", "compErrList", "compUnitList",
                "compDict" #, "compErrDict"
              ]
  def __init__( self, className, itemName, 
                uuidDB  = None, 
                tPrefix = None,     aPrefix = None, 
                unit    = None, vectorLabel = None
                #prefix = "", 
                #myUnit = "",  
                #myLabel = "" 
                ):
        """
        uuidDB    - Either a path to new/existing uuidDataBase (as string),
                    or an existing uuidDB (as dict or UUID class)
                    If not specified (or None), a default filename 'defailt-uuid.csv' will be used,
                    or a new file will be created with this name (not recommended).
        unit      - a unit with "om:" prefix.

        """

        if "" == itemName:
            logging.error( " In OntoVector the entity name is not specified." + \
                           " Critical error!")
            #self.value["name"] = "Unknown"
            return
        else:
            self.value = dict()
            self.value["name"] = itemName

            self.itemName = itemName


        if "" == className:
            logging.warning( " In OntoVector the class name is not specified" + \
                             " for '" + self.itemName + "'." + \
                             " Using default 'Vector'." )
            #self.value["class"] = "Vector"
            self.className = "Vector"
        else:
            #self.value["class"] = className
            self.className = className


        """
    if None == uuidDB:
      logging.warning( " In OntoVector uuidDB is not specified" + \
                       " for '" + self.itemName + "'."  )
      1/0
      " to create a new uuid here"
    elif isinstance( uuidDB, str ):
      logging.warning( " In OntoVector uuidDB is not specified" + \
                       " for '" + self.itemName + "'."  )
      1/0
      " to create a new uuid here"
        """
        if isinstance( uuidDB, tools.UuidDB ):
            self.uuidDB = uuidDB

        else:
            """
            logging.error( " Invalid value or type of uuidDB in OntoVector." + \
                           " Extected class UuidDB, but got '" + \
                           str(type(uuidDB)) + "'." )
            self.uuidDB = uuidDB
            """

            self.uuidDB = tools.UuidDB( uuidDB )
            logging.warning( " Creating a new UuidDB database, in file '" + \
                             self.uuidDB.dbFilename + "'." )

        if "" == unit or None == unit:
            logging.warning( " In OntoVector unit is not specified" +
                             " for '" + self.itemName + "'." )
            self.unit = None
          #self.value["unit"] = "Unknown"
        else:
            #self.value["unit"] = omNameConvert( unit )
            #self.value["unit"] = unit 
            self.unit = unit

        if None == vectorLabel:
            #self.value["label"] = "Unknown"
            if "UnitCellLatticeVector" == self.className:
                logging.error( " In OntoVector label is not specified" +
                               " for '" + self.itemName + "'." )
            self.vectorLabel = vectorLabel
            #self.vectorLabel = None
        else:
            #self.value["label"] = vectorLabel
            self.vectorLabel = vectorLabel
            if self.className in [ "MeasureVector", "MillerIndices" ]:
                logging.error( " In OntoVector class '" + self.className + "'" + \
                               " should not have vectorLabel, but given '" + \
                               self.vectorLabel + "'." )
            pass

        if None == tPrefix:
            self.tPrefix = crystOntoPrefix
            #logging.warning( " Empty tPrefix in '" + self.itemName + "'," + \
            #                 " using global TBox from csv file." ) 
        else:
            self.tPrefix = tPrefix

        if None == aPrefix:
            self.aPrefix = ""
            #logging.warning( " Empty aPrefix in '" + self.itemName + "'," + \
            #                 " using global ABox from csv file." ) 
        else:
            self.aPrefix = aPrefix

        # The vector class is defined in the OntoCrystal ontology:
        self.ontoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
        #if "" == prefix:
        #  logging.error( " In OntoVector ontology prefix is not specified" +
        #                 " for '" + self.itemName + "'." )
        #else:
        #  self.ontoPrefix = prefix

        self.value["comp"] = list()


        #self.value["list"] = dict()
        self.compList      = []
        self.compErrList   = []
        self.compUnitList  = []

        self.compDict      = {}
        #self.compErrDict   = {}

        self.uuid4      = None
        self.uuid_error = None      # This is used to avoid double creation of the Uncertainty Vector
        #self.error = dict()

        pass # OntoVector.__init__()

  def addComponentList( self, valList, unit = None, errList = None ):
        """
        The size of errList must be the same as valList,
        The errList may contain 'None' values for some entries.
        In this case the output will not have an uncertainty.
        """
        #logging.error( " in CsvMaker addComponentList() is not implemented" )

        if [] != self.compList:
            logging.warning( " Over-writing existing values in vector '" + \
                             self.itemName + "'." )
            print( "   >", self.compList )

        if [] != self.compErrList:
            logging.warning( " Over-writing existing errors in vector '" + \
                             self.itemName + "'." )

        if [] != self.compUnitList:
            logging.warning( " Over-writing existing units in vector '" + \
                             self.itemName + "'." )

        #-----------------------------------
        if isinstance( valList, list ):
            self.compList = list( valList )
        else:
            logging.error( " In OntoVector.addComponentList() expect a list," + \
                           " got '" + str(type(valList)) + "'." )
            self.compList = None

        #for il, el in enumerate(valList):
        #  self.value["list"] = None
        #  pass

        #-----------------------------------
        if None == errList:
            # Do nothing, no error list
            self.compErrList = None

        elif isinstance( errList, list ):
            if len(valList) != len(errList):
                logging.error( " In OntoVector.addComponentList() the size of err " + \
                     str(len(errList)) + " is not equal to size of val " + \
                     str(len(valList)) + ", skipping error list." )
            else:
                self.compErrList = list( errList )

        else:
            logging.error( " In OntoVector.addComponentList() expect an error list," + \
                           " got '" + str(type(errList)) + "'." )
            self.compErrList = None

        #-----------------------------------
        if "" == unit or None == unit:
            logging.info( " No unit in vector component list" )
            self.compUnitList = None
            pass
        elif isinstance( unit, list ):
            # Individual unit for each component, as a list
            if len(valList) != len(unit):
                logging.error( " In OntoVector.addComponentList() the size of unit " + \
                     str(len(unit)) + " is not equal to size of val " + \
                     str(len(valList)) + ", skipping unit list." )
            self.compUnitList = list(unit)
            pass
        elif isinstance( unit, str ):
            if unit.startswith( "om:" ):
                #logging.error( " Not implemented unit in vector list" )
                self.compUnitList = [ unit ] * len(valList)
            else:
                logging.error( " In OntoVector.addComponentList() unknown unit value '" + \
                               unit + "', expecting prefix om:" )
                self.compUnitList = None
            pass
        else:
            logging.error( " Unknown case for unit '" + str(unit) + "'." )
            # Do nothing. Need a warning?
            pass

        pass # OntoVector.addComponentList()

  def addComponent( self, label, value = "", unit = "", error = "" ):

        errCount = 0

        if not isinstance( self.compDict, dict ):
            self.compDict = {}
        #if None == self.compErrDict and error != "":
        #    self.compErrDict = {}

        if isinstance( label, str ):
            if "" == label:
                logging.error( " Not specified label for vector component in '" + \
                               self.itemName + "'. 22" )
                errCount += 1

        if isinstance( value, str ):
            if "" == value:
                logging.error( " Not specified value for vector component in '" + \
                               self.itemName + "'." )
                errCount += 1
        else:
            logging.info( "value = '" + str( value ) + "' is not a string in '" + \
                           self.itemName + "'." )
            pass

        if label in self.compDict:
            logging.error( "Over-writing an existing component with label '" + \
                           label + "' for Vector '" + self.itemName + "'." )

        self.compDict[label] = {}
        if "" != label:
            self.compDict[label]["label"] = str(label)
        if "" != value:
            self.compDict[label]["value"] = str(value)
        if "" != error:
            self.compDict[label]["error"] = str(error)
        if "" != unit:
            self.compDict[label]["unit" ] = str(unit)
    
        #if error != "":
        #  self.compErrDict[label] = {}
        #  self.compErrDict[label]["value"] = value
        #  if unit != "":
        #    self.compErrDict[label]["unit"]  = unit

        return errCount
        pass # OntoVector.addComponent()

  def addComponentOld( self, label, value = "", unit = "", error = "" ):
    logging.info( "Starting addComponent '" + self.itemName + "' label: '" + label + "'" )
    """
    Adding a component to a vector.
    TODO: how to create a vector made of multiple values as a list?
          probably it should be a separate function addComponentList( self, list, unit = "", error = "" )

    label - required input argument. It defines the component subscript.
    value - required input argument. The value of this component.
    unit  - Optional. An instance defined in ontology OM-2. 
            Accepted values either full path, or only the instance name.
            The OM-2 prefix will be appended in getCsvArr().
    error - Optional. The component's error-bar (uncertainty). It has the same unit.
    """
    errCount = 0
    if isinstance( value, str ):
        if "" == value:
            logging.error( " Not specified value for vector component in '" + \
                           self.itemName + "'." )
            errCount += 1
    else:
        logging.error( "value = '" + str( value ) + "' is not a string in '" + \
                       self.itemName + "'." )

    if isinstance( label, str ):
        if "" == label:
            logging.error( " Not specified label for vector component in '" + \
                           self.itemName + "'. 33" )
            errCount += 1

        keys = [ x["label"] for x in self.value["comp"] ]
        #keys = list(self.value["comp"].keys())
        if label in keys:
            logging.warning( " Repeatedly adding (overwriting) a component '" + \
                             label + "' in vector '" + self.itemName + "'." )
            logging.warning( "Components-2 of vector =" + str(self.value["comp"]) + \
                             " new value = '" + value + "'."  )
            #errCount += 1 # No need to count as error. It may be intensional.

    else:
        logging.error( "label = '" + str( label ) + "' is not a string in '" + \
                       self.itemName + "'." )

    if 0 == errCount:
        newComp = dict()
        newComp["label"] = label
        newComp["value"] = value
        if "" == unit:
            logging.info( " Not specified unit for vector component in '" + \
                          self.itemName + "'. 44" )
            pass
        else:
            newComp["unit"]  = unit

        if "" != error:
            newComp["error"] = error
            logging.info( " Components of vector = " + str(self.value["comp"]) + \
                             " value = '" + str(value) + "'."  )
            if "" != unit:
                # Generally speaking the error bars may have different units, 
                # though such situation looks strange:
                #print( " Assigned unit to", label )
                newComp["errorunit"]  = unit
                pass
       
        self.value["comp"].append( newComp )
        
        """
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
            #print( " Assigned unit to", label )
            self.value["comp"][label]["errorunit"]  = unit
            pass
        """    
    return errCount
    pass # OntoVector.addComponent()

  def getCsvArr( self, subject, predicate ):
    #print( "================" )
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
    if is_http( predicate ):
        short = predicate.split("/")[-1]
    else:
        short = predicate

    if not short.startswith("has"):
        logging.warning( " Predicate in OntoVector.getCsvArr() is '" + \
                         predicate + "'," + " but expecting it to have '" + \
                         "has" + "'." ) 

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
    #logging.error( " getCsvArr() is not implemented yet" )
    # FIXME TODO

    self.uuid,self.uuid4 = self.uuidDB.addUUID( self.tPrefix + self.className, \
                                                self.aPrefix + self.itemName )

    #self.uuid,self.uuid4 = self.uuidDB.addUUID( 
    #                       self.tPrefix + self.value["class"], 
    #                       self.aPrefix + self.value["name" ] )
    #self.uuid = tools.getUUID( self.uuidDB, self.tPrefix + self.value["class"], self.aPrefix + self.value["name"] )
    output.append( [ self.uuid, "Instance", 
                     self.tPrefix + self.className, "", "", "" ] )
                     #self.tPrefix + self.value["class"], "", "", "" ] )
    output.append( [   subject, "Instance", self.uuid, predicate, "", "" ] )

    # Global unit of the vector:
    if None != self.unit:
        output += omSetUnit( self.uuid, self.unit )

    if None != self.vectorLabel:
        output.append( [ self.ontoPrefix + "hasVectorLabel", "Data Property", 
                         self.uuid, "", self.vectorLabel, "xsd:string" ] )

    if None != self.compList:
        output += self._getCsvValueList( )

    # Redundant now:
    #if None != self.compErrList:
    #  output += self._getCsvErrorList( )
    #if "unit" in self.value.keys():
      #output += omSetUnit( self.uuid, self.value["unit"] )

    if None != self.compDict:
        output += self._getCsvValueDict()

    #if None != self.compErrDict:
      #output += self._getCsvErrorDict()

    return output

    '''
    # Verification of the class and its components:
    if self.value["class"] in [ "Vector" ]:
      # Classes derived from the basic 'Vector':
      if "error" in keys:
        logging.error( " Vector class '" + self.value["class"] + "' has an error value: " + 
                       self.value["error"] + ". Use class 'VectorWithUncertainty' " + 
                       "instead or a class derived from it." )
      pass
       
    elif self.value["class"] in [ "UnitCellAngles", "UnitCellLengths", 
                                  "UnitCellAngles", "PositionVector" ]:
      # Classes derived from 'VectorWithUncertainty':

      self.value["error"] = dict() # the error bars a.k.a. uncertainty. Optional parameter.
      pass

    elif self.value["class"] in [ "UnitCellLatticeVector" ]:
      # Classes derived from 'UnitCellLatticeVector':

      self.value["error"] = dict() # the error bars a.k.a. uncertainty. Optional parameter.
                    
      if "label" in keys:
        output.append( [ self.ontoPrefix + "hasVectorLabel", "Data Property", 
                         self.uuid, "", self.value["label"], "xsd:string" ] )

      if "index" in keys:
        output.append( [ self.ontoPrefix + "hasComponentIndex", "Data Property", 
                         self.uuid, "", self.value["index"], "xsd:integer" ] )

      #else:
      #  logging.warning( " Label is not defined for '" + self.value["name"] + 
      #                   "' of class '" + myClass + "'." )
      pass

    elif "MillerIndices" == self.value["class"]:
      pass
    #elif "Vector" == self.value["class"]: 
    #   REDUNDANT, this cannot happen
    #  pass
      """
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
      logging.error( " Unknown vector class '" + self.value["class"] + "' "
                      "for vector '" + self.value["name"] + "'." )
      pass

    if   isinstance( self.value["comp"], dict):
      print( ">> Old branch of component in getCsvArr() as dict" )

      comp_keys = self.value["comp"].keys()  # comp_keys means 'KEYS of the COMPonents'.
      for ck in comp_keys:
        #print( ck )

        uuid_comp,_ = self.uuidDB.addUUID( "VectorComponent", self.value["name"] + "_comp_" + ck )

        #uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", self.value["name"] + "_comp_" + ck  )
        output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

        output.append( [ self.uuid, "Instance", uuid_comp,  
                         self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasComponentLabel", "Data Property", 
                         uuid_comp, "", ck, "string" ] )

        output.append( [ self.ontoPrefix + "hasComponentValue", "Data Property", 
                         uuid_comp, "", self.value["comp"][ck]["value"], "rdfs:Literal" ] )
 
        if "unit" in list(self.value["comp"][ck].keys()): # and "Unknown" != self.value["comp"][ck]["unit"]:
          output += omSetUnit( uuid_comp, self.value["comp"]["unit"] )

          #unit = self.value["comp"][ck]["unit"]
          #if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
          #  logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
          #                   self.value["name"] + "'. " + 
          #                   "Expecting an instance of OM-2 ontology class Unit, but got '" +
          #                   self.value["comp"][ck]["unit"] )

          #output.append( [ uuid_comp, "Instance", unit,
          #               "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
          #                "", "" ] )
          #print( unit )
          #if "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" == unit.strip():
          #  output.append( [ "rdfs:label", "Data Property",
          #                   unit, "", "angstrom", "xsd:string" ] )
            #omSetUnit( uuid_comp, "angstrom" ):
            
 
        if "error" in list(self.value["comp"][ck].keys()):
          if None == self.uuid_error:
            logging.info( " Creating a new instance for uuid_error for vector '" + self.value["name"] + "' " )

            self.uuid_error,_ = self.uuidDB.addUUID(  self.ontoPrefix + "Vector", self.value["name"] + "_error" )
            #self.uuid_error = tools.getUUID( self.uuidDB, self.ontoPrefix + "Vector", self.value["name"] + "_error" )
            output.append( [ self.uuid_error, "Instance", self.ontoPrefix + "Vector", "", "", "" ] )
 
            output.append( [ self.uuid, "Instance", self.uuid_error,  
                             self.ontoPrefix + "hasUncertaintyVector", "", "" ] )
 
          uuid_comp,_ = self.uuidDB.addUUID( "VectorComponent", self.value["name"] + "_err_" + ck )
         # uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", self.value["name"] + "_err_" + ck  )
          output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

          output.append( [ self.uuid_error, "Instance", uuid_comp,  
                           self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

          output.append( [ self.ontoPrefix + "hasComponentLabel", "Data Property", 
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
            output += omSetUnit( uuid_comp, unit )
            #if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
            #  logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
            #                 self.value["name"] + "'. " + 
            #                 "Expecting an instance of OM-2 ontology class Unit." )

            #output.append( [ uuid_comp, "Instance", unit,
            #               "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
            #                "", "" ] )

            #print( unit )
            #if "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" == unit.strip():
            #  output.append( [ "rdfs:label", "Data Property",
            #                 unit, "", "angstrom", "xsd:string" ] )

    elif isinstance( self.value["comp"], list):
      #print( ">>>>>>>>>>>>>>>>", " starting getCsvArr()", self.value["name"] )
      #output.append( [ "starting getCsvArr() list", "",""] )
      #comp_keys = self.value["comp"].keys()  # comp_keys means 'KEYS of the COMPonents'.
      #for ck in comp_keys:
      for ck in self.value["comp"]:
        if   "label" in ck.keys():
          compStr = ck["label"]
        elif "index" in ck.keys():
          compStr = str(ck["index"])
        else:
          logging.error( " Component '" + str(ck) + "' does not have neither" +
                         " label nor index in '" + self.value["name"] + "'." )

        uuid_comp,_ = self.uuidDB.addUUID( "VectorComponent", self.value["name"] + "_comp_" + compStr )
        #uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", 
        #                           self.value["name"] + "_comp_" + compStr )

        output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

        output.append( [ self.uuid, "Instance", uuid_comp,  
                         self.ontoPrefix + "hasVectorComponent", "", "" ] )

        if "label" in ck.keys():
          output.append( [ self.ontoPrefix + "hasComponentLabel", "Data Property", 
                           uuid_comp, "", ck["label"], "xsd:string" ] )
        if "index" in ck.keys():
          output.append( [ self.ontoPrefix + "hasComponentIndex", "Data Property", 
                           uuid_comp, "", ck["index"], "xsd:integer" ] )

        output.append( [ self.ontoPrefix + "hasComponentValue", "Data Property", 
                         uuid_comp, "", ck["value"], "rdfs:Literal" ] )
                         #uuid_comp, "", self.value["comp"][ck]["value"], "rdfs:Literal" ] )
 
        #if "unit" in list(self.value["comp"][ck].keys()): # and "Unknown" != self.value["comp"][ck]["unit"]:
        if "unit" in ck.keys(): # and "Unknown" != self.value["comp"][ck]["unit"]:
          unit = ck["unit"]
          #if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
          #  logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
          #                   self.value["name"] + "'. " + 
          #                   "Expecting an instance of OM-2 ontology class Unit, but got '" +
          #                   ck["unit"] )

          #output.append( [ uuid_comp, "Instance", unit,
          #               "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
          #                "", "" ] )
          output += omSetUnit( uuid_comp, unit )
          #print( unit )
          # FIXME
          #if "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" == unit.strip():
          #  output.append( [ "rdfs:label", "Data Property",
          #                   unit, "", "angstrom", "xsd:string" ] )
     
          #if "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" == unit.strip():
          #    output.append( [ "rdfs:label", "Data Property",
          #                   unit, "", "degree", "xsd:string" ] )
      
          #if "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom" == unit.strip():
          #    output.append( [ "rdfs:label", "Data Property",
          #                   unit, "", "reciprocalAngstrom", "xsd:string" ] )
 
          #if "http://www.ontology-of-units-of-measure.org/resource/om-2/cubicAngstrom" == unit.strip():
          #    output.append( [ "rdfs:label", "Data Property",
          #                   unit, "", "cubicAngstrom", "xsd:string" ] )
 
          #if "http://www.ontology-of-units-of-measure.org/resource/om-2/dimensionOne" == unit.strip():
          #    output.append( [ "rdfs:label", "Data Property",
          #                   unit, "", "dimensionOne", "xsd:string" ] )       
 
        #if "error" in list(self.value["comp"][ck].keys()):
        if "error" in ck.keys():
          if None == self.uuid_error:
            logging.info( " Creating a new instance for uuid_error for vector '" + self.value["name"] + "' " )

            self.uuid_error,_ = self.uuidDB.addUUID( "VectorComponent", self.value["name"] + "_error" )
            #self.uuid_error = tools.getUUID( self.uuidDB, self.ontoPrefix + "Vector", self.value["name"] + "_error" )
            output.append( [ self.uuid_error, "Instance", self.ontoPrefix + "Vector", "", "", "" ] )
 
            output.append( [ self.uuid, "Instance", self.uuid_error,  
                             self.ontoPrefix + "hasUncertaintyVector", "", "" ] )
 
          if   "label" in ck.keys():
            compStr = ck["label"]
          elif "index" in ck.keys():
            compStr = str(ck["index"])
          else:
            logging.error( " Component '" + str(ck) + "' does not have neither" +
                           " label nor index in '" + self.value["name"] + "'." )

          uuid_comp,_ = self.uuidDB.addUUID( "VectorComponent", self.value["name"] + "_error_" + compStr )
          #uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", self.value["name"] + "_error_" + compStr  )
          output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

          output.append( [ self.uuid_error, "Instance", uuid_comp,  
                           self.ontoPrefix + "hasVectorComponent", "", "" ] )

          if "label" in ck.keys():
            output.append( [ self.ontoPrefix + "hasComponentLabel", "Data Property", 
                             uuid_comp, "", ck["label"], "xsd:string" ] )
          if "index" in ck.keys():
            output.append( [ self.ontoPrefix + "hasComponentIndex", "Data Property", 
                             uuid_comp, "", ck["index"], "xsd:integer" ] )

          output.append( [ self.ontoPrefix + "hasComponentValue", "Data Property", 
                           uuid_comp, "", ck["error"], "rdfs:Literal" ] )
 
          #print( "eeewwwwwwwwweeeeeeeeeeee" )
          # FIXME here there is a mess between the units.
          if "errorunit" in ck.keys():
            unit = ck["errorunit"]
          elif "unit" in ck.keys():
            unit = ck["unit"]
          else:
            unit = ""

          if "" != unit: 
            output += omSetUnit( uuid_comp, unit )
            #if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
            #  logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
            #                 self.value["name"] + "'. " + 
            #                 "Expecting an instance of OM-2 ontology class Unit." )

            #output.append( [ uuid_comp, "Instance", unit,
            #               "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
            #                "", "" ] )

            #print( unit )
            #if "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" == unit.strip():
            #  output.append( [ "rdfs:label", "Data Property",
            #                 unit, "", "angstrom", "xsd:string" ] )
     
            #if "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" == unit.strip():
            #  output.append( [ "rdfs:label", "Data Property",
            #                 unit, "", "degree", "xsd:string" ] )
      
            #if "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom" == unit.strip():
            #  output.append( [ "rdfs:label", "Data Property",
            #                 unit, "", "reciprocalAngstrom", "xsd:string" ] )
 
            #if "http://www.ontology-of-units-of-measure.org/resource/om-2/cubicAngstrom" == unit.strip():
            #  output.append( [ "rdfs:label", "Data Property",
            #                 unit, "", "cubicAngstrom", "xsd:string" ] )
 
            #if "http://www.ontology-of-units-of-measure.org/resource/om-2/dimensionOne" == unit.strip():
            #  output.append( [ "rdfs:label", "Data Property",
            #                 unit, "", "dimensionOne", "xsd:string" ] )

    else:
      logging.error( " Unknown type of vector['comp'] = '" + str(type(self.value["comp"])) + 
                     "' in vector '" + self.value["name"] + "' reter." )

    if "unit" in keys:
#"http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom"
      unit = self.value["unit"]
      output += omSetUnit( self.uuid, unit )
      #if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
      #  logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
      #                   self.value["name"] + "'. " + 
      #                   "Expecting an instance of OM-2 ontology class Unit, but got '" +
      #                   self.value["unit"] + "'" 
      #                   )

      #output.append( [ self.uuid, "Instance", unit,
      #               "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
      #                "", "" ] )

      #print( unit )
      #if "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" == unit.strip():
      #        output.append( [ "rdfs:label", "Data Property",
      #                       unit, "", "angstrom", "xsd:string" ] )
 
    #output.append( ["Ended getCsvArr()" , "", "" ] )
    #print( "==============================" )
    return output
    '''
    pass # OntoVector.getCsvArr()

  def _getCsvValueList( self ):
    output = []

    for iv,v in enumerate(self.compList):
      if None == v:
        logging.error( "Invalid value '" + str(v) + "' in '" + self.value["name"] + "'." )
        continue
      #output +=  

      uuid_comp,_ = self.uuidDB.addUUID( "VectorComponent", 
                                         self.value["name"] + "_comp_" + str(iv+1), 
                                         newUuid = self.uuid4 )

      output.append( [ self.aPrefix + uuid_comp, "Instance", 
                       self.ontoPrefix + "VectorComponent", "", "", "" ] )

      output.append( [ self.uuid, "Instance", self.aPrefix + uuid_comp,  
                       self.ontoPrefix + "hasVectorComponent", "", "" ] )

      if None != self.compUnitList:
          u = self.compUnitList[iv]
          if None != u:
              output += omSetUnit( self.aPrefix + uuid_comp, u )

              #output.append( [ self.ontoPrefix + "hasComponentUncertainty", "Data Property", 
              #                 self.aPrefix + uuid_comp, "", e, "rdfs:Literal" ] )

      output.append( [ self.ontoPrefix + "hasComponentIndex", "Data Property", 
                       self.aPrefix + uuid_comp, "", (iv + 1), "xsd:integer" ] )

      output.append( [ self.ontoPrefix + "hasComponentValue", "Data Property", 
                       self.aPrefix + uuid_comp, "", v, "rdfs:Literal" ] )

      if None != self.compErrList:
          e = self.compErrList[iv]
          if None != e:
              output.append( [ self.ontoPrefix + "hasComponentUncertainty", "Data Property", 
                               self.aPrefix + uuid_comp, "", e, "rdfs:Literal" ] )



      """
          if "label" in ck.keys():
          if "index" in ck.keys():
            output.append( [ self.ontoPrefix + "hasComponentIndex", "Data Property", 
                             uuid_comp, "", ck["index"], "xsd:integer" ] )

          output.append( [ self.ontoPrefix + "hasComponentValue", "Data Property", 
                           uuid_comp, "", ck["error"], "rdfs:Literal" ] )
      """ 

      pass

    return output
    pass # OntoVector._getCsvValueList()

  """
  def _getCsvErrorList( self ):
    output = []

    for iv,v in enumerate(self.compErrList):
      if None == v:
        logging.error( "Invalid value '" + str(v) + "' in '" + self.itemName + "'." )
        continue
      output  

      uuid_comp,_ = self.uuidDB.addUUID( "VectorComponent", 
                                         self.itemName + "_error_" + str(iv+1), 
                                         newUuid = self.uuid4 )

      output.append( [ self.aPrefix + uuid_comp, "Instance", 
                       self.ontoPrefix + "VectorComponent", "", "", "" ] )

      output.append( [ self.uuid, "Instance", self.aPrefix + uuid_comp,  
                       self.ontoPrefix + "hasVectorComponent", "", "" ] )

      output.append( [ self.ontoPrefix + "hasComponentIndex", "Data Property", 
                       self.aPrefix + uuid_comp, "", (iv + 1), "xsd:integer" ] )

      output.append( [ self.ontoPrefix + "hasComponentLabel", "Data Property", 
                       self.aPrefix + uuid_comp, "", v, "decimal" ] )

      pass

    return output
    pass # OntoVector._getCsvErrorList()
  """

  def _getCsvValueDict( self ):
    output = []

    for ik,k in enumerate(self.compDict.keys()):
        if None == k:
            logging.error( "Invalid value '" + str(k) + "' in '" + self.itemName + "'." )
            continue
      #output +=  

        uuid_comp,_ = self.uuidDB.addUUID( "VectorComponent", 
                                           self.itemName + "_comp_" + k,
                                           newUuid = self.uuid4 )

        output.append( [ self.aPrefix + uuid_comp, "Instance", 
                         crystOntoPrefix + "VectorComponent", "", "", "" ] )

        output.append( [ self.uuid, "Instance", self.aPrefix + uuid_comp,  
                         crystOntoPrefix + "hasVectorComponent", "", "" ] )

      #output.append( [ self.ontoPrefix + "hasComponentIndex", "Data Property", 
      #                 self.aPrefix + uuid_comp, "", (iv + 1), "xsd:integer" ] )

        if "unit" in self.compDict[k]:
            u = self.compDict[k]["unit"]
            if None != u:
                output += omSetUnit( self.aPrefix + uuid_comp, u )

        if "label" in self.compDict[k]:
            output.append( [ self.ontoPrefix + "hasComponentLabel", \
                             "Data Property", self.aPrefix + uuid_comp, \
                             "", self.compDict[k]["label"], "xsd:string" ] )

        if "value" in self.compDict[k]:
            output.append( [ self.ontoPrefix + "hasComponentValue", \
                             "Data Property", self.aPrefix + uuid_comp, \
                             "", self.compDict[k]["value"], "rdfs:Literal" ] )

        if "error" in self.compDict[k]:
            output.append( [ self.ontoPrefix + "hasComponentUncertainty", "Data Property", 
                             self.aPrefix + uuid_comp, "", self.compDict[k]["error"], "rdfs:Literal" ] )


      #output.append( [ self.ontoPrefix + "hasComponentLabel", "Data Property", 
      #                 self.aPrefix + uuid_comp, "", self.compDict[k], "xsd:decimal" ] )


    return output
    pass # OntoVector._getCsvValueDict()

  """
  def _getCsvErrorDict( self):
        output = []
        logging.error( " Not implemetned _getCsvErrDict" )

        return output
        pass # OntoVector._getCsvErrorDict()

  """

  pass # class OntoVector

class OntoMatrix:
    __slots__ = [ "uuidDB", "uuid", "myId", "unit",
                  "className", "itemName", 
                  "compList", "compDict"
                ]
    def __init__( self, className, itemName, uuidDB  = None, 
                  tPrefix = None,     aPrefix = None, 
                  unit    = None 
                ):


        """
        if is_http( predicate ):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning( " Predicate in OntoMatrix.getCsvArr() is '" + \
                             predicate + "'," + " but expecting it to have '" + \
                             "has" + "'." ) 
        """
        if "" == itemName:
            logging.error( " In OntoVector the entity name is not specified." + \
                           " Critical error!")
            return
        else:
            self.itemName = itemName


            if "" == className:
                logging.warning( " In OntoMatrix the class name is not" + \
                                 " specified for '" + self.itemName + "'." + \
                                 " Using default 'Matrix'." )
                self.className = "MeasureMatrix"
            else:
                self.className = className

        if isinstance( uuidDB, tools.UuidDB ):
            self.uuidDB = uuidDB
        else:
            self.uuidDB = tools.UuidDB( uuidDB )
            logging.warning( " Creating a new UuidDB database, in file '" + \
                             self.uuidDB.dbFilename + "'." )

        if "" == unit or None == unit:
            logging.warning( " In OntoMatrix unit is not specified" + \
                             " for '" + self.itemName + "'." )
            self.unit = None
        else:
            self.unit = unit

        self.compList = []
        self.compDict = {}

        pass # OntoMatrix.__init__()

    def addComponent( self, label, value = None, error = None, unit = None ):

        if not isinstance( self.compDict, dict):
            self.compDict = {}

        if isinstance( label, str ):
            if "" == label:
                logging.error( " Not specified label for vector component" + \
                               " in '" + self.itemName + "'. 22" )
                errCount += 1
        else:
            logging.warning( "label = '" + str( label ) + "' is not a string" + \
                             " in '" + self.itemName + "'." )
            pass

        if label in self.compDict:
            logging.error( "Over-writing an existing component with label '" + \
                           label + "' for Matrix '" + self.itemName + "'." )

        self.compDict[label] = {}
        if None != value:
            self.compDict[label]["value"] = value

        if None != error:
            self.compDict[label]["error"] = error

        if None != unit:
            self.compDict[label]["unit"] = unit

        pass # OntoMatrix.addComponent

    def addComponentList( self, label, valList = None, errList = None, unit = None ):
        logging.error( " Not implemented addComponentList() e3e3e3." )

        pass # OntoMatrix.addComponentList

    def getCsvArr( self, subject, predicate ):
        if is_http( predicate ):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning( " Predicate in OntoMatrix.getCsvArr() is '" + \
                             predicate + "'," + " but expecting it" + \
                             " to have 'has'." ) 
        output = []

        logging.error( " Not implemented OntoMatrix.getCsvArr" )

    # Vector for Unit Cell length parameters (a,b,c):
        self.uuid,self.uuid4 = self.uuidDB.addUUID( 
                               self.tPrefix + self.value["class"], 
                               self.aPrefix + self.value["name" ] )
                               #self.uuid = tools.getUUID( self.uuidDB, self.tPrefix + self.value["class"], self.aPrefix + self.value["name"] )
        output.append( [ self.uuid, "Instance", 
                         self.tPrefix + self.className, "", "", "" ] )
                         #self.tPrefix + self.value["class"], "", "", "" ] )
        output.append( [   subject, "Instance", self.uuid, predicate, "", "" ] )

        # Global unit of the vector:
        if None != self.unit:
            output += omSetUnit( self.uuid, self.unit )

        if None != self.vectorLabel:
            output.append( [ self.ontoPrefix + "hasVectorLabel", "Data Property", 
            self.uuid, "", self.vectorLabel, "xsd:string" ] )

        if None != self.compList:
            output += self._getCsvValueList( )


        if None != self.compDict:
            output += self._getCsvValueDict()

        return output
        pass # OntoMatrix.getCsvArr()

    """
    def _getCsvValueList( self ):
        output = []
        return output
        pass # OntoVector._getCsvValueList() 

    def _getCsvValueDict( self ):
        output = []   

        return output
        pass # OntoVector._getCsvValueDict()
    """

    pass # class OntoMatrix


class OntoPlot:
    """
    uuid - is the unique name of the entity of this class.
    """
    __slots__ = [ "uuidDB", "plotId", "uuid", #"", 
                  "curveList", #"" 
                ]
    def __init__( self, uuidDB = None, plotId = None ):
        self.uuidDB = uuidDB
        self.plotId = plotId

  # prefix = "", uuidDB = None, myClass = "", 
  #                    myName = "", myUnit = ""  ):
        self.curveList = []

        pass # OntoPlot.__init__()

    def addCurves( self, listAbscissa, listOrdinate ):
        if not isinstance( listAbscissa, list ):
            logging.error( " In addCurve listAbscissa must be a list." )
            return
        if not isinstance( listOrdinate, list ):
            logging.error( " In addCurve listOrdinate must be a list." )
            return

        for a in listAbscissa:
            if not isinstance( a, list ):
                logging.error( " In addCurve listAbscissa must be a list of lists." )
                return

            for o in listOrdinate:
                if not isinstance( o, list ):
                    logging.error( " In addCurve listOrdinate must be a list of lists." )
                    return

                self.curveList.append( [a,o] )

        pass # OntoPlot.addCurves()

    def getCsvArr( self, subject, predicate ):
        """
        subject   - Is the full hame of instance of class, 
                    which contains this plot class.
        predicate - Is the Object Property linking the Subject and the current Plot.
                    Typically is should be contain "has".
        """
        if subject.find("XRDSpectrum") < 0:
            logging.warning( " OntoPlot expects a subject 'XRDSpectrum', " + \
                             "but got '" + subject + "'." )
        if predicate.find("hasPlot") < 0:
            logging.warning( " OntoPlot expects a predicate 'hasPlot', " + \
                             "but got '" + predicate + "'." )

        output = []

        if None == self.plotId:
            logging.warning( " Neither plotId, nor '' is specified. " + 
                             "I use a random number as label." )
            plotLabel = tools.getUUID_random( "" )
        else:
            plotLabel = self.cifLabel
    #elif label != None:
    #  atomLabel = str(label)

        self.uuid,_ = self.uuidDB.addUUID( "PlotXY", "PlotXY_" + plotLabel )
        #self.uuid = tools.getUUID( self.uuidDB, "PlotXY", "PlotXY_" + plotLabel )
        output.append( [ self.uuid, "Instance", "PlotXY", "", "", "" ] )
 
        output.append( [ subject, "Instance", self.uuid, predicate, "", "" ] )

        for ic,curve in enumerate(self.curveList):
            plotX = OntoVector( uuidDB = self.uuidDB, \
                                myClass = "AbscissaData", \
                                myName  = plotLabel + "_Plot" + str(ic+1) + "X", \
                                myUnit  = "om:dimensionOne" )

            plotX.addComponentList( curve[0], unit = "om:degree" )

            output += plotX.getArrVector( uuid_plot, self.crystOntoPrefix + "hasAbscissaData" ) 
 
            plotY = OntoVector( uuidDB = self.uuidDB, \
                                myClass = "OrdinateData", \
                                myName  = plotLabel + "_Plot" + str(ic+1) + "Y",\
                                myUnit  = "om:dimensionOne" )

            plotY.addComponentList( curve[1], unit = "om:dimensionOne" )

            output += plotY.getArrVector( uuid_plot, self.crystOntoPrefix + "hasOrdinate" ) 
 
            output.append( [ plotX.uuid, "Instance", plotY.uuid, \
                             self.ontoPrefix + "isAbscissaOf", "", "" ] )

        return output
        pass # OntoPlot.getCsvArr()

    pass # class OntoPlot

class AtomInformation:
  """
  cifName  - is the name of the molecule, as it will be save in abox. 
             Withing the molecule an atom can be addressed by the label. 
  cifLabel - label of the atom within the compound (cifName).
             The unique id of the atom is cifName+_+cifLabel.

  """
  __slots__ = [ "uuidDB", "cifName", "cifLabel", 
                "frac", "cart", "element", "occupancy", 
                "crystOntoPrefix" ]
  def __init__(self, uuidDB, compound ):
    self.uuidDB    = uuidDB
    self.cifName   = compound

    self.frac      = None # save as array, converted to vector only before saving.
    self.cart      = None # save as array, converted to vector only before saving.
    self.element   = None # A string (1-2 letters), the atom name from Mendeleev table.
    self.occupancy = None # Optional.
    self.cifLabel  = None # This is directly from cif. Optional value.

    self.crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
    pass # AtomInformation.__init__()

  def setCoordFrac( self, x, y, z ):
    self.frac = [ x, y, z ]
    pass # AtomInformation.setCoordFrac()

  def setCoordCart( self, x, y, z ):
    self.cart = [ x, y, z ]
    pass # AtomInformation.setCoordFrac()

  def setProp( self, element = None, occupancy = None, label = None ):

    if element != None:
      if not isinstance( element, str):
        logging.warning( " atom element is not a string: '" + str(element) + "'." )
      self.element = element

    if occupancy != None:
      if not isinstance( occupancy, float) and not isinstance(occupancy, int):
        logging.warning( " atom occupancy is not a number: '" + str(occupancy) + "'." )
      self.occupancy = occupancy

    if label != None:
      if not isinstance( label, str):
        logging.warning( " atom label is not a string: '" + str(label) + "'." )
      self.cifLabel = str(label)

    pass # AtomInformation.setProp()

  def getArrAtom( self, subject, predicate, label = None ):
    #print( "================" )
    """
    subject   - Is the full hame of instance of class, 
                which contains this Vector class.
    predicate - Is the Object Property linking the Subject and the current UnitCell.
                Typically is should be contain "has".
    """
    if subject.find("AtomicStructure") < 0:
      logging.warning( " Atom expects a subject 'AtomicStrucure', " + \
                       "but got '" + subject + "'." )
    if predicate.find("hasAtomSite") < 0:
      logging.warning( " Atom expects a predicate 'hasAtomSite', " + \
                       "but got '" + predicate + "'." )

    output = []

    if self.cifLabel != None:
      atomLabel = self.cifLabel
    elif label != None:
      atomLabel = str(label)
    else:
      logging.warning( " Neither cifLabel, nor atom label is specified. " + 
                       "I use a random number as label." )
      atomLabel = tools.getUUID_random( "" )


    uuid_atom,_ = self.uuidDB.addUUID(  "AtomSite", "Atom_" + self.cifName + atomLabel )
    #uuid_atom = tools.getUUID( self.uuidDB, "AtomSite", "Atom_" + self.cifName + atomLabel )
    output.append( [ uuid_atom, "Instance", "AtomSite", "", "", "" ] )
 
    # Define relation between the class instances:
    output.append( [ subject, "Instance", uuid_atom, predicate, "", "" ] )

    ### Setting the available data:
    if self.cifLabel != None:
      output.append( [ self.crystOntoPrefix + "hasAtomSiteLabel", "Data Property", 
                             uuid_atom, "", self.cifLabel, "xsd:string" ] )
      
    if self.occupancy != None:
      output.append( [ self.crystOntoPrefix + "hasOccupancy", "Data Property", 
                             uuid_atom, "", self.occupancy, "xsd:decimal" ] )
      
    if self.element != None:
      # TODO add species
      #output.append( "" )
      pass

    if self.frac != None:
      atomPos = OntoVector( 
                            className = "PositionVector", 
                            itemName  = self.cifName + "_FracCoord_" + label, #+ cifName + label,
                            uuidDB    = self.uuidDB, 
                            #myName  = self.cifName + "_FracCoord_" + label, #+ cifName + label,
                            unit  = "om:dimensionOne" )
      #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )

      atomPos.addComponent( label = "x", value = str(self.frac[0]) ) #, error = error )
      atomPos.addComponent( label = "y", value = str(self.frac[1]) ) #, error = error )
      atomPos.addComponent( label = "z", value = str(self.frac[2]) ) #, error = error )
      output += atomPos.getCsvArr( uuid_atom, self.crystOntoPrefix + "hasFractionalPosition" ) 
      pass

    if self.cart != None:
      atomPos = OntoVector( 
                            className = "PositionVector", 
                            itemName  = self.cifName + "_CartCoord_" + label, #+ cifName + label,
                            uuidDB = self.uuidDB, 
                            #myName  = self.cifName + "_CartCoord_" + label, #+ cifName + label,
                            unit  = "om:angstrom" )
      #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )

      atomPos.addComponent( label = "x", value = str(self.cart[0]) ) #, error = error )
      atomPos.addComponent( label = "y", value = str(self.cart[1]) ) #, error = error )
      atomPos.addComponent( label = "z", value = str(self.cart[2]) ) #, error = error )
      output += atomPos.getCsvArr( uuid_atom, self.crystOntoPrefix + "hasCartesianPosition" ) 
 
      pass

    #uuid_cif_uc = tools.getUUID( self.uuidDB, "UnitCell", "UnitCell_" + zeoname )
    #output.append( [ uuid_cif_uc, "Instance", "UnitCell", "", "", "" ] )

    # Define relation between the class instances:
    #output.append( [ subject, "Instance", uuid_cif,  
    #output.append( [ uuid_zeolite, "Instance", uuid_cif,  
                     #self.crystOntoPrefix + "hasCrystalInformation", "", "" ] )

    #output.append( [ uuid_cif, "Instance", uuid_cif_uc,  
                     #predicate, "", "" ] )
                     #self.crystOntoPrefix + "hasUnitCell", "", "" ] )


   
    return output
    pass # AtomInformation.getArrAtom()

  pass # class AtomInformation
 
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

  TODO
  """

  __slots__ = [ "uuidDB", "algorithm", "cifPath", "cifName", # the unique id in owl file
                "struct", # temporary information
                "unitCellLengths", "unitCellRecipLengths", 
                "unitCellAngles",  "unitCellRecipAngles",
                "unitCellVectorA",      "unitCellVectorB",      "unitCellVectorC",
                "unitCellRecipVectorA", "unitCellRecipVectorB", "unitCellRecipVectorC",
                "listAtomRaw", "listAtomAll", "listAtomSymm",
                "unitCellVolume", 
                # other properties:
                "symmLatticeSystem", "symmITNumber", #"", "", 
                "cifStandard", "loopHeaders", 
              ] 
  def __init__( self, algType, uuidDB ):
        # Must be one of 'PyMatGen' or 'ValAndErr', depending how it was created:
        if   "PyMatGen"  == algType:
            self.algorithm  = algType
        elif "ValAndErr" == algType:
            self.algorithm  = algType
        else:
            logging.error( " Invalid algorithm type in CrystalInformation: '" + 
                           algType + "'. Must be one of: 'PyMatGen', 'ValAndErr'." )
            self.algorithm = None

        # The uuid database to generate unique uuid:
        if isinstance( uuidDB, tools.UuidDB ):
            self.uuidDB = uuidDB
        else:
            logging.error( " Invalid entry '" + str(uuidDB) + "', expecting a database." )
            self.uuidDB = None
        #if isinstance( uuidDB, dict ):
        #  logging.error( " Invalid entry '" + str(uuidDB) + "', expecting a database." )

        #self.cifStandard = None
        self.cifStandard = self.readStandardFile( "CIF_standard_2.4.5.txt" )

        self.cifName = None
        self.cifPath = None

        self.struct               = None # entity of PyMatGen Structure() class.

        self.unitCellLengths      = None
        self.unitCellAngles       = None
        self.unitCellRecipLengths = None
        self.unitCellRecipAngles  = None
        self.unitCellVolume       = None

        self.listAtomRaw  = None
        self.listAtomAll  = None
        self.listAtomSymm = None

        self.symmLatticeSystem = None
        self.symmITNumber      = None

        pass # CrystalInformation.__init__()

  def readStandardFile( self, path ):
        output = []
        if not os.path.isfile( path ):
            logging.error( "CIF standard file does not exist: '" + path + "'" )
            return []
        f = open( path, encoding="utf8" )
        for line in f:
            short = cleanString( line )
            pos = short.find( "_" )
            if 0 == pos:
                pos = short.find( "_[]" )
                if pos < 0:
                    output.append(short)
                else:
                    pass

        f.close()
        return output

        pass # CrystalInformation.readStandardFile()

  def loadData( self, cifPath, cifName ):
        """

        """
        if None == self.algorithm:
            logging.error( " Failed CrystalInformation.loadData(" + cifPath + \
                           ")," + " due to missing algorithm.")
            return

        self.cifName = cifName
        self.cifPath = cifPath

        where = " in '" + cifPath + "', algorithm '" + self.algorithm + "'"
        if None != self.unitCellLengths:
            logging.warning( " Overwriting 'unitCellLengths' " + where + "." )
        if None != self.unitCellAngles:
            logging.warning( " Overwriting 'unitCellAngles' " + where + "." )
        if None != self.unitCellRecipLengths:
            logging.warning( " Overwriting 'unitCellRecipLengths' " + where + "." )
        if None != self.unitCellRecipAngles:
            logging.warning( " Overwriting 'unitCellRecipAngles' " + where + "." )
        if None != self.unitCellVolume:
            logging.warning( " Overwriting 'unitCellVolume' " + where + "." )
        if None != self.listAtomRaw:
            logging.warning( " Overwriting 'listAtomRaw' " + where + "." )
        if None != self.listAtomAll:
            logging.warning( " Overwriting 'listAtomAll' " + where + "." )
        if None != self.listAtomSymm:
            logging.warning( " Overwriting 'listAtomSymm' " + where + "." )

        if "PyMatGen" == self.algorithm:
            self.loadPyMatGen( cifPath, cifName )

        elif "ValAndErr" == self.algorithm:
            self.loadValAndErr( cifPath, cifName )
       
        else:
            logging.error( " Unknown algorithm '" + str(self.algorithm) + "'. " +
                           "Expecting 'PyMatGen' or 'ValAndErr'." )

        pass # CrystalInformation.loadData()

  def loadPyMatGen( self, cifPath, cifName ):
        if not os.path.isfile( cifPath ):
            logging.error( " Failed to load CIF data, no input file '" + cifPath + "'." )
            return

        self.struct = Structure.from_file( cifPath )

        #logging.error( " Not implemented def loadPyMatGen ( self, path ): " )

        pass # CrystalInformation.loadPyMatGen()

  def evalPyMatGenUnitCell( self ):
    """
    Convert the PyMatGet-structure data into unit-cell information.
    """
    #logging.error( " Not implemented eeetttwww  def loadPyMatGenUnitCell ( self, path ): " )


    # The Unit Cell Lengths:
    self.unitCellLengths = OntoVector( 
                                       className = "UnitCellLengths", 
                                       itemName  = "UnitCellLengths_" + self.cifName,
                                       uuidDB    = self.uuidDB, 
                                       #myName  = "UnitCellLengths_" + self.cifName,
                                       unit      = "om:angstrom" )
         #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )

    self.unitCellLengths.addComponent( label = "a", 
         value = str(round(self.struct.lattice.a, 12)), error = "" )

    self.unitCellLengths.addComponent( label = "b", 
         value = str(round(self.struct.lattice.b, 12)), error = "" )

    self.unitCellLengths.addComponent( label = "c", 
         value = str(round(self.struct.lattice.c, 12)), error = "" )

    # The Unit Cell Angles:
    self.unitCellAngles = OntoVector( className = "UnitCellAngles", 
                                      itemName  = "UnitCellAngles_" + self.cifName,
                                       uuidDB = self.uuidDB, 
                                      unit  = "om:degree" )
         #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )

    self.unitCellAngles.addComponent( label = "alpha", 
         value = str(round(self.struct.lattice.alpha, 12)), error = "" )

    self.unitCellAngles.addComponent( label = "beta", 
         value = str(round(self.struct.lattice.beta , 12)), error = "" )

    self.unitCellAngles.addComponent( label = "gamma", 
         value = str(round(self.struct.lattice.gamma, 12)), error = "" )

    # The Reciprocal Unit Cell Lengths:
    self.unitCellRecipLengths = OntoVector( 
                                       className = "UnitCellLengths", 
                                       itemName  = "ReciprocalUnitCellLengths_" + self.cifName,
                                       uuidDB = self.uuidDB, 
                                       #myName  = "ReciprocalUnitCellLengths_" + self.cifName,
                                       unit  = "om:reciprocalAngstrom" )
         #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom" )

    self.unitCellRecipLengths.addComponent( label = "a", 
         value = str(round(self.struct.lattice.reciprocal_lattice.a, 12)), error = "" )

    self.unitCellRecipLengths.addComponent( label = "b", 
         value = str(round(self.struct.lattice.reciprocal_lattice.b, 12)), error = "" )

    self.unitCellRecipLengths.addComponent( label = "c", 
         value = str(round(self.struct.lattice.reciprocal_lattice.c, 12)), error = "" )

    # The Reciprocal Unit Cell Angles:
    self.unitCellRecipAngles = OntoVector( uuidDB = self.uuidDB, 
                                       className = "UnitCellAngles", 
                                       itemName  = "ReciprocalUnitCellAngles_" + self.cifName,
                                       #myName  = "ReciprocalUnitCellAngles_" + self.cifName,
                                       unit  = "om:degree" )
         #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )

    self.unitCellRecipAngles.addComponent( label = "alpha", 
         value = str(round(self.struct.lattice.reciprocal_lattice.alpha, 12)), error = "" )

    self.unitCellRecipAngles.addComponent( label = "beta", 
         value = str(round(self.struct.lattice.reciprocal_lattice.beta , 12)), error = "" )

    self.unitCellRecipAngles.addComponent( label = "gamma", 
         value = str(round(self.struct.lattice.reciprocal_lattice.gamma, 12)), error = "" )

    # Vectors to keep three Unit Cell vectors (a,b,c):
    self.unitCellVectorA = OntoVector( uuidDB = self.uuidDB, 
                                       className = "UnitCellLatticeVector", 
                                       itemName  = "UnitCellVectorA_" + self.cifName,
                                       unit  = "om:angstrom",
                                       vectorLabel = "a" )

    self.unitCellVectorA.addComponent( label = "x", 
         value = str(round(self.struct.lattice.matrix[0][0], 12)), error = "" )

    self.unitCellVectorA.addComponent( label = "y", 
         value = str(round(self.struct.lattice.matrix[0][1], 12)), error = "" )

    self.unitCellVectorA.addComponent( label = "z", 
         value = str(round(self.struct.lattice.matrix[0][2], 12)), error = "" )

    self.unitCellVectorB = OntoVector( uuidDB = self.uuidDB, 
                                       className = "UnitCellLatticeVector", 
                                       itemName  = "UnitCellVectorB_" + self.cifName,
                                       unit  = "om:angstrom",
                                       vectorLabel = "b" )

    self.unitCellVectorB.addComponent( label = "x", 
         value = str(round(self.struct.lattice.matrix[1][0], 12)), error = "" )

    self.unitCellVectorB.addComponent( label = "y", 
         value = str(round(self.struct.lattice.matrix[1][1], 12)), error = "" )

    self.unitCellVectorB.addComponent( label = "z", 
         value = str(round(self.struct.lattice.matrix[1][2], 12)), error = "" )


    self.unitCellVectorC = OntoVector( uuidDB = self.uuidDB, 
                                       className = "UnitCellLatticeVector", 
                                       itemName  = "UnitCellVectorC_" + self.cifName,
                                       unit  = "om:angstrom",
                                       vectorLabel = "c" )

    self.unitCellVectorC.addComponent( label = "x", 
         value = str(round(self.struct.lattice.matrix[2][0], 12)), error = "" )

    self.unitCellVectorC.addComponent( label = "y", 
         value = str(round(self.struct.lattice.matrix[2][1], 12)), error = "" )

    self.unitCellVectorC.addComponent( label = "z", 
         value = str(round(self.struct.lattice.matrix[2][2], 12)), error = "" )


    # Vectors to keep three Reciprocal Unit Cell vectors (a,b,c):
    self.unitCellRecipVectorA = OntoVector( uuidDB = self.uuidDB, 
                                  className = "UnitCellLatticeVector", 
                                  itemName  = "ReciprocalUnitCellLatticeVectorA_" + self.cifName,
                                  unit  = "om:reciprocalAngstrom",
                                  vectorLabel = "a" )

    self.unitCellRecipVectorA.addComponent( label = "x", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[0][0], 12)), error = "" )

    self.unitCellRecipVectorA.addComponent( label = "y", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[0][1], 12)), error = "" )

    self.unitCellRecipVectorA.addComponent( label = "z", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[0][2], 12)), error = "" )

    self.unitCellRecipVectorB = OntoVector( uuidDB = self.uuidDB, 
                                  className = "UnitCellLatticeVector", 
                                  itemName  = "ReciprocalUnitCellLatticeVectorB_" + self.cifName,
                                  unit  = "om:reciprocalAngstrom",
                                  vectorLabel = "a" )

    self.unitCellRecipVectorB.addComponent( label = "x", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[1][0], 12)), error = "" )

    self.unitCellRecipVectorB.addComponent( label = "y", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[1][1], 12)), error = "" )

    self.unitCellRecipVectorB.addComponent( label = "z", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[1][2], 12)), error = "" )


    self.unitCellRecipVectorC = OntoVector( uuidDB = self.uuidDB, 
                                  className = "UnitCellLatticeVector", 
                                  itemName  = "ReciprocalUnitCellLatticeVectorC_" + self.cifName,
                                  unit  = "om:reciprocalAngstrom",
                                  vectorLabel = "a" )

    self.unitCellRecipVectorC.addComponent( label = "x", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[2][0], 12)), error = "" )

    self.unitCellRecipVectorC.addComponent( label = "y", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[2][1], 12)), error = "" )

    self.unitCellRecipVectorC.addComponent( label = "z", 
         value = str(round(self.struct.lattice.reciprocal_lattice.matrix[2][2], 12)), error = "" )


    self.unitCellVolume = OntoMeasureWithUncertainty( className = "UnitCellVolume", 
                                      itemName = "CellVolume_" + self.cifName,
                                      uuidDB = self.uuidDB )

    self.unitCellVolume.setValue( self.struct.lattice.volume , unit = "om:cubicAngstrom" )
    
    sga = pymatgen.symmetry.analyzer.SpacegroupAnalyzer( self.struct )

    #print( "Zeo Name = ", zeoname )
    if isinstance( sga._space_group_data, dict ):
      if "number" in sga._space_group_data.keys():

        self.symmLatticeSystem = sga.get_crystal_system()
        self.symmITNumber = sga.get_space_group_number()
        """
        print( "SG number:", sga._space_group_data["number"], sga.get_space_group_number(), sga.get_crystal_system() )
        print( "   ", #sga._get_symmetry(), 
                      sga.get_hall(), 
                      sga.get_lattice_type(), #sga.get_symmetry_dataset() 
                      #sga.int_symbol()
                      #sga._abc_impl
                      )
        """
        """
        #if isinstance( sga.get_crystal_system(), str) :
        output.append( [ self.crystOntoPrefix + "hasLatticeSystem", 
                           "Data Property", uuid_cif_uc, "", 
                           sga.get_crystal_system() , "string" ] )
        output.append( [ self.crystOntoPrefix + "hasSymmetryNumber", 
                           "Data Property", uuid_cif_uc, "", 
                           sga.get_space_group_number() , "xsd:integer" ] )
   
    if self.cifOutput.symmLatticeSystem != None:
      output += self.cifOutput.symmLatticeSystem.getArr( uuid_uc_r_vec_abc, 
                self.crystOntoPrefix + "hasLatticeSystem" )

    if self.cifOutput.symmITNumber != None:
      output += self.cifOutput.symmITNumber.getArr( uuid_uc_r_vec_abc, 
                self.crystOntoPrefix + "hasSymmetryNumber" )

        """

    pass # CrystalInformation.evalPyMatGenUnitCell()

  def evalPyMatGen( self ):
        #logging.error( " Not implemented eeetttwww2  def evalPyMatGen ( self, path ): " )

        if "PyMatGen" != self.algorithm:
            logging_error( " Invalid algorithm '" + self.algorithm + "', expectec 'PyMatGen'" )

        self.evalPyMatGenUnitCell()
        self.evalPyMatGenAtom()

        pass # CrystalInformation.evalPyMatGen()

  def evalPyMatGenAtom( self ):
        """
        Convert the PyMatGet-structure data into internal array listAtomAll.


        """
    #logging.error( " Not implemented eeetttrrr  def loadPyMatGenAtom ( self, path ): " )
    #print( "Number of sites =", len(self.struct.sites) )

        self.listAtomAll = []
        for site in self.struct.sites:
            atom = AtomInformation( self.uuidDB, compound = self.cifName   )
            atom.setCoordFrac( site.frac_coords[0], site.frac_coords[1], site.frac_coords[2] )
            atom.setCoordCart( site.coords[0], site.coords[1], site.coords[2] )
            atom.setProp( element = site.species_string )

            #atom.x = site.coords[0]
            #atom.y = site.coords[1]
            #atom.z = site.coords[2]
            #atom.element   = site.species_string
            # FIXME no occupancy ??
            #atom.occupancy = site.species_and_occupancy[0].occupancy
      
            self.listAtomAll.append( atom )

        pass # CrystalInformation.evalPyMatGenAtom()

  def evalValAndErr( self ):
        logging.error( " Not implemented eeetttwww3  def evalValAndErr ( self, path ): " )

        if "ValAndErr" != self.algorithm:
            logging_error( " Invalid algorithm '" + self.algorithm + "', expectec 'ValAndErr'" )
       
        self.evalValAndErrUnitCell()
        #self.evalValAndErrAtom()

        pass # CrystalInformation.evalValAndErr()

  def evalValAndErrUnitCell( self ):

    """
    self.unitCellLengths.addComponent( label = "a", 
         value = str(round(self.struct.lattice.a, 12)), error = "" )

    self.unitCellLengths.addComponent( label = "b", 
         value = str(round(self.struct.lattice.b, 12)), error = "" )

    self.unitCellLengths.addComponent( label = "c", 
         value = str(round(self.struct.lattice.c, 12)), error = "" )


    # Vectors to keep three Unit Cell vectors (a,b,c):
    self.unitCellVectorA = OntoVector( uuidDB = self.uuidDB, 
                                       className = "UnitCellLatticeVector", 
                                       itemName  = "UnitCellVectorA_" + self.cifName,
                                       unit  = "om:angstrom",
         #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                                       vectorLabel = "a" )

    self.unitCellVectorA.addComponent( label = "x", 
         value = str(round(self.struct.lattice.matrix[0][0], 12)), error = "" )

    self.unitCellVectorA.addComponent( label = "y", 
         value = str(round(self.struct.lattice.matrix[0][1], 12)), error = "" )

    self.unitCellVectorA.addComponent( label = "z", 
         value = str(round(self.struct.lattice.matrix[0][2], 12)), error = "" )

    """


    pass # CrystalInformation.evalValAndErrUnitCell()

  def loadValAndErr( self, cifPath, cifName ):
        if not os.path.isfile( cifPath ):
            logging.error( " Failed to load CIF data, no input file '" + cifPath + "'." )
            return

        #self.
        #logging.error( " Not implemented def loadValAndErr ( self, path ): " )

        #All loading is done in one go:
        nBracket = self.readWithUncertainties( cifPath, cifName, save = True )
        #print( "Found brackets:",  nBracket, "in '" + cifName + "'." )

        #self.loadValAndErrUnitCell()
        #self.loadValAndErrAtom()
        #self.loadValAndErr


        pass # CrystalInformation.loadValAndErr()

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

      if il >= 20:
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
            lineNew, nbrac = splitErrorBarLoop( line, self.loopHeaders, file_line )
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
            
          elif words[0] in entriesWithUncertainties:
            logging.info( " Found one of the entries:" + words[0]  )
            vOut, eOut = splitErrorBar( words[1], file_line )
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
    pass # CrystalInformation.readWithUncertainties()


  def setValueAndError( self, cifName, entry, value, error ):
    """
    Setting the value and error values to the internal parameters.

    """

    if ("_cell_length_a" == entry or "_cell_length_b" == entry or 
        "_cell_length_c" == entry) and  (None == self.unitCellLengths):
      self.unitCellLengths = OntoVector( className = "UnitCellLengths", 
                                         itemName  = "UnitCellLengths_" + cifName,
                                         uuidDB    = self.uuidDB, 
                                         unit      = "om:angstrom" )
       #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )

    if ("_cell_angle_alpha" == entry or "_cell_angle_beta" == entry or 
        "_cell_angle_gamma" == entry ) and (None == self.unitCellAngles):
      self.unitCellAngles = OntoVector( 
                                        className = "UnitCellAngles", 
                                        itemName  = "UnitCellAngles_" + cifName,
                                        uuidDB    = self.uuidDB, 
                                        unit      = "om:degree" )
       #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/degree" )

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


    elif "_cell_volume" == entry:
      self.unitCellVolume = OntoMeasureWithUncertainty(  
                tPrefix = "", aPrefix = "", 
                uuidDB = self.uuidDB,
                className = "UnitCellVolume",
                itemName = "UC_Volume_" + str(cifName) )
                  
      self.unitCellVolume.setValue( value = value, error = error, 
                                    unit = "om:cubicAngstrom" )
       #unit = "http://www.ontology-of-units-of-measure.org/resource/om-2/cubicAngstrom" )

    elif "_symmetry_equiv_pos_as_xyz" == entry or "_space_group_symop_operation_xyz":
      # '_space_group_symop_operation_xyz'
      # May appear in list containing _space_group_symop_id
      # '_symmetry_equiv_pos_as_xyz'
      # This definition has been superseded and is retained here only for archival purposes.
      self.listAtomSymm.append( value )

    elif "_atom_site_fract_x" == entry:
      pass
    elif "_atom_site_fract_y" == entry:
      pass
    elif "_atom_site_fract_z" == entry:
      pass
    elif "_atom_site_label"   == entry:
      pass
    elif "_atom_site_type_symbol" == entry:
      pass


    #elif "" == entry:
    else:
      logging.error( " Unknown entry to store data with error: '" + entry + "'." )

    #print( " >>>> entry ", entry, self.algorithm )
    #print( self.unitCellLengths )

    pass # CrystalInformation.setValueAndError()


    """
    if None != self.listAtomRaw:
      logging.warning( " Overwriting 'listAtomRaw' " + where + "." )
    if None != self.listAtomAll:
      logging.warning( " Overwriting 'listAtomAll' " + where + "." )
    if None != self.listAtomSymm:
      logging.warning( " Overwriting 'listAtomSymm' " + where + "." )
    """

  pass # class CrystalInformation


def getCsvInit( baseName, tPrefix, aPrefix ):
    """
    Return the top several lines for the .csv abox:
    - The column headerss,
    - The ABox and TBox definitions.

    """

    output = [ ]
    output.append( [ "Source", "Type", "Target", "Relation", "Value", "Data Type" ] )

    output.append( [ baseName, "Ontology", tPrefix, 
                     "http://www.w3.org/2002/07/owl#imports", "", "" ] )

    output.append( [ baseName, "Ontology", aPrefix, "base", "", "" ] )
    return output
    pass # getCsvInit()

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
  __slots__ = [ "zeoList",  "zeoOption", "zeoOntoPrefix", "crystOntoPrefix",  
                "ontoBase", 
                "inputDir", "outputDir", "cifStandard", "uuidDB",    
                "entriesWithUncertainties", "loopHeaders", #"", "", 
                "unitCellLengths", "unitCellRecipLengths", 
                "unitCellAngles",  "unitCellRecipAngles",

                "unitCellVectorA",      "unitCellVectorB",      "unitCellVectorC",
                "unitCellRecipVectorA", "unitCellRecipVectorB", "unitCellRecipVectorC",
                "unitCellVolume",

                "cifPyMatGen", "cifValAndErr", "cifOutput", # Instances of class CrystalInformation.

                #"", "", "", 
                ]

  def __init__( self ):
        #self.zeoOption = "main"
        self.zeoOption = "test"
        self.zeoList = []

        self.ontoBase        = "OntoZeolite"
        self.zeoOntoPrefix   = "http://www.theworldavatar.com/kg/ontozeolite/"
        self.crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

        self.inputDir  = os.path.join( "ontozeolite", "data" )
        self.outputDir = os.path.join( "ontozeolite", "zeocsv" )

        self.cifStandard = []
        #self.cifStandard = CrystalInformation( "PyMatGen", None ).readStandardFile( "CIF_standard_2.4.5.txt" )

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

        self.unitCellVolume = None
        #baseName = self.ontoBase + "-" + zeoname
        #self.ontoHttp = ""

        pass # CsvMaker.__init__()

  def loadPyMatGen ( self, path, name ):
        """
        Load the CIF by the standard PyMatGen 
        and save it to CrystalInformation class.
        """

        #logging.error( " Not implemented def CsvMaker.loadPyMatGen ( self, path ): " )

        self.cifPyMatGen = CrystalInformation( "PyMatGen", self.uuidDB )
        self.cifPyMatGen.loadData( path, name )

        pass # CsvMaker.loadPyMatGen ()

  def loadValAndErr( self, path, name ):
        """
        Load the CIF data written by this hand-made class (with uncertainty) 
        and save it to CrystalInformation class.
        """

        #logging.error( " Not implemented def loadValAndErr( self, path ): " )

        self.cifValAndErr = CrystalInformation( "ValAndErr", self.uuidDB )
        self.cifValAndErr.loadData( path, name )

        #print( self.cifValAndErr.unitCellLengths )
        pass # CsvMaker.loadValAndErr()

  def evalPyMatGen( self ):
        self.cifPyMatGen.evalPyMatGen()

        pass # CsvMaker.evalPyMatGen()

  def evalValAndErr( self ):
        self.cifValAndErr.evalValAndErr()

        pass # CsvMaker.evalValAndErr()


  def mergeCrystInfo( self ):
        """
        Run through both versions: cifPyMatGen and cifValAndErr and fix by rules:
        j) For cifValAndErr use math to compute not available data,
        2) I there is uncertainty - use it,
        3) If only one CIF has data - use it,
        4) 

        """

        #print( self.cifValAndErr.unitCellLengths )
        self.cifOutput = self.cifPyMatGen
        '''
        self.cifOutput.unitCellLengths      = self.cifValAndErr.unitCellLengths
        self.cifOutput.unitCellAngles       = self.cifValAndErr.unitCellAngles
        self.cifOutput.unitCellRecipLengths = self.cifValAndErr.unitCellRecipLengths
        self.cifOutput.unitCellRecipAngles  = self.cifValAndErr.unitCellRecipAngles
        self.cifOutput.unitCellVolume       = self.cifValAndErr.unitCellVolume
        '''

        #logging.error( " Not implemented def mergeCrystInfo( self ): " )
        #print( " >>> ", self.cifPyMatGen.listAtomAll )

        pass # CsvMaker.mergeCrystInfo()

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

        #self.loadCifStructure( filePath, zeoname )

        nBracket = self.readWithUncertainties( filePath, zeoname, save = True )
        print( "Found brackets:",  nBracket, "in '" + zeoname + "'." )
 
        return
        pass # CsvMaker.loadCifZeolite()


  def loadCifStructure( self, filePath, cifName ):
        """
        This function does not know about zeolites, it can be used for any CIF file.
        Loads CIF file and stores the data in internal variable(s) 
        self.structure ?

        ??? cifName is a unique name for the CIF structure, as it should appear in the ontology abox.
        """

        self.struct = Structure.from_file( filePath )
        pass # CsvMaker.loadCifStructure()

  def prepare( self ):
        self.zeoList = zeolist.getZeoList( self.zeoOption )
        #self.zeoList = self.zeoList[210:240]
        #self.zeoList = self.zeoList[240:]
        #self.zeoList = [ os.path.join( "test", "913885.cif" ) ]

        #self.uuidDB  = tools.loadUUID( )
        self.uuidDB  = tools.UuidDB( os.path.join( "ontozeolite", "uuid", "default.csv") )

        # May be also command line arguments here:
        pass # CsvMaker.prepare()

  def __del__( self ):
        #self.finalize()
        pass # CsvMaker.__del__()

  def finalize( self ):

        #tools.saveUUID( self.uuidDB )
        self.uuidDB.saveDB( )
        pass # CsvMaker.finalize()

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

  def arrInit( self, ontology ):
        output = [ ]
        baseName = self.ontoBase # + "-" + zeoname

        if "zeolite" == ontology:
            tbox = "http://www.theworldavatar.com/kg/ontozeolite/OntoZeolite.owl"
            abox = "http://www.theworldavatar.com/kg/ontozeolite"
        else:
            logging.error( " Unknown ontology name '" + ontology + "'." )

        output += getCsvInit( self.ontoBase, tbox, abox )

        #########################
        # This is a common instance for all measures.

        #uuid_om_angstrom = tools.getUUID( self.uuidDB, self.omOntoPrefix + "Unit", "angstrom" )
        # FIXME This must be initialized in om-2, not here.
        output += omInitUnit( "om:angstrom" )
        output += omInitUnit( "om:reciprocalAngstrom" )
        output += omInitUnit( "om:cubicAngstrom" )
        output += omInitUnit( "om:degree" )
        output += omInitUnit( "om:dimensionOne" )
        output.append( [ omOntoPrefix + "angstrom", "Instance", 
                         omOntoPrefix + "Unit", "", "", "" ] )
        output.append( [ omOntoPrefix + "reciprocalAngstrom", "Instance", 
                         omOntoPrefix + "Unit", "", "", "" ] )
        output.append( [ omOntoPrefix + "cubicAngstrom", "Instance", 
                         omOntoPrefix + "Unit", "", "", "" ] )
        output.append( [ omOntoPrefix + "degree", "Instance", 
                         omOntoPrefix + "Unit", "", "", "" ] )
        output.append( [ omOntoPrefix + "dimensionOne", "Instance", 
                         omOntoPrefix + "Unit", "", "", "" ] )

        # Example of use:
        #omSetUnit( uuid_volume, "cubicAngstrom" )
        '''
        '''
 
        # The framework should be initialized only once.
        #uuidDB = tools.loadUUID( )
        #uuidDB = self.uuidDB
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

    structure = self.cifPyMatGen.struct
    #path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

    #dataIn = tools.readCsv( path )
    #if not os.path.isfile( path ):
    #  logging.error( "File not found '" + path + "'." )
    #  return

    #structure = Structure.from_file( path )
    #uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    #uuid_cif = tools.getUUID( self.uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )

    #uuid_cif_core_trans,_ = self.uuidDB.addUUID( "CoordinateTransformation", 
    #                                   "CoordinateCIFCoreTransform_" + zeoname )
    uuid_cif_core_trans = tools.getUUID( self.uuidDB.uuidDB, "CoordinateTransformation", 
                                         "CoordinateCIFCoreTransform_" + zeoname )
    output.append( [ uuid_cif_core_trans, "Instance", "CoordinateTransformation", "", "", "" ] )
    output.append( [ subject, "Instance", uuid_cif_core_trans, predicate, "", "" ] )

    ################# Fractional to Cartesian ########################
    uuid_m_frac_to_cart = tools.getUUID( self.uuidDB.uuidDB, "TransformationMatrix", 
                          zeoname + "_TransfMatrixToCart" )
    output.append( [ uuid_m_frac_to_cart, "Instance", 
                           "TransformationMatrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "Instance", uuid_m_frac_to_cart,  
                     self.crystOntoPrefix + "hasTransformationMatrixToCartesian", "", "" ] )

    output.append( [ uuid_m_frac_to_cart, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for iy in range(3):
        for ix in range(3):
            uuid_m_comp = tools.getUUID( self.uuidDB.uuidDB, "MatrixComponent", 
                        "MatrixComponentToCartesian"+str(ix)+str(iy)+"_" + zeoname )
            output.append( [ uuid_m_comp, "Instance", 
                           "MatrixComponent", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
            output.append( [ uuid_m_frac_to_cart, "Instance", uuid_m_comp,  
                     self.crystOntoPrefix + "hasMatrixComponent", "", "" ] )

            output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.crystOntoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "xsd:integer" ] )

            output.append( [ self.crystOntoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "xsd:integer" ] )

            output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )

    uuid_v_frac_to_cart = tools.getUUID( self.uuidDB.uuidDB, "Vector", 
                          zeoname + "_TransfVectorToCart" )
    output.append( [ uuid_v_frac_to_cart, "Instance", 
                           "PositionVector", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "Instance", uuid_v_frac_to_cart,  
                     self.crystOntoPrefix + "hasTransformationVectorToCartesian", "", "" ] )

    output.append( [ uuid_v_frac_to_cart, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for ix in range(3):
        uuid_v_comp = tools.getUUID( self.uuidDB.uuidDB, "VectorComponent", 
                    "VectorComponentToCartesian" + str(ix) + "_" + zeoname )
        output.append( [ uuid_v_comp, "Instance", 
                       "VectorComponent", "", "", "" ] )
                       #"CIFCoreTransformationVectorToCartesian", "", "", "" ] )
        output.append( [ uuid_v_frac_to_cart, "Instance", uuid_v_comp,  
                         self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentIndex", "Data Property", 
                         uuid_v_comp, "", ix, "xsd:integer" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )


    ################# Cartesian to Fractional ########################
    uuid_m_cart_to_frac = tools.getUUID( self.uuidDB.uuidDB, "TransformationMatrix", 
                          zeoname + "TransfMatrixToFrac")
    output.append( [ uuid_m_cart_to_frac, "Instance", 
                           "TransformationMatrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "Instance", uuid_m_cart_to_frac,  
                     self.crystOntoPrefix + "hasTransformationMatrixToFractional", "", "" ] )

    output.append( [ uuid_m_cart_to_frac, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for iy in range(3):
        for ix in range(3):
            uuid_m_comp = tools.getUUID( self.uuidDB.uuidDB, "MatrixComponent", 
                        "MatrixComponentToFractional"+str(ix)+str(iy)+"_" + zeoname )
            output.append( [ uuid_m_comp, "Instance", 
                           "MatrixComponent", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
            output.append( [ uuid_m_cart_to_frac, "Instance", uuid_m_comp,  
                     self.crystOntoPrefix + "hasMatrixComponent", "", "" ] )

            output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.crystOntoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "xsd:integer" ] )

            output.append( [ self.crystOntoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "xsd:integer" ] )

            output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.reciprocal_lattice.matrix[iy][ix]/TWOPI, 12), "decimal" ] )

    uuid_v_cart_to_frac = tools.getUUID( self.uuidDB.uuidDB, "PositionVector", 
                          zeoname + "_TransfVectorToFrac" )
    output.append( [ uuid_v_cart_to_frac, "Instance", 
                           "PositionVector", "", "", "" ] )
                           #"CIFCoreTransformationVectorToFractional", "", "", "" ] )

    output.append( [ uuid_cif_core_trans, "Instance", uuid_v_cart_to_frac,  
                     self.crystOntoPrefix + "hasTransformationVectorToFractional", "", "" ] )

    output.append( [ uuid_v_cart_to_frac, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for ix in range(3):
        uuid_v_comp = tools.getUUID( self.uuidDB.uuidDB, "VectorComponent", 
                    "VectorComponentToFractional" + str(ix) + "_" + zeoname )
        output.append( [ uuid_v_comp, "Instance", 
                       "VectorComponent", "", "", "" ] )
                       #"CIFCoreTransformationVectorToCartesian", "", "", "" ] )
        output.append( [ uuid_v_cart_to_frac, "Instance", uuid_v_comp,  
                         self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentIndex", "Data Property", 
                         uuid_v_comp, "", ix, "xsd:integer" ] )

        output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )

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

    #logging.warning( "arrAtomSite() is not implemented yet" )

    output = []

    uuid_atomic = tools.getUUID( self.uuidDB.uuidDB, "AtomicStructure", "AtomicStructure_" + zeoname )

    output.append( [ uuid_atomic, "Instance", "AtomicStructure", "", "", "" ] )
    output.append( [ subject,     "Instance", uuid_atomic, predicate, "", "" ] )

    for ia, atom in enumerate(self.cifOutput.listAtomAll):
      output += atom.getArrAtom( uuid_atomic, self.crystOntoPrefix + "hasAtomSite", 
                                 label = str(ia) )
      pass

    return output
    pass # CsvMaker.arrAtomSite()


  """ Redundant??
  """
  def loadUnitCellPyMatGen( self, zeoname ):
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

    ###########################################
    ###########################################
  
    pass # CsvMaker.loadUnitCellPyMat()


  def arrUnitCell( self, subject, predicate, zeoname ):
 
    """
    subject   - Is the full hame of instance of class CrystalInformation, 
                which contains this UnitCell class.
    predicate - Is the Object Property linking the Subject and the current UnitCell.
                Typically is should be equal to "hasUnitCell".
    """
    if subject.find( "CrystalInformation" ) < 0:
      logging.warning( " Subject in arrUnitCell() is '" + subject + "'," +
                       " expecting the name to contain '" + "CrystalInfromation" + "'." ) 

    if predicate.find( "hasUnitCell" ) < 0:
      logging.warning( " Predicate in arrUnitCell() is '" + predicate + "'," +
                       " but expecting '" + "hasUnitCell" + "'." ) 

    output = []

    uuid_cif_uc = tools.getUUID( self.uuidDB.uuidDB, "UnitCell", "UnitCell_" + zeoname )
    output.append( [ uuid_cif_uc, "Instance", "UnitCell", "", "", "" ] )

    output.append( [ subject, "Instance", uuid_cif_uc, predicate, "", "" ] )

    if not None == self.cifOutput.unitCellLengths:
      output +=    self.cifOutput.unitCellLengths.getCsvArr( uuid_cif_uc, 
                   self.crystOntoPrefix + "hasUnitCellLengths" ) 
    else:
      logging.warning( "Missing unit cell lengths for", self.uuid )
      pass

    if not None == self.cifOutput.unitCellAngles:
      output +=    self.cifOutput.unitCellAngles.getCsvArr(  uuid_cif_uc, 
                   self.crystOntoPrefix + "hasUnitCellAngles" ) 

    if not None == self.cifOutput.unitCellRecipLengths:
      output +=    self.cifOutput.unitCellRecipLengths.getCsvArr( uuid_cif_uc, 
                   self.crystOntoPrefix + "hasReciprocalUnitCellLengths" )

    if not None == self.cifOutput.unitCellRecipAngles:
      output +=    self.cifOutput.unitCellRecipAngles.getCsvArr( uuid_cif_uc, 
                   self.crystOntoPrefix + "hasReciprocalUnitCellAngles" )

    #output += self.cifOutput.unitCellVolume.getCsvArr( uuid_cif_uc, 
    #          self.crystOntoPrefix + "hasUnitCellVolume" )
 
    if not None == self.cifOutput.unitCellVolume:
      output +=    self.cifOutput.unitCellVolume.getCsvArr( uuid_cif_uc, 
                   self.crystOntoPrefix + "hasUnitCellVolume" )
    else:
      logging.warning( "Missing volume for", uuid_cif_uc )
      pass

    #################################################
    # Vector to keep three Unit Cell vectors (a,b,c):
    uuid_uc_vec_abc = tools.getUUID( self.uuidDB.uuidDB, "UnitCellVectorSet", "UnitCellVectorSet_" + zeoname )
    output.append( [ uuid_uc_vec_abc, "Instance", "UnitCellVectorSet", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_vec_abc,  
                     self.crystOntoPrefix + "hasUnitCellVectorSet", "", "" ] )

    output += self.cifOutput.unitCellVectorA.getCsvArr( uuid_uc_vec_abc, self.crystOntoPrefix + "hasUnitCellVector" )
    output += self.cifOutput.unitCellVectorB.getCsvArr( uuid_uc_vec_abc, self.crystOntoPrefix + "hasUnitCellVector" )
    output += self.cifOutput.unitCellVectorC.getCsvArr( uuid_uc_vec_abc, self.crystOntoPrefix + "hasUnitCellVector" )

    # Vector to keep three Reciprocal Unit Cell vectors (a,b,c):
    uuid_uc_r_vec_abc = tools.getUUID( self.uuidDB.uuidDB, "UnitCellVectorSet", "ReciprocalUnitCellVectorSet_" + zeoname )
    output.append( [ uuid_uc_r_vec_abc, "Instance", "UnitCellVectorSet", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_r_vec_abc,  
                     self.crystOntoPrefix + "hasReciprocalUnitCellVectorSet", "", "" ] )

    output += self.cifOutput.unitCellRecipVectorA.getCsvArr( uuid_uc_r_vec_abc, 
              self.crystOntoPrefix + "hasUnitCellVector" )
    output += self.cifOutput.unitCellRecipVectorB.getCsvArr( uuid_uc_r_vec_abc, 
              self.crystOntoPrefix + "hasUnitCellVector" )
    output += self.cifOutput.unitCellRecipVectorC.getCsvArr( uuid_uc_r_vec_abc, 
              self.crystOntoPrefix + "hasUnitCellVector" )

    if self.cifOutput.symmLatticeSystem != None:
      #output += self.cifOutput.symmLatticeSystem.getArr( uuid_uc_r_vec_abc, 
      #          self.crystOntoPrefix + "hasLatticeSystem" )
      output.append( [ self.crystOntoPrefix + "hasLatticeSystem", 
                           "Data Property", uuid_cif_uc, "", 
                           self.cifOutput.symmLatticeSystem, "string" ] )
    else:
      logging.warning( " Missing hasLatticeSystem value" )

    if self.cifOutput.symmITNumber != None:
      #output += self.cifOutput.symmITNumber.getArr( uuid_uc_r_vec_abc, 
      #          self.crystOntoPrefix + "hasSymmetryNumber" )

      output.append( [ self.crystOntoPrefix + "hasSymmetryNumber", 
                           "Data Property", uuid_cif_uc, "", 
                           self.cifOutput.symmITNumber, "integer" ] )
    else:
      logging.warning( " Missing hasSymmetryNumber value" )


    # The symmetry information of the unit cell.
    # TODO to add get_crystal_system(), get_space_group_number(), 
    #      https://pymatgen.org/pymatgen.symmetry.html

    return output
    pass # CsvMaker.arrUnitCell()

  def arrUnitCellOld( self, subject, predicate, zeoname ):
 
    """
    subject   - Is the full hame of instance of class CrystalInformation, 
                which contains this UnitCell class.
    predicate - Is the Object Property linking the Subject and the current UnitCell.
                Typically is should be equal to "hasUnitCell".
    """

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return

    structure = Structure.from_file( path )

    output = []
    #print( "'" + subject + "'", "'" + predicate + "'" )
    if subject.find( "CrystalInformation" ) < 0:
      logging.warning( " Subject in arrUnitCell() is '" + subject + "'," +
                       " expecting the name to contain '" + "CrystalInfromation" + "'." ) 

    if predicate.find( "hasUnitCell" ) < 0:
      logging.warning( " Predicate in arrUnitCell() is '" + predicate + "'," +
                       " but expecting '" + "hasUnitCell" + "'." ) 

    uuid_cif_uc = tools.getUUID( self.uuidDB.uuidDB, "UnitCell", "UnitCell_" + zeoname )
    output.append( [ uuid_cif_uc, "Instance", "UnitCell", "", "", "" ] )

    output.append( [ subject, "Instance", uuid_cif_uc,  
                     predicate, "", "" ] )

    # Unit Cell volume, single value, not vector:
    uuid_uc_volume = tools.getUUID( self.uuidDB.uuidDB, 
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
                     # FIXME

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
    pass # CsvMaker.arrUnitCellOld()

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
    if None == data:
      logging.warning( " No data available for tile '" + zeoname + "'." )
      return None

    #uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )

    uuid_tstructure = tools.getUUID( self.uuidDB.uuidDB, "TiledStructure", "TiledStructure_" + zeoname )
    output.append( [ uuid_tstructure, "Instance", "TiledStructure", "", "", "" ] )
    output.append( [ subject, "Instance", uuid_tstructure, predicate, "", "" ] )

    output.append( [ self.crystOntoPrefix + "hasTileSignature", "Data Property", 
                     uuid_tstructure, "", data[2].strip(' "'), "string" ] )

    ### Begin of transitivity
    uuid_tile_trans = tools.getUUID( self.uuidDB.uuidDB, "Transitivity", "TileNumber_" + zeoname + "_transitivity" )
    output.append( [ uuid_tile_trans, "Instance", "Transitivity",
                     "", "", "" ] )

    uuid_tile_transP = tools.getUUID( self.uuidDB.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityP" )
    output.append( [ uuid_tile_transP, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transQ = tools.getUUID( self.uuidDB.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityQ" )
    output.append( [ uuid_tile_transQ, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transR = tools.getUUID( self.uuidDB.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityR" )
    output.append( [ uuid_tile_transR, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transS = tools.getUUID( self.uuidDB.uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityS" )
    output.append( [ uuid_tile_transS, "Instance", "VectorComponent",
                     "", "", "" ] )

    output.append( [ uuid_tstructure, "Instance", uuid_tile_trans ,  
                     self.crystOntoPrefix + "hasTransitivity", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transP,  
                     self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transQ,  
                     self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transR,  
                     self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transS,  
                     self.crystOntoPrefix + "hasVectorComponent", "", "" ] )

    trans = self.getTransitivity( data[1] )

    output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
                                        uuid_tile_transP, "", "p", "string" ] )
    output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                                        uuid_tile_transP, "", trans[0], "integer" ] )

    output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
                                        uuid_tile_transQ, "", "q", "string" ] )
    output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                                        uuid_tile_transQ, "", trans[1], "integer" ] )

    output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
                                        uuid_tile_transR, "", "r", "string" ] )
    output.append( [ self.crystOntoPrefix + "hasComponentValue", "Data Property", 
                                        uuid_tile_transR, "", trans[2], "integer" ] )

    output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
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
      uuid_tile     = tools.getUUID( self.uuidDB.uuidDB, "Tile", "Tile_" + zeoname + "_cage" + str(ic+1) )
      uuid_tile_num = tools.getUUID( self.uuidDB.uuidDB, "TileNumber", "TileNumber_" + zeoname + "_cage" + str(ic+1) )

      if ic >= len(codeN):
        logging.error( "Too big ic in tile signature, skipping it... FIXME" )
        continue

      output.append( [ uuid_tile, "Instance", "Tile", 
                       "", "", "" ] )

      output.append( [ uuid_tstructure, "Instance", uuid_tile, 
                       self.crystOntoPrefix + "hasTile", "", "" ] )

      output.append( [ self.crystOntoPrefix + "hasTileCode", "Data Property", 
                                         uuid_tile, "UNDEF", codeN[ic], "string" ] )

      output.append( [ self.crystOntoPrefix + "hasNumberOfFaces", "Data Property", 
                                         uuid_tile, "", faceN[ic], "integer" ] )

      output.append( [ self.crystOntoPrefix + "hasNumberOfEdges", "Data Property", 
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
                       self.crystOntoPrefix + "isNumberOfTiles", "", "" ] )

      output.append( [ self.crystOntoPrefix + "hasValue", "Data Property", 
                       #uuid_tile_num, "", cage[0], "" ] )
                       uuid_tile_num, "", cage[0], "integer" ] )
                       #uuid_tile_num, "", cage[0], 
                       #"http://www.w3.org/2001/XMLSchema#integer" ] )


      faces = tilesignature.cageToFaces( cage[1] )

      for f, face in enumerate(faces):
        #print( "face = ", face )
        
        suffix = "_" + zeoname + "_cage" + str(ic+1) + "_face" + str(f+1)
        uuid_tile_face     = tools.getUUID( self.uuidDB.uuidDB, "TileFace", "TileFace"       + suffix )
        uuid_tile_face_num = tools.getUUID( self.uuidDB.uuidDB, "TileFaceNumber", "TileFaceNumber" + suffix )

        output.append( [ uuid_tile_face,     "Instance", "TileFace", 
                         "" , "", "" ] )

        output.append( [ uuid_tile,   "Instance", uuid_tile_face,   
                         self.crystOntoPrefix + "hasTileFace" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", "TileFaceNumber",  
                         "" , "", "" ] )
        
        output.append( [ uuid_tile, "Instance", uuid_tile_face_num,  
                         self.crystOntoPrefix + "hasTileFaceNumber" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", uuid_tile_face,
                         self.crystOntoPrefix + "isNumberOfTileFaces" , "", "" ] )

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
                                       uuid_tile_face, "", "UNDEF", "string" ] )
                                       #uuid_tile_face, "", str("UNKNOWN"), "string" ] )

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
    pass # CsvMaker.getTransitivity()

  def arrSpectrum( self, subject, predicate, zeoname ):
    """
    subject   - 
    predicate - 
    """
    if subject.find("CrystalInformation") < 0:
        logging.warning( " XRDSpectrum expects a subject 'CrystalInformation'," + \
                         " but got '" + subject + "'." )
    if predicate.find("hasXRDSpectrum") < 0:
        logging.warning( " Spectrum expects a predicate 'hasXRDSpectrum'," + \
                         " but got '" + predicate + "'." )
 
    output = []

    xrdFile = "xrd-ed5.csv"
    xrdData = tools.readCsv( xrdFile )
    #print( xrdData[:2] )

    nSpectra = 0
    xrdHead = xrdData[0]
    xrdData = xrdData[1:]
    for xrd in xrdData:

        #print( "Framework:", xrd[2] )
        if zeoname == xrd[2]:
            nSpectra += 1

            uuid_xrd = tools.getUUID( self.uuidDB.uuidDB, "XRDSpectrum", \
                      "XRDSpectrum" + str(nSpectra) +"_" + zeoname)
            output.append( [ uuid_xrd, "Instance", "XRDSpectrum", "", "", "" ] )

            output.append( [ subject, "Instance", uuid_xrd, predicate, "", "" ] )


            #print( zeoname, "=====>", xrd[5] )
            output += self.loadXRDPeaks( uuid_xrd, \
                                self.crystOntoPrefix + "hasCharacteristicPeak", \
                                zeoname + str(nSpectra), xrd[5] )

            if xrd[6] != "" and xrd[6] != None:
                print( "TODO Add DOI " )

            break
    #if z in xrdData:

    return output
    pass # CsvMaker.arrSpectrum()

  def loadXRDPeaks( self, subject, predicate, zeoname, filename ):
    """
    See also
    https://stackoverflow.com/questions/28173128/find-local-maximum-of-data-files-in-gnuplot/56606820#56606820
    
    """
    if subject.find("XRDSpectrum") < 0:
      logging.warning( " CharacteristicPeak expects a subject 'XRDSpectrum', " + \
                       "but got '" + subject + "'." )
    if predicate.find("hasCharacteristicPeak") < 0:
      logging.warning( " Spectrum expects a predicate 'CharacteristicPeak', " + \
                       "but got '" + predicate + "'." )
   
    output = []
    
    if not os.path.isfile( filename ):
      logging.error( "Missing XRD peaks file '" + filename + "' for "+ zeoname )
      return output
    
    f = open( filename, "r" )
    for il,line in enumerate(f):
      words = line.strip().split()
      #print( il, "=>", line.strip(), "=>", words )

      uuid_peak = tools.getUUID( self.uuidDB.uuidDB, "CharacteristicPeak", 
                  "CharacteristicPeak_" + zeoname + "_peak_" + str(il+1) )
      output.append( [ uuid_peak, "Instance", "CharacteristicPeak", "", "", "" ] )

      output.append( [ subject, "Instance", uuid_peak, predicate, "", "" ] )

      output.append( [ self.crystOntoPrefix + "hasRelativeIntensity", 
                      "Data Property", uuid_peak, "", words[6], "decimal" ] )

      output.append( [ self.crystOntoPrefix + "isSimulated", "Data Property", 
                         uuid_peak, "", True, "boolean" ] )

      miller = OntoVector( 
                           className = "MillerIndices", 
                           itemName  = "MillerIndices_" + zeoname + "_peak_" + str(il+1),
                           #myName  = "MillerIndices_" + zeoname + "_peak_" + str(il+1),
                           uuidDB = self.uuidDB, 
                           unit      = "om:dimensionOne" )
       #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom" )

      miller.addComponent( label = "h", value = str(words[0]) )
      miller.addComponent( label = "k", value = str(words[1]) )
      miller.addComponent( label = "l", value = str(words[2]) )
      output += miller.getCsvArr( uuid_peak, self.crystOntoPrefix + "hasMillerIndices" )

      #uuid_peak = tools.getUUID( self.uuidDB, "CharacteristicPeak", 
      #            "CharacteristicPeak_" + zeoname + "_peak_" + str(il+1) )
      #output.append( [ uuid_peak, "Instance", "CharacteristicPeak", "", "", "" ] )
      #output.append( [ subject, "Instance", uuid_peak, predicate, "", "" ] )

      output.append( [ self.crystOntoPrefix + "hasTwoThetaPosition", "Data Property", 
                         uuid_peak, "", words[3], "decimal" ] )


      #output.append( [ self.crystOntoPrefix + "hasComponentLabel", "Data Property", 
      #                   uuid_peak, "", self.value["label"], "string" ] )

      pass

   
    return output
    pass # CsvMaker.loadXRDPeaks()

  # TODO delete this function from this class:
  def splitErrorBarLoop( self, line, headers, file_line ):
    """
    Remove error bars from the CIF file and save the result to a temporary file
    """
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
    pass # CsvMaker.splitErrorBarLoop()

  # TODO detele this function from this class:
  def setValueAndError( self, cifName, entry, value, error ):
        """
        Setting the value and error values to the internal parameters.

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

        self.unitCellLengths = OntoVector( uuidDB = self.uuidDB, 
                                           myClass = "UnitCellLengths", 
                                           myName  = "UnitCellLengths_" + cifName,
                                           myUnit  = "om:angstrom" )

        self.unitCellAngles = OntoVector( uuidDB = self.uuidDB, 
                                           myClass = "UnitCellAngles", 
                                           myName  = "UnitCellAngles_" + cifName,
                                           myUnit  = "om:degree" )


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

        pass # CsvMaker.setValueAndError()

  # TODO delete this function from here:
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
    pass # CsvMaker.splitStr()

  def splitErrorBar( self, value, file_line ):
    # TODO delete this function from here.

    #print( "Starting splitErrorBar() ================== " )
    vOut = value 
    eOut = ""

    if not isinstance( value, str ):
        logging.error( " Impossible error: not a string " + str(line) + " " + \
                       file_line )
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
        logging.error( " Something is wrong with brackets: '" + line + \
                       "' . " + file_line )

    #print( "value, vOut, eOut =", value, vOut, eOut )
    return vOut, eOut
    pass # CsvMaker.splitErrorBar()

  #def getCifLineRanges( self, fileIn ):
  #    return [ 0, 100, 400 ]

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
                self.readWithUncertainties( fileIn, zeoname, lineRanges[i], \
                                            lineRanges[i+1], fileOut = fileOut )

        #else:
        #fileOut = "after-913885.cif"

        return filesOut
        pass # CsvMaker.cleanCif()

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

      if il >= 20:
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
          logging.warning( " Comment starts not from beginning of the line: '" + \
                         line.strip() + "' " + file_line + "."  \
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
    pass # CsvMaker.readWithUncertainties()


  def makeCsvs( self ):
    errCount = 0

    self.prepare()
    #self.zeoList = self.zeoList[0:1]

    t_start = datetime.datetime.now()
    for iz, z in enumerate(self.zeoList):
      #print( "In zeolist z =", z )
      arr  = self.arrInit( "zeolite" )  # The header (ontology description)

      path = os.path.join( "CIF", zeolist.zeoCodeToCode3(z).upper() + ".cif")

      # First load data to (struct in cifPyMatGen and unitCellLengths in cifValAndErr)
      # QQQQQQ
      self.loadPyMatGen(  path, z )
      self.loadValAndErr( path, z )

      # Second evaluate data (create all arrays and internal data):
      # QQQQQQ
      self.evalPyMatGen ()
      self.evalValAndErr()

      # Third choose which to save from which crystal information:
      #print(" ==> ", self.cifValAndErr.unitCellLengths )
      # QQQQQQ
      self.mergeCrystInfo()
      #self.cifOutput = self.cifValAndErr
      #print(" -->  ", self.cifOutput.unitCellLengths )

      #self.loadCifZeolite( z ) 
      #self.evalCifData()

     
      uuid_zeolite = tools.getUUID( self.uuidDB.uuidDB, "ZeoliteFramework", "Zeolite_" + z )
      arr.append( [ uuid_zeolite, "Instance", "ZeoliteFramework", "", "", "" ] )
 
      arr.append( [ self.zeoOntoPrefix + "hasZeoliteCode", "Data Property", 
                    uuid_zeolite, "", z.strip(' "'), "string" ] )


      uuid_cif = tools.getUUID( self.uuidDB.uuidDB, "CrystalInformation", "ZeoliteCIF_" + z )
      arr.append( [ uuid_cif, "Instance", "CrystalInformation", "", "", "" ] )
 

    # Define relation between the class instances:
    #output.append( [ subject, "Instance", uuid_cif,  
      arr.append( [ uuid_zeolite, "Instance", uuid_cif,  
                    self.crystOntoPrefix + "hasCrystalInformation", "", "" ] )


      #self.loadUnitCellPyMatGen( zeoname )

      #arr += self.arrInitZeolite( uuid_zeolite )

      tmp = self.arrUnitCell( uuid_cif, self.crystOntoPrefix + "hasUnitCell", z )
      if None == tmp:
        logging.warning( " Missing Unit Cell information!" )
      else:
        arr += tmp

      tmp = self.arrTransform( uuid_cif, self.crystOntoPrefix + "hasCoordinateTransformation", z )
      if None == tmp:
        logging.warning( " Missing Transformation information!" )
      else:
        arr += tmp

      tmp = self.arrAtomSite( uuid_cif, self.crystOntoPrefix + "hasAtomicStructure", z )
      if None == tmp:
        logging.warning( " Missing Atom Site information!" )
      else:
        arr += tmp

      tmp = self.arrTiles( uuid_cif, self.crystOntoPrefix + "hasTiledStructure", z )
      if None == tmp:
        logging.warning( " Missing Tiled Structure information!" )
      else:
        arr += tmp

      tmp = self.arrSpectrum( uuid_cif, self.crystOntoPrefix + "hasXRDSpectrum", z )
      if None == tmp:
        logging.warning( " Missing Spectrum information!" )
      else:
        arr += tmp
      # FIXME

      #print( "arr =", arr )
      #csvWrite( arr )
      path = os.path.join( self.outputDir, z + ".csv" )
      tools.writeCsv( path, arr )

      if iz > 0 and iz % 10 == 0:
        self.finalize()
        t_finish = datetime.datetime.now()
        t_delta = t_finish - t_start
        t_delta = t_delta.total_seconds()
        print( "Finished", iz, "compounds. Saved uuidDB in", round(t_delta,1), "sec." )
        #print( t_finish - t_start )

      #errCount += 1
      #logging.warning( "Not implemented creating zeolite csv" )
      pass

    self.finalize() # << Important! Saves the current list of the UUIDs

    logging.warning( "Detected " + str(errCount) + " errors" )
    #if errCount > 0:
    #  logging.warning( "Detected " + str(errCount) + " errors" )

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


