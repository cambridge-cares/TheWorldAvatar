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
logging.basicConfig( level = logging.WARNING )
#logging.basicConfig( level = logging.ERROR )

import tools
import zeolist
import tilesignature
import math
import numpy
import argparse

from pymatgen.core.structure import Structure, Lattice


# FIXME
#http://www.w3.org/2000/01/rdf-schema#Literal
class CommandLine:
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


class CsvMaker:
  def __init__( self ):
    #self.zeoOption = "main"
    self.zeoOption = "test"
    self.zeoList = []

    self.ontoBase = "OntoZeolite"
    self.ontoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"

    self.inputDir  = os.path.join( "ontozeolite", "data" )
    self.outputDir = os.path.join( "ontozeolite", "zeocsv" )

    self.cifStandard = []
    self.cifStandard = self.readStandardFile( "CIF_standard_2.4.5.txt" )

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

  def getArrVector( self, subject, predicate, value ):
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

    keys = value.keys()
    if "name" not in keys:
      logging.error( " Creating a vector without the value['name'] specified. I skip it." )
      return []

    if "class" in keys:
      myClass = value["class"]
    else:
      logging.error( " Missing value['class'] for vector '" + value["name"] + 
                     "', will use default 'Vector'."  )
      myClass = "Vector" 

    #if "comp" not in keys:
    #  logging.error( " Missing value['comp'] for vector '" + value["name"] + "'"  )

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
        logging.error( " Unknown vector class '" + myClass + "' for vector '" + value["name"] + "'." )


    # Vector for Unit Cell length parameters (a,b,c):
    uuid_vector = tools.getUUID( self.uuidDB, myClass, value["name"] )
    output.append( [ uuid_vector, "Instance", myClass, "", "", "" ] )
 
    if "UnitCellLatticeVector" == myClass:
      if "label" in keys:
        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_vector, "", value["label"], "string" ] )

      else:
        logging.warning( " Label is not defined for '" + value["name"] + "' of class '" + 
                         myClass + "'." )


    if   isinstance( value["comp"], dict):
      comp_keys = value["comp"].keys()  # comp_keys means 'KEYS of the COMPonents'.
      for ck in comp_keys:
        uuid_comp = tools.getUUID( self.uuidDB, "VectorComponent", value["name"] + "_comp_" + ck  )
        output.append( [ uuid_comp, "Instance", "VectorComponent", "", "", "" ] )

        output.append( [ uuid_vector, "Instance", uuid_comp,  
                         self.ontoPrefix + "hasComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_comp, "", ck, "string" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                         uuid_comp, "", value["comp"][ck]["value"], "rdfs:Literal" ] )
 
        if "unit" in list(value["comp"][ck].keys()):
          unit = value["comp"][ck]["unit"]
          if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
            logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
                             value["name"] + "'. " + 
                             "Expecting an instance of OM-2 ontology class Unit." )

          output.append( [ uuid_comp, "Instance", unit,
                         "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                          "", "" ] )


    if "unit" in keys:
#"http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom"
      unit = value["unit"]
      if not unit.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
        logging.warning( " Possibly wrong unit '" + unit + "' in vector '" + 
                         value["name"] + "'. " + 
                         "Expecting an instance of OM-2 ontology class Unit." )

      output.append( [ uuid_vector, "Instance", unit,
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    elif isinstance( value["comp"], list):
      logging.error( " Not implemented yet list component 2222222 " )

    else:
      logging.error( " Unknown type of value['comp'] = '" + str(type(value["comp"])) + 
                     "' in vector '" + value["name"] + "'." )

    #print( "==============================" )
    return output
    pass # getArrVector()

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
 
    output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
                     uuid_zeolite, "", zeoname.strip(' "'), "string" ] )

    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrInit()

  def arrTransform( self, zeoname ):
    #print( "arrTransform started" )
    TWOPI = 2 * math.pi
    DIRS = "xyz"

    output = []

    #uuidDB = tools.loadUUID( )
    #uuidDB = self.uuidDB

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
                     self.ontoPrefix + "hasCifCoreTransformation", "", "" ] )

    ################# Fractional to Cartesian ########################
    uuid_m_frac_to_cart = tools.getUUID( self.uuidDB, "CIFCoreTransformationMatrixToCartesian", 
                        "ZeoliteCIFTransformationMatrixToCartesian_" + zeoname )
    output.append( [ uuid_m_frac_to_cart, "Instance", 
                           "Matrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_m_frac_to_cart,  
                     self.ontoPrefix + "hasTransformationMatrixToCartesian", "", "" ] )

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
                     self.ontoPrefix + "hasComponent", "", "" ] )


            output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.ontoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "integer" ] )

            output.append( [ self.ontoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "integer" ] )

            output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )

    uuid_v_frac_to_cart = tools.getUUID( self.uuidDB, "Vector", 
                        "ZeoliteCIFTransformationVectorToCartesian_" + zeoname )
    output.append( [ uuid_v_frac_to_cart, "Instance", 
                           "CIFCoreTransformationVectorToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_v_frac_to_cart,  
                     self.ontoPrefix + "hasTransformationVectorToCartesian", "", "" ] )

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
                         self.ontoPrefix + "hasComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.ontoPrefix + "hasIndex", "Data Property", 
                         uuid_v_comp, "", ix, "integer" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )
                         #round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )


    ################# Cartesian to Fractional ########################
    uuid_m_cart_to_frac = tools.getUUID( self.uuidDB, "CIFCoreTransformationMatrixToFractional", 
                        "ZeoliteCIFTransformationMatrixToFractional_" + zeoname )
    output.append( [ uuid_m_cart_to_frac, "Instance", 
                           "Matrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_m_cart_to_frac,  
                     self.ontoPrefix + "hasTransformationMatrixToFractional", "", "" ] )

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
                     self.ontoPrefix + "hasComponent", "", "" ] )

            output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.ontoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "integer" ] )

            output.append( [ self.ontoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "integer" ] )

            output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.reciprocal_lattice.matrix[iy][ix]/TWOPI, 12), "decimal" ] )

    uuid_v_cart_to_frac = tools.getUUID( self.uuidDB, "Vector", 
                        "ZeoliteCIFTransformationVectorToFractional" + zeoname )
    output.append( [ uuid_v_cart_to_frac, "Instance", 
                           "Vector", "", "", "" ] )
                           #"CIFCoreTransformationVectorToFractional", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_v_cart_to_frac,  
                     self.ontoPrefix + "hasTransformationVectorToFractional", "", "" ] )

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
                         self.ontoPrefix + "hasComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.ontoPrefix + "hasIndex", "Data Property", 
                         uuid_v_comp, "", ix, "integer" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )
                         #round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )


    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrTransform()
 
  def arrAtomSite( self, zeoname ):
    logging.warning( "arrAtomSite() is not implemented yet" )

    output = []

    #uuidDB = tools.loadUUID( )
    #uuidDB = self.uuidDB

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")
    #dataIn = tools.readCsv( path )
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return

    structure = Structure.from_file( path )

    uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )

    uuid_cif = tools.getUUID( self.uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )
 

    return output
    pass # CsvMaker.arrAtomSite()

  def arrUnitCell( self, zeoname ):
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

    # Define class instances:
    uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
 
    #output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
    #                 uuid_zeolite, "", zeoname.strip(' "'), "string" ] )

    uuid_cif = tools.getUUID( self.uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )
    output.append( [ uuid_cif, "Instance", "CIFCore", "", "", "" ] )
 
    uuid_cif_uc = tools.getUUID( self.uuidDB, "CIFCoreCell", "UnitCell_" + zeoname )
    output.append( [ uuid_cif_uc, "Instance", "CIFCoreCell", "", "", "" ] )

    # Define relation between the class instances:
    output.append( [ uuid_zeolite, "instance", uuid_cif,  
                     self.ontoPrefix + "hasCIF", "", "" ] )

    output.append( [ uuid_cif, "Instance", uuid_cif_uc,  
                     self.ontoPrefix + "hasCIFCoreCell", "", "" ] )

    ###########################################

    # Vector for Unit Cell length parameters (a,b,c):
    tmp = {"class":"UnitCellLengths", 
           "name": "UnitCellLengths_" + zeoname,
           "comp":{ "a": {"value":round(structure.lattice.a, 12)},
                    "b": {"value":round(structure.lattice.b, 12)},
                    "c": {"value":round(structure.lattice.c, 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
           }
    arr = self.getArrVector( uuid_cif_uc, self.ontoPrefix + "hasUnitCellLengths", tmp )
    for line in arr:
      output.append( line )

    # Vector for Unit Cell angles parameters (alpha,beta,gamma):
    tmp = {"class":"UnitCellAngles", 
           "name": "UnitCellAngles_" + zeoname,
           "comp":{ "alpha": {"value":round(structure.lattice.alpha, 12)},
                    "beta" : {"value":round(structure.lattice.beta , 12)},
                    "gamma": {"value":round(structure.lattice.gamma, 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/degree",
           }
    arr = self.getArrVector( uuid_cif_uc, self.ontoPrefix + "hasUnitCellAngless", tmp )
    for line in arr:
      output.append( line )

    # Vector for reciprocal Unit Cell length parameters (a*,b*,c*):
    tmp = {"class":"UnitCellLengths", 
           "name": "UnitCellReciprocalLengths_" + zeoname,
           "comp":{ "a*": {"value":round(structure.lattice.reciprocal_lattice.a, 12)},
                    "b*": {"value":round(structure.lattice.reciprocal_lattice.b, 12)},
                    "c*": {"value":round(structure.lattice.reciprocal_lattice.c, 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
           }
    arr = self.getArrVector( uuid_cif_uc, self.ontoPrefix + "hasUnitCellReciprocalLengths", tmp )
    for line in arr:
      output.append( line )

    # Vector for Reciprocal Unit Cell angle parameters (alpha*,beta*,gamma*):
    tmp = {"class":"UnitCellAngles", 
           "name": "UnitCellReciprocalAngles_" + zeoname,
           "comp":{ "alpha*": {"value":round(structure.lattice.reciprocal_lattice.alpha, 12)},
                    "beta*" : {"value":round(structure.lattice.reciprocal_lattice.beta , 12)},
                    "gamma*": {"value":round(structure.lattice.reciprocal_lattice.gamma, 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/degree",
           }
    arr = self.getArrVector( uuid_cif_uc, self.ontoPrefix + "hasUnitCellReciprocalAngles", tmp )
    for line in arr:
      output.append( line )

    ###########################################

    ###########################################
    # Vector to keep three Unit Cell vectors (a,b,c):
    uuid_uc_vec_abc = tools.getUUID( self.uuidDB, "UnitCellVectorSet", "UnitCellVectorSet_" + zeoname )
    output.append( [ uuid_uc_vec_abc, "Instance", "UnitCellVectorSet", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_vec_abc,  
                     self.ontoPrefix + "hasUnitCellVectorSet", "", "" ] )

    tmp = {"class":"UnitCellLatticeVector", 
           "name": "UnitCellLatticeVectorA_" + zeoname,
           "comp":{ "x": {"value": round(structure.lattice.matrix[0][0], 12)},
                    "y": {"value": round(structure.lattice.matrix[0][1], 12)},
                    "z": {"value": round(structure.lattice.matrix[0][2], 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
           "label": "a"
           }
    arr = self.getArrVector( uuid_uc_vec_abc, self.ontoPrefix + "hasLatticeVector", tmp )
    for line in arr:
      output.append( line )


    tmp = {"class":"UnitCellLatticeVector", 
           "name": "UnitCellLatticeVectorB_" + zeoname,
           "comp":{ "x": {"value": round(structure.lattice.matrix[1][0], 12)},
                    "y": {"value": round(structure.lattice.matrix[1][1], 12)},
                    "z": {"value": round(structure.lattice.matrix[1][2], 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
           "label": "b"
           }
    arr = self.getArrVector( uuid_uc_vec_abc, self.ontoPrefix + "hasLatticeVector", tmp )
    for line in arr:
      output.append( line )

    tmp = {"class":"UnitCellLatticeVector", 
           "name": "UnitCellLatticeVectorC_" + zeoname,
           "comp":{ "x": {"value": round(structure.lattice.matrix[2][0], 12)},
                    "y": {"value": round(structure.lattice.matrix[2][1], 12)},
                    "z": {"value": round(structure.lattice.matrix[2][2], 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
           "label": "c"
           }

    arr = self.getArrVector( uuid_uc_vec_abc, self.ontoPrefix + "hasLatticeVector", tmp )
    for line in arr:
      output.append( line )

    # Vector for Reciprocal Unit Cell vectors (a*,b*,c*):
    uuid_uc_vec_r_abc = tools.getUUID( self.uuidDB, "UnitCellVectorSet", "ReciprocalUnitCellVectorSet_" + zeoname )
    output.append( [ uuid_uc_vec_r_abc, "Instance", "UnitCellVectorSet", "", "", "" ] )
 
    output.append( [ uuid_cif_uc, "Instance", uuid_uc_vec_r_abc,  
                     self.ontoPrefix + "hasReciprocalUnitCellVectorSet", "", "" ] )

    tmp = {"class":"UnitCellLatticeVector", 
           "name": "UnitCellReciprocalLatticeVectorA_" + zeoname,
           "comp":{ "x": {"value": round(structure.lattice.reciprocal_lattice.matrix[0][0], 12)},
                    "y": {"value": round(structure.lattice.reciprocal_lattice.matrix[0][1], 12)},
                    "z": {"value": round(structure.lattice.reciprocal_lattice.matrix[0][2], 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
           "label": "a*"
           }
    arr = self.getArrVector( uuid_uc_vec_r_abc, self.ontoPrefix + "hasLatticeVector", tmp )
    for line in arr:
      output.append( line )


    tmp = {"class":"UnitCellLatticeVector", 
           "name": "UnitCellReciprocalLatticeVectorB_" + zeoname,
           "comp":{ "x": {"value": round(structure.lattice.reciprocal_lattice.matrix[1][0], 12)},
                    "y": {"value": round(structure.lattice.reciprocal_lattice.matrix[1][1], 12)},
                    "z": {"value": round(structure.lattice.reciprocal_lattice.matrix[1][2], 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
           "label": "b*"
           }
    arr = self.getArrVector( uuid_uc_vec_r_abc, self.ontoPrefix + "hasLatticeVector", tmp )
    for line in arr:
      output.append( line )

    tmp = {"class":"UnitCellLatticeVector", 
           "name": "UnitCellReciprocalLatticeVectorC_" + zeoname,
           "comp":{ "x": {"value": round(structure.lattice.reciprocal_lattice.matrix[2][0], 12)},
                    "y": {"value": round(structure.lattice.reciprocal_lattice.matrix[2][1], 12)},
                    "z": {"value": round(structure.lattice.reciprocal_lattice.matrix[2][2], 12)}
                  },
           "unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
           "label": "c*"
           }

    arr = self.getArrVector( uuid_uc_vec_r_abc, self.ontoPrefix + "hasLatticeVector", tmp )
    for line in arr:
      output.append( line )

    ###########################################

    # Unit cell volume, single value, not vector:

    uuid_uc_volume = tools.getUUID( self.uuidDB, 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure", 
                     "UnitCellVolume_" + zeoname )

    output.append( [ uuid_uc_volume, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure",
                     "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_volume,  
                     self.ontoPrefix + "hasVolume", "", "" ] )

    output.append( [ "http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", 
                     "Data Property", uuid_uc_volume, "", 
                     round(structure.lattice.volume, 12) , "decimal" ] )

    output.append( [ uuid_uc_volume, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/cubicAngstrom", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    ###########################################

    ###########################################

    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrUnitCell()

  def arrTiles( self, zeoname ):
    output = []

    #uuidDB = tools.loadUUID()
    #uuidDB = self.uuidDB

    path = os.path.join( self.inputDir, "Tile-signature-2023.csv" )
    dataIn = tools.readCsv( path )

    data = tilesignature.getDataByCode( dataIn, zeoname )

    uuid_zeolite = tools.getUUID( self.uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )

    uuid_tstructure = tools.getUUID( self.uuidDB, "TiledStructure", "TiledStructure_" + zeoname )
    output.append( [ uuid_tstructure, "Instance", "TiledStructure",
                     "", "", "" ] )

    output.append( [ uuid_zeolite, "Instance", uuid_tstructure,  
                     self.ontoPrefix + "hasTiledStructure", "", "" ] )

    output.append( [ self.ontoPrefix + "hasTileSignature", "Data Property", 
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
                     self.ontoPrefix + "hasTransitivity", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transP,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transQ,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transR,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transS,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    trans = self.getTransitivity( data[1] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transP, "", "p", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                                        uuid_tile_transP, "", trans[0], "integer" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transQ, "", "q", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                                        uuid_tile_transQ, "", trans[1], "integer" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transR, "", "r", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                                        uuid_tile_transR, "", trans[2], "integer" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transS, "", "s", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
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
                       self.ontoPrefix + "hasTile", "", "" ] )

      output.append( [ self.ontoPrefix + "hasTileCode", "Data Property", 
                                         uuid_tile, "", codeN[ic], "string" ] )

      output.append( [ self.ontoPrefix + "hasNumberOfFaces", "Data Property", 
                                         uuid_tile, "", faceN[ic], "integer" ] )

      output.append( [ self.ontoPrefix + "hasNumberOfEdgges", "Data Property", 
                                         uuid_tile, "", edgeN[ic], "integer" ] )

      output.append( [ self.ontoPrefix + "hasNumberOfVertices", "Data Property", 
                                         uuid_tile, "", vertN[ic], "integer" ] )

      # TODO
      #output.append( [ self.ontoPrefix + "hasTileSymmetry", "Data Property", 
      #                                   uuid_tile, "", symmN[ic], "string" ] )

      output.append( [ self.ontoPrefix + "hasTileSignature", "Data Property", 
                                         uuid_tile, "", signN[ic], "string" ] )

      output.append( [ uuid_tile_num, "Instance", "TileNumber", 
                       "", "", "" ] )

      output.append( [ uuid_tstructure, "Instance", uuid_tile_num, 
                       self.ontoPrefix + "hasTileNumber", "", "" ] )

      output.append( [ uuid_tile_num, "Instance", uuid_tile, 
                       self.ontoPrefix + "isNumberOf", "", "" ] )

      output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
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
                         self.ontoPrefix + "hasTileFace" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", "TileFaceNumber",  
                         "" , "", "" ] )
        
        output.append( [ uuid_tile, "Instance", uuid_tile_face_num,  
                         self.ontoPrefix + "hasTileFaceNumber" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", uuid_tile_face,
                         self.ontoPrefix + "isNumberOf" , "", "" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                              #uuid_tile_face_num, "", face[1], "" ] )
                              uuid_tile_face_num, "", face[1], "integer" ] )
                              #uuid_tile_face_num, "", face[1], 
                              #"http://www.w3.org/2001/XMLSchema#integer" ] )

        output.append( [ self.ontoPrefix + "hasNumberOfEdges", "Data Property", 
                              #uuid_tile_face, "", int(face[0]), "" ] )
                              uuid_tile_face, "", int(face[0]), "integer" ] )
                              #uuid_tile_face, "", int(face[0]), 
                              #"http://www.w3.org/2001/XMLSchema#integer" ] )

        output.append( [ self.ontoPrefix + "hasFaceCode", "Data Property", 
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
    #print( self.entriesToCheck )
    #for i in range( len(headers) ):
    for ih, h in enumerate(headers):
      #print( "=== '" + h + "' ===" )
      if h in self.entriesToCheck:
        #print( "   need to process" )
        #logging.warning( " Found one of the entries" )
        vOut, eOut = self.splitErrorBar( words[ih].strip(), file_line )
        if eOut != "":
          nBracket += 1
        pos = line.find( words[ih] )
        lineNew = lineNew.replace( words[ih], vOut )

    #print( "lineNew =", lineNew )
    return lineNew, nBracket


  def splitStr( self, value ):

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
      logging.info( " Brackets are not detected. " + file_line )
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

      nBracket = self.readUncertainties( fileIn, lineRanges[i], lineRanges[i+1] )
      if ( nBracket > 0 ) or ( len(lineRanges) - 1 > 1 ):
        fileBase, ext = os.path.splitext( os.path.basename(fileIn) )
        print( "Input file name and extension:", fileBase, ext )

        fileOut = os.path.join( tmpDir, fileBase + "_" + str(i+1) + ext )
        filesOut.append( fileOut )
        self.readUncertainties( fileIn, lineRanges[i], lineRanges[i+1], fileOut = fileOut )

    #else:
    #fileOut = "after-913885.cif"

    return filesOut
    pass # cleanCif()

  def readUncertainties(self, fileIn, lineFr, lineTo, fileOut = "" ):
    """
    This function can do three different operations:
    1) read fileIn and count the number of brackets (i.e. the uncertainties)
       The argument fileOut = "" or not specified.
    2) Save a modified file whith uncertanties removed, so that the file
       can be used for reading by standard libraries.
       Specify the fileOut a path.
    3) Read both value and uncertainty and assign to the internal variables,
       which later can be saved into an ABox formal (.csv).

      Return In all 3 cases function returns the number of detected uncertainties.

    """

    self.entriesToCheck = [ "_cell_length_a", "_cell_length_b", "_cell_length_c", 
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
      return

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
                       #+ " This is not supported by readUncertainties()." 
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
        #print( "Checking property", words[0], "is it in list of known?", words[0] in self.entriesToCheck ) 
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
            
          elif words[0] in self.entriesToCheck:
            #logging.info( " Found one of the entries" )
            vOut, eOut = self.splitErrorBar( words[1], file_line )
            if "" != eOut:
              countBrackets += 1

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

    print( "Number of brackets =", countBrackets )
    return countBrackets
    pass # readUncertainties()


  def makeCsvs( self ):
    errCount = 0

    self.prepare()

    for z in self.zeoList:
      #print( "In zeolist z =", z )
      arr  = self.arrInit( z )
      #arr += self.arrTiles( z )
      arr += self.arrUnitCell( z )
      #arr += self.arrAtomSite( z )
      #arr += self.arrTransform( z )

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


if __name__ == "__main__":
  
  settings = CommandLine()

  a = CsvMaker()

  a.makeCsvs()

  fileIn  = os.path.join( "test", "913885.cif" )

  filesOut = a.cleanCif( fileIn )

  print( "Created", len(filesOut), "file(s) after clean-up:", filesOut )

  #input_strings = [ "123450(16)", "123450(160)", "1234.5(16)", "123.45(16)", "12.345(16)"]
  #for input_string in input_strings:
  #  value, error = a.splitStr( input_string )
  #  print(f"Input: {input_string}, Value: {value}, Error: {error}")
    
  #input_strings = [ "1 2 3 4 5", "1   2  3   4  5\n", "1 '2 3' 4 5", "1 2 3 '4   5'"  ]
  #for input_string in input_strings:
  #  out = tools.strSplit(input_string )
  #  print( f"Input: {input_string}, Out: {str(out)}" )
  #  #print( out )


