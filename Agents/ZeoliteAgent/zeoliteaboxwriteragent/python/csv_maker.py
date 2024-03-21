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
#import csv
import math

#import numpy
import argparse
import datetime

import logging

import tools
import zeolist
import tilesignature
#import crystaldata
import crystalinfo

import ontozeolite
import zeolite_db

#from ontocrystal_datatypes import *
import ontocrystal_datatypes as ocdt

import ontocrystal_species as ocos

#logging.basicConfig(level = logging.DEBUG)
#logging.basicConfig(level = logging.INFO)
#logging.basicConfig(level = logging.WARNING)
logging.basicConfig(level = logging.ERROR)

# Ontology for Crystal,
#crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

def is_http(value):
    if not isinstance(value, str):
        print(" In is_http input is not a string: '", str(value), "'.", sep="")
        return False
    if value.strip().lower().startswith("http://") or \
       value.strip().lower().startswith("https://"):
        return True
    return False

# FIXME
#http://www.w3.org/2000/01/rdf-schema#Literal
class CommandLine:
    __slots__ = ["out_dir", "zeo_fr", "zeo_to", "cif_file"
                 # , "", "", "", ""
             ]

    def __init__(self):
        parser = argparse.ArgumentParser(description='what is this?')

        parser.add_argument('--cif', '-c', type=str, default='',
                            help='file name of a single CIF file.')
        parser.add_argument('--part', '-p', type=int, default=None,
                            help='an option to choose a part of zeo_list.')
        parser.add_argument('--fr', '-f', type=int, default=None,
                            help='start index of the zeo_list.')
        parser.add_argument('--to', '-t', type=int, default=None,
                            help='end index of the zeo_list.')
        parser.add_argument('--output', '-o', type=str, default='',
                            help='the output directory to store the results.')

        args = parser.parse_args()
        #print(args.accumulate, args.cif)

        #self.cif_file = ""
        #self.zeo_fr = None
        #self.zeo_to = None
        #self.out_dir = ""
        self.cif_file = args.cif
        self.zeo_fr = args.fr
        self.zeo_to = args.to
        self.out_dir = args.output

        if args.cif != "":
            print("Input zeolite frameworks (i.e. CIF files) = '", args.cif,
                  "'. Accepted: 'all' or none.", sep="")
            #self.cif_file = args.cif
        if args.part is not None:
            print("Input part of list = '", args.part, "'.", sep="")
            print("Not implemented yet")
        if args.fr is not None or args.to is not None:
            print("Range of zeo_list: = [", args.fr, " -> ", args.to, ").", sep="")
            #self.zeo_fr = args.fr
            #self.zeo_to = args.to
        if args.output != "":
            print("The output directory is : = '", args.output, "'.", sep="")
            #self.out_dir = args.output
        #self.file =

        # === end of CommandLine.__init__()

    def _get_output_folder(self):
        return
        # === end of CommandLine._get_output_folder()

    # === end of class CommandLine

def get_csv_init(base_name, tbox_prefix, abox_prefix):
    """
    Return the top several lines for the .csv abox:
    - The column headerss,
    - The ABox and TBox definitions.

    """

    output = []
    output.append(["Source", "Type", "Target", "Relation", "Value", "Data Type"])

    output.append([base_name, "Ontology", tbox_prefix,
                     "http://www.w3.org/2002/07/owl#imports", "", ""])

    output.append([base_name, "Ontology", abox_prefix, "base", "", ""])

    return output
    # === end of get_csv_init()

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
    __slots__ = ["zeoList",  "zeoOption", "zeoOntoPrefix", "crystOntoPrefix",
                 "ontoBase", "inputDir", "outputDir", "cifStandard", "uuidDB",
                 "entriesWithUncertainties", "loopHeaders", #"", "",
                 "unitCellLengths", "unitCellRecipLengths",
                 "unitCellAngles",  "unitCellRecipAngles",

                 "unitCellVectorA",      "unitCellVectorB",      "unitCellVectorC",
                 "unitCellRecipVectorA", "unitCellRecipVectorB", "unitCellRecipVectorC",
                 "unitCellVolume",

                 "cifPyMatGen", "cifValAndErr", "cifOutput",  # Instances of CrystalInformation
                 "settings"

                #"", "", "",
            ]

    def __init__(self):
        #self.zeoOption = "main"
        self.zeoOption = "test"
        self.zeoOption = ["main", "new"]
        self.zeoList = []

        self.ontoBase        = "OntoZeolite"
        self.zeoOntoPrefix   = "http://www.theworldavatar.com/kg/ontozeolite/"
        self.crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

        self.cifStandard = []
        #self.cifStandard = CrystalInformation("PyMatGen", None).readStandardFile("CIF_standard_2.4.5.txt")

        #self.unitCellLengths = None
        #self.unitCellAngles = None
        #self.unitCellRecipLengths = None
        #self.unitCellRecipAngles = None

        #self.unitCellVectorA = None
        #self.unitCellVectorB = None
        #self.unitCellVectorC = None

        #self.unitCellRecipVectorA = None
        #self.unitCellRecipVectorB = None
        #self.unitCellRecipVectorC = None

        #self.unitCellVolume = None
        #baseName = self.ontoBase + "-" + zeoname
        #self.ontoHttp = ""
        self.settings = None

        self.cifValAndErr = None
        self.cifPyMatGen = None
        self.cifOutput = None
        # === end of CsvMaker.__init__()


    def loadCifZeolite(self, zeoname):
        """
        - zeolite-specific, calls the next funtion:
        """

        filePath = os.path.join("CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

        #print("Using a test case 913885.cif for the UnitCell class")
        #path = os.path.join("test", "913885.cif")
        #dataIn = tools.readCsv(path)
        if not os.path.isfile(filePath):
            logging.error("File not found '" + filePath + "'.")
            return

        #self.loadCifStructure(filePath, zeoname)

        nBracket = self.readWithUncertainties(filePath, zeoname, save = True)
        print("Found brackets:",  nBracket, "in '" + zeoname + "'.")

        return
        # === end of CsvMaker.loadCifZeolite()

    def loadCifStructure(self, filePath, cif_name):
        """
        This function does not know about zeolites, it can be used for any CIF file.
        Loads CIF file and stores the data in internal variable(s)
        self.structure ?

        ? cifName is a unique name for the CIF structure, as it should appear
          in the ontology abox.
        """

        self.struct = Structure.from_file(filePath)
        # === end of CsvMaker.loadCifStructure()

    def prepare(self):
        if self.settings.cif_file == "all":
            self.zeoList = zeolist.getZeoList(["main", "new"])

        elif self.settings.cif_file == "":
            self.zeoList = zeolist.getZeoList(self.zeoOption)
            #self.zeoList = self.zeoList[210:240]
            #self.zeoList = self.zeoList[240:]
            #self.zeoList = [os.path.join("test", "913885.cif")]
            #self.zeoList = self.zeoList[0:10]

        else:
            logging.error(" Invalid command line argument '%s', expect" +
                          " Framework code or 'all'", self.settings.cif_file)

        self.inputDir  = os.path.join("ontozeolite", "data")

        if self.settings.out_dir == "":
            self.uuidDB = tools.UuidDB(os.path.join("ontozeolite", "uuid", "default.csv"))
            self.outputDir = os.path.join("ontozeolite", "zeocsv")
        else:
            if os.path.isdir(self.settings.out_dir):
                self.uuidDB = tools.UuidDB(os.path.join(self.settings.out_dir, "default.csv"))
                self.outputDir = self.settings.out_dir
            else:
                logging.error("Invalid output folder '%s'.", self.settings.out_dir)

        if self.settings.zeo_fr is not None and self.settings.zeo_to is not None:
            self.zeoList = self.zeoList[self.settings.zeo_fr:self.settings.zeo_to]

        #1/0
        # May be also command line arguments here:
        # === end of CsvMaker.prepare()

    def __del__(self):
        #self.finalize()
        # === end of CsvMaker.__del__()
        pass

    def finalize(self):

        self.uuidDB.saveDB()
        # === end of CsvMaker.finalize()

    '''
  def getArrVector(self, subject, predicate, vector):
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
    #logging.error(" getArrVector() is not implemented yet")
    # FIXME TODO

    keys = vector.keys()
    if "name" not in keys:
      logging.error(" Creating a vector without the vector['name'] specified. I skip it.")
      return []

    if "class" in keys:
      myClass = vector["class"]
    else:
      logging.error(" Missing vector['class'] for vector '" + vector["name"] +
                     "', will use default 'Vector'.")
      myClass = "Vector"

    if "comp" not in keys:
      logging.error(" Missing vector['comp'] for vector '" + vector["name"] + "'")

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
        logging.error(" Unknown vector class '" + myClass + "' "
                       "for vector '" + vector["name"] + "'.")

    # Vector for Unit Cell length parameters (a,b,c):
    uuid_vector = tools.getUUID(self.uuidDB, myClass, vector["name"])
    output.append([uuid_vector, "Instance", myClass, "", "", ""])

    if "UnitCellLatticeVector" == myClass:
      if "label" in keys:
        output.append([self.ontoPrefix + "hasLabel", "Data Property",
                         uuid_vector, "", vector["label"], "string"])

      else:
        logging.warning(" Label is not defined for '" + vector["name"] +
                         "' of class '" + myClass + "'.")


    if   isinstance(vector["comp"], dict):
      comp_keys = vector["comp"].keys()  # comp_keys means 'KEYS of the COMPonents'.
      for ck in comp_keys:
        uuid_comp = tools.getUUID(self.uuidDB, "VectorComponent", vector["name"] + "_comp_" + ck)
        output.append([uuid_comp, "Instance", "VectorComponent", "", "", ""])

        output.append([uuid_vector, "Instance", uuid_comp,
                         self.ontoPrefix + "hasComponent", "", ""])

        output.append([self.ontoPrefix + "hasLabel", "Data Property",
                         uuid_comp, "", ck, "string"])

        output.append([self.ontoPrefix + "hasValue", "Data Property",
                         uuid_comp, "", vector["comp"][ck]["value"], "rdfs:Literal"])

        if "unit" in list(vector["comp"][ck].keys()):
          unit = vector["comp"][ck]["unit"]
          if not unit.startswith("http://www.ontology-of-units-of-measure.org/resource/om-2/"):
            logging.warning(" Possibly wrong unit '" + unit + "' in vector '" +
                             vector["name"] + "'. " +
                             "Expecting an instance of OM-2 ontology class Unit.")

          output.append([uuid_comp, "Instance", unit,
                         "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                          "", ""])


    if "unit" in keys:
#"http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom"
      unit = vector["unit"]
      if not unit.startswith("http://www.ontology-of-units-of-measure.org/resource/om-2/"):
        logging.warning(" Possibly wrong unit '" + unit + "' in vector '" +
                         vector["name"] + "'. " +
                         "Expecting an instance of OM-2 ontology class Unit.")

      output.append([uuid_vector, "Instance", unit,
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", ""])

    elif isinstance(vector["comp"], list):
      logging.error(" Not implemented yet list component 2222222 ")

    else:
      logging.error(" Unknown type of vector['comp'] = '" + str(type(vector["comp"])) +
                     "' in vector '" + vector["name"] + "'.")

    #print("==============================")
    return output
    pass # getArrVector()
    '''

    def arrInit(self, ontology):
        output = []
        #baseName = self.ontoBase # + "-" + zeoname

        if "zeolite" == ontology:
            tbox = "http://www.theworldavatar.com/kg/ontozeolite/OntoZeolite.owl"
            abox = "http://www.theworldavatar.com/kg/ontozeolite"
        else:
            logging.error(" Unknown ontology name '%s' in arrInit().", ontology)

        output += get_csv_init(self.ontoBase, tbox, abox)

        #########################
        # Each Unit is a common instance for all measures, defined only once:
        #output += ocdt.omInitUnits()
        # Example of use:
        # ocdt.omSetUnit(uuid_volume, "cubicAngstrom")

        # Initialization of OntoSpecies elements:
        #output += ocos.osInitElements()
        #output += ocos.osInitCompounds()

        return output
        # === end of CsvMaker.arrInit()


    def arrTiles(self, subject, predicate, zeoname):
        """
        subject   - Is the full hame of instance of class CrystalInformation,
                    which contains this TiledStructure class.
        predicate - Is the Object Property linking the Subject and the current TiledStructure.
                    Typically is should be equal to "hasTiledStructure".
        """

        if subject.find("CristalInformation") < 0 and subject.find("CIF") < 0:
            logging.warning(" Subject in arrTiles() is '%s', expecting the" +
                            " name to contain 'TiledStructure'.", subject)

        if predicate.find("hasTiledStructure") < 0:
            logging.warning(" Predicate in arrTiles() is '%s'," +
                            " but expecting 'hasTiledStructure'.", predicate)

        output = []

        path   = os.path.join(self.inputDir, "Tile-signature-2023.csv")
        dataIn = tools.readCsv(path)

        data = tilesignature.getDataByCode(dataIn, zeoname)
        #print("tiles for zeoname:", zeoname, ":", data, type(data))

        if data is None:
            logging.warning(" No data available for tile '%s'.", zeoname)
            return None

        uuid_tstructure = tools.getUUID(self.uuidDB.uuidDB,
                                        "TiledStructure", "TiledStructure_" + zeoname)
        output.append([uuid_tstructure, "Instance", "TiledStructure", "", "", ""])
        output.append([subject, "Instance", uuid_tstructure, predicate, "", ""])

        output.append([self.crystOntoPrefix + "hasTileSignature", "Data Property",
                       uuid_tstructure, "", data[2].strip(' "'), "string"])

        ### Begin of transitivity
        uuid_tile_trans = tools.getUUID(self.uuidDB.uuidDB, "Transitivity",
                                        "TileNumber_" + zeoname + "_transitivity")
        output.append([uuid_tile_trans, "Instance", "Transitivity",
                       "", "", ""])

        uuid_tile_transP = tools.getUUID(self.uuidDB.uuidDB, "VectorComponent",
                                         "TileNumber_" + zeoname + "_transitivityP")
        output.append([uuid_tile_transP, "Instance", "VectorComponent",
                       "", "", ""])

        uuid_tile_transQ = tools.getUUID(self.uuidDB.uuidDB, "VectorComponent",
                                         "TileNumber_" + zeoname + "_transitivityQ")
        output.append([uuid_tile_transQ, "Instance", "VectorComponent",
                       "", "", ""])

        uuid_tile_transR = tools.getUUID(self.uuidDB.uuidDB, "VectorComponent",
                                         "TileNumber_" + zeoname + "_transitivityR")
        output.append([uuid_tile_transR, "Instance", "VectorComponent",
                       "", "", ""])

        uuid_tile_transS = tools.getUUID(self.uuidDB.uuidDB, "VectorComponent",
                                         "TileNumber_" + zeoname + "_transitivityS")
        output.append([uuid_tile_transS, "Instance", "VectorComponent",
                       "", "", ""])

        output.append([uuid_tstructure, "Instance", uuid_tile_trans,
                       self.crystOntoPrefix + "hasTransitivity", "", ""])

        output.append([uuid_tile_trans, "Instance", uuid_tile_transP,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        output.append([uuid_tile_trans, "Instance", uuid_tile_transQ,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        output.append([uuid_tile_trans, "Instance", uuid_tile_transR,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        output.append([uuid_tile_trans, "Instance", uuid_tile_transS,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        trans = self.getTransitivity(data[1])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       uuid_tile_transP, "", "p", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       uuid_tile_transP, "", trans[0], "integer"])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       uuid_tile_transQ, "", "q", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       uuid_tile_transQ, "", trans[1], "integer"])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       uuid_tile_transR, "", "r", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       uuid_tile_transR, "", trans[2], "integer"])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       uuid_tile_transS, "", "s", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       uuid_tile_transS, "", trans[3], "integer"])
    ### End of transitivity

        cages = tilesignature.cellToCages(data[2])
        signN = data[6].split("+")
        codeN = data[7].split("+")
        vertN = data[9].split("+")
        edgeN = data[10].split("+")
        faceN = data[11].split("+")

        #print("faces =", faceN)
        for ic, cage in enumerate(cages):
            uuid_tile     = tools.getUUID(self.uuidDB.uuidDB, "Tile", 
                                          "Tile_" + zeoname + "_cage" + str(ic+1))
            uuid_tile_num = tools.getUUID(self.uuidDB.uuidDB, "TileNumber",
                                          "TileNumber_" + zeoname + "_cage" + str(ic+1))

            if ic >= len(codeN):
                logging.error("Too big ic in tile signature, skipping it... FIXME")
                continue

            output.append([uuid_tile, "Instance", "Tile",
                           "", "", ""])

            output.append([uuid_tstructure, "Instance", uuid_tile,
                           self.crystOntoPrefix + "hasTile", "", ""])

            output.append([self.crystOntoPrefix + "hasTileCode", "Data Property",
                           uuid_tile, "UNDEF", codeN[ic], "string"])

            output.append([self.crystOntoPrefix + "hasNumberOfFaces", "Data Property",
                           uuid_tile, "", faceN[ic], "integer"])

            output.append([self.crystOntoPrefix + "hasNumberOfEdges", "Data Property",
                           uuid_tile, "", edgeN[ic], "integer"])

            output.append([self.crystOntoPrefix + "hasNumberOfVertices", "Data Property",
                           uuid_tile, "", vertN[ic], "integer"])

            # TODO
            #output.append([self.ontoPrefix + "hasTileSymmetry", "Data Property",
            #                                   uuid_tile, "", symmN[ic], "string"])

            output.append([self.crystOntoPrefix + "hasTileSignature", "Data Property",
                           uuid_tile, "", signN[ic], "string"])

            output.append([uuid_tile_num, "Instance", "TileNumber",
                           "", "", ""])

            output.append([uuid_tstructure, "Instance", uuid_tile_num,
                           self.crystOntoPrefix + "hasTileNumber", "", ""])

            output.append([uuid_tile_num, "Instance", uuid_tile,
                           self.crystOntoPrefix + "isNumberOfTiles", "", ""])

            output.append([self.crystOntoPrefix + "hasValue", "Data Property",
                           #uuid_tile_num, "", cage[0], ""])
                           uuid_tile_num, "", cage[0], "integer"])
                           #uuid_tile_num, "", cage[0],
                           #"http://www.w3.org/2001/XMLSchema#integer"])


            faces = tilesignature.cageToFaces(cage[1])
            signNshort = signN[ic].replace("[", "").replace("]", "").strip()
            signF = signNshort.split(".")
            for f, face in enumerate(faces):
                #print("face = ", face)

                suffix = "_" + zeoname + "_cage" + str(ic+1) + "_face" + str(f+1)
                uuid_tile_face     = tools.getUUID(self.uuidDB.uuidDB, "TileFace",
                                                   "TileFace"       + suffix)
                uuid_tile_face_num = tools.getUUID(self.uuidDB.uuidDB, "TileFaceNumber",
                                                   "TileFaceNumber" + suffix)

                output.append([uuid_tile_face,     "Instance", "TileFace",
                             "" , "", ""])

                output.append([uuid_tile,   "Instance", uuid_tile_face,
                             self.crystOntoPrefix + "hasTileFace" , "", ""])

                output.append([uuid_tile_face_num, "Instance", "TileFaceNumber",
                             "" , "", ""])

                output.append([uuid_tile, "Instance", uuid_tile_face_num,
                             self.crystOntoPrefix + "hasTileFaceNumber" , "", ""])

                output.append([uuid_tile_face_num, "Instance", uuid_tile_face,
                             self.crystOntoPrefix + "isNumberOfTileFaces" , "", ""])

                output.append([self.crystOntoPrefix + "hasValue", "Data Property",
                                  #uuid_tile_face_num, "", face[1], ""])
                                  uuid_tile_face_num, "", face[1], "integer"])
                                  #uuid_tile_face_num, "", face[1],
                                  #"http://www.w3.org/2001/XMLSchema#integer"])

                output.append([self.crystOntoPrefix + "hasNumberOfEdges", "Data Property",
                                  #uuid_tile_face, "", int(face[0]), ""])
                                  uuid_tile_face, "", int(face[0]), "integer"])
                                  #uuid_tile_face, "", int(face[0]),
                                  #"http://www.w3.org/2001/XMLSchema#integer"])

                if f < len(signF):  # FIXME why f here can exceed len(signF)?
                    output.append([self.crystOntoPrefix + "hasFaceCode", "Data Property",
                                   uuid_tile_face, "", signF[f], "string"])
                               #uuid_tile_face, "", "UNDEF", "string"])
                               #uuid_tile_face, "", str("UNKNOWN"), "string"])

                # TODO add face code (?), like 14a, 14b, etc
                #output.append([self.ontoBase + "#hasFaceCode", "Data Property",
                #                               uuid_tile_face, "", int(cell[0]), "string"])

        return output
        # === end of CsvMaker.arrTiles()

    def getTransitivity(self, strValue):
        if not isinstance(strValue, str):
            logging.error(" input must be string")
            return None

        # print("In getTransitivity(): strValue =", strValue)
        short = strValue.strip()
        if short[0] != "[" or short[-1] != "]":
            logging.error(" Transitivity must be in [] brackets")

        trans = [None]*4
        i = 1   # position of character in the string
        #it = 0  # iterator for transitivity vector. 0 to 3.
        for it in range(4):
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
            #print("it = ", it, ":", trans, ", i =", i)
        #print(trans)
        return trans
        # === end of CsvMaker.getTransitivity()

    def arrSpectrum(self, subject, predicate, zeoname):
        """
        subject   -
        predicate -
        """
        if subject.find("CrystalInformation") < 0 and subject.find("CIF") < 0:
            logging.warning(" XRDSpectrum expects a subject 'CrystalInformation'," +
                            " but got '%s'.", subject)
        if predicate.find("hasXRDSpectrum") < 0:
            logging.warning(" Spectrum expects a predicate 'hasXRDSpectrum'," +
                            " but got '%s'.", predicate)

        output = []

        xrdFile = "xrd-ed5.csv"
        xrdData = tools.readCsv(xrdFile)
        #print(xrdData[:2])

        nSpectra = 0
        xrdHead = xrdData[0]
        xrdData = xrdData[1:]
        for xrd in xrdData:

            #print("Framework:", xrd[2])
            if zeoname == xrd[2]:
                nSpectra += 1

                uuid_xrd = tools.getUUID(self.uuidDB.uuidDB, "XRDSpectrum",
                           "XRDSpectrum" + str(nSpectra) +"_" + zeoname)
                output.append([uuid_xrd, "Instance", "XRDSpectrum", "", "", ""])

                output.append([subject, "Instance", uuid_xrd, predicate, "", ""])


                #print(zeoname, "=====>", xrd[5])
                output += self.loadXRDPeaks(uuid_xrd,
                                self.crystOntoPrefix + "hasCharacteristicPeak",
                                zeoname + str(nSpectra), xrd[5])

                if xrd[6] != "" and xrd[6] is not None:
                    print("TODO Add DOI ")

                break
        #if z in xrdData:

        return output
        # === end of CsvMaker.arrSpectrum()

    def loadXRDPeaks(self, subject, predicate, zeoname, filename):
        """
        See also
        https://stackoverflow.com/questions/28173128/find-local-maximum-of-data-files-in-gnuplot/56606820#56606820
        """
        if subject.find("XRDSpectrum") < 0:
            logging.warning(" CharacteristicPeak expects a subject 'XRDSpectrum', " +
                            "but got '%s'.", subject)
        if predicate.find("hasCharacteristicPeak") < 0:
            logging.warning(" Spectrum expects a predicate 'CharacteristicPeak', " +
                            "but got '%s'.", predicate)

        output = []

        if not os.path.isfile(filename):
            logging.error(" Missing XRD peaks file '%s' for %s",
                          filename, zeoname)
            return output

        #f = open(filename, "r", encoding="utf-8")
        with open(filename, "r", encoding="utf-8") as f:
            for il,line in enumerate(f):
                words = line.strip().split()
                #print(il, "=>", line.strip(), "=>", words)

                uuid_peak = tools.getUUID(self.uuidDB.uuidDB, "CharacteristicPeak",
                            "CharacteristicPeak_" + zeoname + "_peak_" + str(il+1))
                output.append([uuid_peak, "Instance", "CharacteristicPeak", "", "", ""])

                output.append([subject, "Instance", uuid_peak, predicate, "", ""])

                output.append([self.crystOntoPrefix + "hasRelativeIntensity",
                               "Data Property", uuid_peak, "", words[6], "decimal"])

                output.append([self.crystOntoPrefix + "isSimulated", "Data Property",
                               uuid_peak, "", True, "boolean"])

                miller = ocdt.OntoVector(
                           class_name = "MillerIndices",
                           item_name  = "MillerIndices_" + zeoname + "_peak_" + str(il+1),
                           uuidDB = self.uuidDB,
                           unit      = "om:dimensionOne")

                miller.addComponent(label = "h", value = str(words[0]))
                miller.addComponent(label = "k", value = str(words[1]))
                miller.addComponent(label = "l", value = str(words[2]))
                output += miller.get_csv_arr(uuid_peak, self.crystOntoPrefix + "hasMillerIndices")

                #uuid_peak = tools.getUUID(self.uuidDB, "CharacteristicPeak",
            #            "CharacteristicPeak_" + zeoname + "_peak_" + str(il+1))
                #output.append([uuid_peak, "Instance", "CharacteristicPeak", "", "", ""])
                #output.append([subject, "Instance", uuid_peak, predicate, "", ""])

                output.append([self.crystOntoPrefix + "hasTwoThetaPosition", "Data Property",
                              uuid_peak, "", words[3], "decimal"])

                #output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                #                   uuid_peak, "", self.value["label"], "string"])

        return output
        # === end of CsvMaker.loadXRDPeaks()

    '''
  # TODO delete this function from this class:
  def splitErrorBarLoop(self, line, headers, file_line):
    """
    Remove error bars from the CIF file and save the result to a temporary file
    """
    #print("Running error bar loop", line)
    #words = line.split()
    nBracket = 0
    words = tools.strSplit(line)
    #words = tools.strSplit(line.relpace("\t", " ")) # Some CIFs have a tab in a line
    #print (">>>>>>>>>>> ", words)

    if len(words) == 0:
      logging.error(" Empty line " + file_line)
      return line, 0

    if len(words) != len(headers):
      logging.error(" Number of elements on line is different from the " +
                     "definition of the loop: " + str(len(words)) +" vs " +
                     str(len(headers)) + ": " + str(words) + " vs " + str(headers) +
                     " " + file_line + ".")
      return line, 0

    lineNew = line
    #print(self.entriesWithUncertainties)
    #for i in range(len(headers)):
    for ih, h in enumerate(headers):
      #print("=== '" + h + "' ===")
      if h in self.entriesWithUncertainties:
        #print("   need to process")
        #logging.warning(" Found one of the entries in a loop")

        vOut, eOut = self.splitErrorBar(words[ih].strip(), file_line)
        if eOut != "":
          nBracket += 1
          #self._setValueAndError(cifName, h, vOut, eOut)

        pos = line.find(words[ih])
        lineNew = lineNew.replace(words[ih], vOut)

    #print("lineNew =", lineNew)
    return lineNew, nBracket
    pass # CsvMaker.splitErrorBarLoop()
     '''

    # TODO detele this function from this class:
    def _setValueAndError(self, cifName, entry, value, error):
        """
        Setting the value and error values to the internal parameters.

        """

        self.unitCellLengths = ocdt.OntoVector(uuidDB = self.uuidDB,
                                               class_name = "UnitCellLengths",
                                               item_name  = "UnitCellLengths_" + cifName,
                                               unit  = "om:angstrom")

        self.unitCellAngles = ocdt.OntoVector(uuidDB = self.uuidDB,
                                              class_name = "UnitCellAngles",
                                              item_name  = "UnitCellAngles_" + cifName,
                                              unit  = "om:degree")

        #logging.warning("_setValueAndError(): " + "cifName = '%s', " +
        #                "entry = '%s', value = '%s', error = '%s'.",
        #                str(cifName), str(entry), str(value), str(error))

        if "_cell_length_a" == entry:
            self.unitCellLengths.addComponent(label = "a",
                                              value = value, error = error)

        elif "_cell_length_b" == entry:
            self.unitCellLengths.addComponent(label = "b",
                                              value = value, error = error)

        elif "_cell_length_c" == entry:
            self.unitCellLengths.addComponent(label = "c",
                                              value = value, error = error)

        elif "_cell_angle_alpha" == entry:
            self.unitCellAngles.addComponent(label = "alpha",
                                             value = value, error = error)

        elif "_cell_angle_beta" == entry:
            self.unitCellAngles.addComponent(label = "beta",
                                             value = value, error = error)

        elif "_cell_angle_gamma" == entry:
            self.unitCellAngles.addComponent(label = "gamma",
                                             value = value, error = error)

        else:
            logging.error(" Unknown entry to store data with error: '%s'.",
                          entry)

        # === end of CsvMaker._setValueAndError()

    '''
    # TODO delete this function from here:
    def splitStr(self, value):
        """
        Function splits a string of a form "12.345(6)" into two strings: "12.345" and "0.006".
        """

        pos1 = value.find("(")
        pos2 = value.find(")")
        vOut = value[:pos1] + value[pos2+1:]
        eOut = value[pos1+1:pos2]

        '' ' This sometimes return value like 0.007000000001, last digit round effect
        n = len(eOut)
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
        eOut = str(float(factor) * float(eOut))
        '' '

        #iv = len(vOut) - 1
        #ie = len(eOut) - 1
        ie = 1
        factor = []
        for ix,x in enumerate(vOut[::-1]):
            if "." == x:
                factor.insert(0, ".")
            elif ie <= len(eOut):
                factor.insert(0, eOut[-ie])
                ie += 1
            else:
                factor.insert(0, "0")
                ie += 1

        #print(factor)
        factor = "".join(factor)
        if factor.find(".") < 0:
            eOut = str(int(factor))
        else:
            eOut = str(float(factor))
        #for i in range(len(eOut)):
          #vOut[-i-1] = eOut[-i-1]
        return vOut, eOut
        # === end of CsvMaker.splitStr()
    '''

    def splitErrorBar(self, value, file_line):
        # TODO delete this function from here.

        #print("Starting splitErrorBar() ================== ")
        vOut = value
        eOut = ""

        if not isinstance(value, str):
            logging.error(" Impossible error: not a string %s, %s.",
                          str(value), file_line)
            return vOut,eOut

        pos1 = value.find("(")
        pos2 = value.find(")")
        if pos1 < 0 and pos2 < 0:
            #logging.info(" Brackets are not detected. " + file_line)
            pass
        elif pos1 >= 0 and pos2 >= 0 and pos1 < pos2:
            #print("pos1 = ", pos1, " pos2 = ", pos2)
            #vOut,eOut = self.splitStr(value)
            vOut, eOut = splitStr(value)
        else:
            logging.error(" Something is wrong with brackets: '%s', %s",
                          value, file_line)

        #print("value, vOut, eOut =", value, vOut, eOut)
        return vOut, eOut
        # === end of CsvMaker.splitErrorBar()

    def readWithUncertainties(self, fileIn, cifName,
                              lineFr=None, lineTo=None,
                              fileOut="", save=False):
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

        self.entriesWithUncertainties = ["_cell_length_a", "_cell_length_b", "_cell_length_c",
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

        if not os.path.isfile(fileIn):
            logging.error(" Input file '%s' does not exist in cleanCif().",
                          fileIn)
            return -1

        fIn = open(fileIn, encoding="utf-8")

        if len(fileOut) > 0:
            fOut = open(fileOut, "w", encoding="utf8")

        inLoop = False
        countBrackets = 0

        for il, line in enumerate(fIn):
            file_line = "In file '" + fileIn + "' line " + str(il+1)
              #print(file_line)

            if il >= 20:
                logging.debug(" Break after 75 lines for testing, %s.",
                              file_line)
                #break

            pos1 = line.find("#")
            pos2 = line.find(";")
            if   0 == pos1:
                logging.info("Found a comment in string '%s', %s.",
                             line.strip(), file_line)
                #short = line[:pos] + "\n"
                if len(fileOut) > 0:
                    fOut.write(line)
                continue

            elif 0 == pos2:
                logging.info(" Found a comment in string '%s', %s.",
                             line.strip(), file_line)
                #short = line[:pos] + "\n"
                if len(fileOut) > 0:
                    fOut.write(line)
                continue

            elif pos1 > 0 or pos2 > 0:
                logging.warning(" Comment starts not from beginning" +
                                " of the line: '%s', %s.",
                                line.strip(), file_line
                               #+ " This is not supported by readWithUncertainties()."
            )
                if len(fileOut) > 0:
                    fOut.write(line)
                continue

            #words = line.strip().split()
            words = tools.strSplit(line)
            #print("on line", il, "words:", words)

            if len(words) == 0:
                #logging.info(" Empty string " + file_line)
                if len(fileOut) > 0:
                    fOut.write(line)
                inLoop = False
                continue

            elif "loop_" == words[0].strip():
                #logging.info(" Found a 'loop_' option")
                inLoop = True
                inLoopHead = True
                inLoopBody = False
                self.loopHeaders = []
                if len(fileOut) > 0:
                    #fOut.write("added 'loop_': " + line)
                    fOut.write(line)
                continue

            elif inLoop:
                #logging.info(" In inLoop option")
                #print("Inside the loop")

                if inLoopHead:
                    #print("Inside the loop head")
                    if "_" == words[0][0]:
                        self.loopHeaders.append(words[0].strip())
                        if len(fileOut) > 0:
                            fOut.write(line)
                    else:
                        inLoopHead = False
                        inLoopBody = True

                if inLoopBody:
                    if "_" == words[0][0]:
                        inLoop = False
                        #logging.info(" inLoop is set to False")
                        if len(fileOut) > 0:
                            fOut.write(line)
                        #self.loopHeaders = []

                    else:
                        lineNew, nbrac = self.splitErrorBarLoop(line, self.loopHeaders, file_line)
                        countBrackets += nbrac
                        #print("after splitErrorBarLoop():", lineNew, nbrac)
                        if len(fileOut) > 0:
                            fOut.write(lineNew)
                        continue

                    pass

        #print("headers =", self.loopHeaders)

            elif len(words) > 2:
                #logging.info(" Length of string = " + str(len(words)) + " " + file_line)
                if len(fileOut) > 0:
                    fOut.write(line)

            elif "_" == words[0][0]:
                #print("Checking property", words[0], "is it in list of known?", words[0] in self.entriesWithUncertainties)
                if inLoop is False:
                    if len(words) == 1:
                        #logging.info(" Only 1 entry in '" + line.strip() + "' " + file_line + ". I skip this case.")
                        if len(fileOut) > 0:
                            fOut.write(line)
                        continue
                    elif len(words) > 2:
                        #logging.info(" More than 2 entries in '" + line.strip() + "' " + file_line + ". I skip this case.")
                        if len(fileOut) > 0:
                            fOut.write(line)
                        continue

                    elif words[0] in self.entriesWithUncertainties:
                        logging.info(" Found one of the entries: %s.", words[0])
                        vOut, eOut = self.splitErrorBar(words[1], file_line)
                        if "" != eOut:
                            countBrackets += 1
                            self._setValueAndError(cifName, words[0], vOut, eOut)

                        #pos = line.find(words[1])

                        newLine = line.replace(words[1], vOut)
                        if len(fileOut) > 0:
                            fOut.write(newLine)
                            #fOut.write(line[:pos] + vOut + "\n")
                        continue

                    elif words[0] in self.cifStandard:
                        if len(fileOut) > 0:
                            fOut.write(line)
                    else:
                        logging.warning(" Unknown situation. Line = '%s', %s.",
                                        line.strip(), file_line)
                        if len(fileOut) > 0:
                            fOut.write(line)
                else:
                    if len(fileOut) > 0:
                        fOut.write(line)

            elif len(words) == 2:
                #logging.warning(" Length of string = " + str(len(words)) + " " + file_line)
                if len(fileOut) > 0:
                    fOut.write(line)

            else:
                #logging.warning(" default else option.")
                if len(fileOut) > 0:
                    fOut.write(line)

        fIn.close()
        if len(fileOut) > 0:
            fOut.close()

        #print("Number of brackets =", countBrackets)
        return countBrackets
        # === end of CsvMaker.readWithUncertainties()

    def makeCsvs(self):
        err_count = 0

        self.prepare()   # <- self.zeoList is defined inside

        # Load all general information about zeolitic materials:
        #zeoData = ontozeolite.OntoZeolite(#itemName = z, className = "Zeolite",
        #                                  uuidDB = self.uuidDB)
        #zeoData.load_zeolites()

        zeoDataBase = zeolite_db.ZeoliteDB(uuidDB = self.uuidDB)
        #zeoDataBase.load("zeolites_merged.json")
        #zeoDataBase.load("a_final_3.json")
        zeoDataBase.load("a_final_species.json")

        #print("Number of zeoData in csv_maker =", len(self.zeoList))
        #print(self.zeoList)
        t_start = datetime.datetime.now()
        # FIXME move back all iems
        #for iz, z in enumerate(["MFS"]):
        #for iz, z in enumerate(["MWW"]):
        #for iz, z in enumerate(["DFO"]):
        #for iz, z in enumerate([self.zeoList[3]]):
        #for iz, z in enumerate(self.zeoList[0:10]):
        #for iz, z in enumerate(["ZON"]):
        #for iz, z in enumerate(["ABW"]):
        #for iz, z in enumerate(["FAU"]):
        #for iz, z in enumerate(["-CHI"]):
        #for iz, z in enumerate(["VFI"]):
        for iz, z in enumerate(self.zeoList):
            #print("In zeolist z =", z)

            # Each framework is saved in a separate arr (and separate file):
            arr = []

            # The header (tbox and abox description):
            arr += self.arrInit("zeolite")

            #uuid_zeoframe = tools.getUUID(self.uuidDB.uuidDB,
            #                              "ZeoliteFramework", "Zeolite_" + z)

            #arr.append([uuid_zeoframe, "Instance", "ZeoliteFramework", "", "", ""])

            #arr.append([self.zeoOntoPrefix + "hasZeoliteCode", "Data Property",
            #              uuid_zeoframe, "", z.strip(' "'), "string"])

            # Description of the framework:
            #arr += zeoData.getCsvArrFramework(z, "", "")
            arr += zeoDataBase.get_csv_arr_framework(z)

            # get the framework from the zeoData.
            # This is necessary, because OntoCrystal is not a separate module.
            # Otherwise I would simply call Crystal information from the
            # OntoZeolite class. TODO
            #uuid_zeoframe = zeoData.get_framework_UUID(z)
            uuid_zeoframe = zeoDataBase.get_framework_UUID(z)

            #logging.warning(" For debugging no CIF data")
            # CIF information for the given framework:
            framework_cif = crystalinfo.CrystalInfo(uuidDB = self.uuidDB)

            # uuuuuuu
            path = os.path.join("CIF", zeolist.zeoCodeToCode3(z).upper() + ".cif")

            arr += framework_cif.get_csv_arr_from_cif(path, z, uuid_zeoframe)

            uuid_cif = framework_cif.get_uuid()
            if uuid_cif is None:
                print("Error: uuid_cif is None")

            #print("starting arrTile for", z)
            tmp = self.arrTiles(uuid_cif, self.crystOntoPrefix + "hasTiledStructure", z)
            if tmp is None:
                logging.warning(" Missing Tiled Structure information! for %s", z)
            else:
                # uuuuuuuuu
                arr += tmp
                pass

            tmp = self.arrSpectrum(uuid_cif, self.crystOntoPrefix + "hasXRDSpectrum", z)
            if tmp is None:
                logging.warning(" Missing Spectrum information! for %s", z)
            else:
                # uuuuuuuuu
                arr += tmp
                pass
            #arr +=[[">>>>>>>>>>", "After spectrum", z]]
            #arr +=[[">>>>>>>>>>", "Number of material:", len(zeoDataBase.get_framework_materials(z))]]
            #print([">>>>>>>>>>", "Number of material:", len(zeoDataBase.get_framework_materials(z))])
            #1/0
            # FIXME

            # To add Citation information, or is it loaded in get_csv_arr_from_cif ?

            # Description of all the zeolites for this framework:
            #for cif_line, ext_line in zeoData.getMaterialIDs(z):
            #for cif_line, mat_line in zeoData.getMaterialIDs(z):
            for zeolite in zeoDataBase.get_framework_materials(z):
                # getMaterialIDs return a list of tuples: cif and material

                #print("cif_line =", cif_line)
                #print("mat_line =", mat_line)
                #arr += zeoData.get_csv_arr_material(cif_line, mat_line, uuid_zeoframe,
                #                                    self.zeoOntoPrefix + "hasZeoliticMaterial")
                arr += zeolite.get_csv_arr_material(uuid_zeoframe,
                                                    self.zeoOntoPrefix + "hasZeoliticMaterial")

                #print(zeolite.data)
                #1/0
                #arr +=[[">>>>>>>>>>", "After csv_arr_material", uuid_zeoframe, zeolite.data["safe_name"]]]
                #for line in arr:
                #    print(line)

                #uuid_zeolite = zeoData.get_material_UUID(cif_line, mat_line)
                uuid_zeolite = zeolite.get_material_UUID()

                # CIF if exists:
                #cif_path = zeoData.get_cif_file(cif_line)
                #cif_path = zeolite.data["cif_path"] #get_cif_file()
                #cif_path = os.path.join("CIF", zeolist.zeoCodeToCode3(z).upper() + ".cif")
                #material_cif = crystalinfo.CrystalInfo(uuidDB=self.uuidDB)

                logging.warning(" FIXME: no CIF data ")
                paths = []

                if "cif_path" in zeolite.data:
                    paths = zeolite.data["cif_path"]
                    if isinstance(paths, str):
                        paths = [paths]
                    if isinstance(paths, list):
                        pass
                    else:
                        logging.error("'cif_path' must be a string or list of str: %s",
                                      str(zeolite.data["cif_file"]))
 
                if "cif_file" in zeolite.data:
                    tmp = zeolite.data["cif_file"]
                    if isinstance(tmp, str):
                        tmp = [tmp]

                    if isinstance(tmp, list):
                        for cif_path in tmp:
                            if cif_path not in paths:
                                paths.append(cif_path)
                    else:
                        logging.error("'cif_file' must be a string or list of str: %s",
                                      str(zeolite.data["cif_file"]))

                for cif_path in paths:
                    print("-------------> starting CIF", cif_path, "for", zeolite.get_iri())
                    try:
                        material_cif = crystalinfo.CrystalInfo(uuidDB=self.uuidDB)
                        # uuuuuuuuu
                        # arr += self.get_csv_arr_from_cif(cif_path, cif_line[1], uuid_zeolite)
                        #safe_name = zeolite.data["safe_name"]
                        #print("ccc")
                        safe_name = zeolite.get_iri()
                        #arr += material_cif.get_csv_arr_from_cif(cif_path, safe_name, uuid_zeolite)
                        #print("bbb")
                        arr += material_cif.get_csv_arr_from_cif(cif_path, safe_name, safe_name)
                        #print("aaa")
                    except :
                        logging.error("=================================================")
                        logging.error(" Failed to read data from '%s' cif file...", cif_path)
                        logging.error("=================================================")
                        with open("failed_cif_files.txt", "a", encoding="utf-8") as fp:
                            fp.write(cif_path + "\n")

                    #print( safe_name, uuid_zeolite)
                    #1/0
                else:
                    print("    Not found CIF in zeolite", zeolite.data["safe_name"], zeolite.data["uuid"])

                # Description of the recipe for the given zeolite (if exists):
                logging.warning(" FIXME: no recipe data")
                #arr += zeoData.get_csv_arr_recipe(cif_line, uuid_zeolite, "")

                # Description of the chemical constituent for the given zeolite:
                logging.warning(" FIXME: no constituent data")
                #arr += zeoData.getCsvArrConstituent(cif_line, uuid_zeolite, "")
                arr += zeoDataBase.getCsvArrConstituent(uuid_zeolite, "predicate")

            #arr += zeoData.get_csv_material(uuid_zeoframe,
            #       ontozeolite.zeoOntoPrefix + "hasZeoliticMaterial", z)

            # Descibe topological priperties of selected zeolite
            #uuid_zeolite
            #arr += zeoData.get_csv_arr_topology(uuid_zeoframe, 
            #       ontozeolite.zeoOntoPrefix + "hasZeoliteTopology", z)
                if zeolite.is_reference_material():
                    arr += zeoDataBase.get_csv_arr_topology(uuid_zeoframe,
                           ontozeolite.zeoOntoPrefix + "hasTopologicalProperties", z)
                   #ontozeolite.zeoOntoPrefix + "hasZeoliteTopology", z)

            #tmp = zeoData.getCsvArr(uuid_cif, self.crystOntoPrefix + "has???")

            #if None == tmp:
            #    logging.warning(" Missing zeolite information!")
            #else:
            #    arr += tmp

            # Saving the framework in a file:
            #print("arr =", arr)
            #csvWrite(arr)
            path = os.path.join(self.outputDir, z + ".csv")
            tools.writeCsv(path, arr)

            if iz > 0 and iz % 10 == 0:
                self.finalize()
                t_finish = datetime.datetime.now()
                t_delta = t_finish - t_start
                t_delta = t_delta.total_seconds()
                print("Finished", iz, "compounds. Saved uuidDB in", round(t_delta,1), "sec.")
                #print(t_finish - t_start)

            #err_count += 1
            #logging.warning("Not implemented creating zeolite csv")

        self.finalize() # << Important! Saves the current list of the UUIDs

        if err_count > 0:
            logging.warning("CSV_maker: Detected %d errors.", err_count)

        # === end of CsvMaker.makeCsvs()

    # === end of class CsvMaker


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


    a = CsvMaker()

    a.settings = CommandLine()

    a.makeCsvs()

    #fileIn  = os.path.join("test", "913885.cif")
    #filesOut = a.cleanCif(fileIn)
    #print("Created", len(filesOut), "file(s) after clean-up:", filesOut)

    #input_strings = ["123450(16)", "123450(160)", "1234.5(16)", "123.45(16)", "12.345(16)"]
    #for input_string in input_strings:
    #  value, error = a.splitStr(input_string)
    #  print(f"Input: {input_string}, Value: {value}, Error: {error}")

    #input_strings = ["1 2 3 4 5", "1   2  3   4  5\n", "1 '2 3' 4 5", "1 2 3 '4   5'"]
    #for input_string in input_strings:
    #  out = tools.strSplit(input_string)
    #  print(f"Input: {input_string}, Out: {str(out)}")
    #  #print(out)


