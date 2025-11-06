"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

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
import sys
import math

import argparse
import datetime

import logging

import tools
import zeolist
import tilesignature
import crystalinfo

import ontozeolite
import zeolite_db

import ontocrystal_datatypes as ocdt

import ontocrystal_species as ocos

#logging.basicConfig(level = logging.DEBUG)
#logging.basicConfig(level = logging.INFO)
#logging.basicConfig(level = logging.WARNING)
logging.basicConfig(level = logging.ERROR)

#if len(sys.argv) > 1:
#    DATA_DIR = sys.argv[1]
#else:
if True:
    DATA_DIR = "ontozeolite"
    print("Missing command line argument in csv_maker.py, using", DATA_DIR)

# Ontology for Crystal,
#crystOntoPrefix = "https://www.theworldavatar.com/kg/ontocrystal/"

def is_http(value):
    if not isinstance(value, str):
        print(" In is_http input is not a string: '", str(value), "'.", sep="")
        return False
    if value.strip().lower().startswith("http://") or \
       value.strip().lower().startswith("https://"):
        return True
    return False

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

        self.cif_file = args.cif
        self.zeo_fr = args.fr
        self.zeo_to = args.to
        self.out_dir = args.output

        if args.cif != "":
            print("Input zeolite frameworks (i.e. CIF files) = '", args.cif,
                  "'. Accepted: 'all' or none.", sep="")
        if args.part is not None:
            print("Input part of list = '", args.part, "'.", sep="")
            print("Not implemented yet")
        if args.fr is not None or args.to is not None:
            print("Range of zeo_list: = [", args.fr, " -> ", args.to, ").", sep="")
        if args.output != "":
            print("The output directory is : = '", args.output, "'.", sep="")

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
        #self.zeoOption = ["main", "new"]
        self.zeoList = []

        self.ontoBase        = "OntoZeolite"
        self.zeoOntoPrefix   = "https://www.theworldavatar.com/kg/ontozeolite/"
        self.crystOntoPrefix = "https://www.theworldavatar.com/kg/ontocrystal/"

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
            # One can choose a custom zeolite (for debugging):
            #self.zeoList = [os.path.join("test", "913885.cif")]

        else:
            logging.error(" Invalid command line argument '%s', expect" +
                          " Framework code or 'all'", self.settings.cif_file)

        self.inputDir  = os.path.join(DATA_DIR, "zeolite", "data")

        if self.settings.out_dir == "":
            self.uuidDB = tools.UuidDB(os.path.join(DATA_DIR, "zeolite", "uuid", "default.csv"))
            self.outputDir = os.path.join(DATA_DIR, "zeolite", "csv")
        else:
            if os.path.isdir(self.settings.out_dir):
                self.uuidDB = tools.UuidDB(os.path.join(self.settings.out_dir, "default.csv"))
                self.outputDir = self.settings.out_dir
            else:
                logging.error("Invalid output folder '%s'.", self.settings.out_dir)

        if self.settings.zeo_fr is not None and self.settings.zeo_to is not None:
            self.zeoList = self.zeoList[self.settings.zeo_fr:self.settings.zeo_to]

        # === end of CsvMaker.prepare()

    def finalize(self):
        self.uuidDB.saveDB()
        # === end of CsvMaker.finalize()

    def arrInit(self, ontology):
        output = []

        if "zeolite" == ontology:
            tbox = "https://www.theworldavatar.com/kg/ontozeolite/OntoZeolite.owl"
            abox = "https://www.theworldavatar.com/kg/ontozeolite"
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
        subject   - Is the full name of instance of class CrystalInformation,
                    which contains this TiledStructure class.
        predicate - Is the Object Property linking the Subject and the current TiledStructure.
                    Typically is should be equal to "hasTiledStructure".
        """

        if subject.find("CrystalInformation") < 0 and subject.find("CIF") < 0:
            logging.warning(" Subject in arrTiles() is '%s', expecting the" +
                            " name to contain 'CrystalInformation'.", subject)

        if predicate.find("hasTiledStructure") < 0:
            logging.warning(" Predicate in arrTiles() is '%s'," +
                            " but expecting 'hasTiledStructure'.", predicate)

        output = []

        path   = os.path.join(self.inputDir, "Tile-signature-2023.csv")
        dataIn = tools.readCsv(path)

        data = tilesignature.getDataByCode(dataIn, zeoname)

        if data is None:
            logging.warning(" No data available for tile '%s'.", zeoname)
            return None

        uuid_tstructure, _ = self.uuidDB.addUUID(
                                       self.crystOntoPrefix + "TiledStructure",
                                       self.zeoOntoPrefix + "TiledStructure_" + zeoname)

        output.append([uuid_tstructure, "Instance", self.crystOntoPrefix + "TiledStructure", "", "", ""])
        output.append([subject, "Instance", uuid_tstructure, predicate, "", ""])

        output.append([self.crystOntoPrefix + "hasTileSignature", "Data Property",
                       uuid_tstructure, "", data[2].strip(' "'), "string"])

        ### Begin of transitivity
        tile_trans_iri, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "Transitivity",
                                                 self.zeoOntoPrefix + "TileNumber_" + zeoname + "_transitivity")
        output.append([tile_trans_iri, "Instance", self.crystOntoPrefix + "Transitivity",
                       "", "", ""])

        tile_trans_iri_P, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "VectorComponent",
                                                   self.zeoOntoPrefix + "TileNumber_" + zeoname + "_transitivityP")
        output.append([tile_trans_iri_P, "Instance", self.crystOntoPrefix + "VectorComponent",
                       "", "", ""])

        tile_trans_iri_Q, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "VectorComponent",
                                                  self.crystOntoPrefix + "TileNumber_" + zeoname + "_transitivityQ")
        output.append([tile_trans_iri_Q, "Instance", self.crystOntoPrefix + "VectorComponent",
                       "", "", ""])

        tile_trans_iri_R, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "VectorComponent",
                                                  self.zeoOntoPrefix + "TileNumber_" + zeoname + "_transitivityR")
        output.append([tile_trans_iri_R, "Instance", self.crystOntoPrefix + "VectorComponent",
                       "", "", ""])

        tile_trans_iri_S, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "VectorComponent",
                                         self.zeoOntoPrefix + "TileNumber_" + zeoname + "_transitivityS")
        output.append([tile_trans_iri_S, "Instance", self.crystOntoPrefix + "VectorComponent",
                       "", "", ""])

        output.append([uuid_tstructure, "Instance", tile_trans_iri,
                       self.crystOntoPrefix + "hasTransitivity", "", ""])

        output.append([tile_trans_iri, "Instance", tile_trans_iri_P,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        output.append([tile_trans_iri, "Instance", tile_trans_iri_Q,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        output.append([tile_trans_iri, "Instance", tile_trans_iri_R,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        output.append([tile_trans_iri, "Instance", tile_trans_iri_S,
                       self.crystOntoPrefix + "hasVectorComponent", "", ""])

        trans = self.getTransitivity(data[1])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       tile_trans_iri_P, "", "p", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       tile_trans_iri_P, "", trans[0], "integer"])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       tile_trans_iri_Q, "", "q", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       tile_trans_iri_Q, "", trans[1], "integer"])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       tile_trans_iri_R, "", "r", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       tile_trans_iri_R, "", trans[2], "integer"])

        output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                       tile_trans_iri_S, "", "s", "string"])
        output.append([self.crystOntoPrefix + "hasComponentValue", "Data Property",
                       tile_trans_iri_S, "", trans[3], "integer"])
    ### End of transitivity

        cages = tilesignature.cellToCages(data[2])
        signN = data[6].split("+")
        codeN = data[7].split("+")
        vertN = data[9].split("+")
        edgeN = data[10].split("+")
        faceN = data[11].split("+")

        for ic, cage in enumerate(cages):
            tile_iri, _     = self.uuidDB.addUUID(self.crystOntoPrefix + "Tile",
                                                   self.zeoOntoPrefix + "Tile_" + zeoname + "_cage" + str(ic+1))
            tile_num_iri, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "TileNumber",
                                                   self.zeoOntoPrefix + "TileNumber_" + zeoname + "_cage" + str(ic+1))

            if ic >= len(codeN):
                logging.error(" Too big ic in tile signature, skipping it... Data corrupted?")
                continue

            output.append([tile_iri, "Instance", self.crystOntoPrefix + "Tile",
                           "", "", ""])

            output.append([uuid_tstructure, "Instance", tile_iri,
                           self.crystOntoPrefix + "hasTile", "", ""])

            output.append([self.crystOntoPrefix + "hasTileCode",
                          "Data Property", tile_iri, "", codeN[ic], "string"])

            output.append([self.crystOntoPrefix + "hasNumberOfFaces",
                           "Data Property", tile_iri, "", faceN[ic], "integer"])

            output.append([self.crystOntoPrefix + "hasNumberOfEdges",
                           "Data Property", tile_iri, "", edgeN[ic], "integer"])

            output.append([self.crystOntoPrefix + "hasNumberOfVertices",
                           "Data Property", tile_iri, "", vertN[ic], "integer"])

            #output.append([self.ontoPrefix + "hasTileSymmetry", "Data Property",
            #                                   tile_iri, "", symmN[ic], "string"])

            output.append([self.crystOntoPrefix + "hasTileSignature",
                           "Data Property", tile_iri, "", signN[ic], "string"])

            output.append([tile_num_iri, "Instance",
                           self.crystOntoPrefix + "TileNumber", "", "", ""])

            output.append([uuid_tstructure, "Instance", tile_num_iri,
                           self.crystOntoPrefix + "hasTileNumber", "", ""])

            output.append([tile_num_iri, "Instance", tile_iri,
                           self.crystOntoPrefix + "isNumberOfTiles", "", ""])

            output.append([self.crystOntoPrefix + "hasValue", "Data Property",
                           tile_num_iri, "", cage[0], "integer"])

            faces = tilesignature.cageToFaces(cage[1])
            signNshort = signN[ic].replace("[", "").replace("]", "").strip()
            signF = signNshort.split(".")
            for f, face in enumerate(faces):

                suffix = "_" + zeoname + "_cage" + str(ic+1) + "_face" + str(f+1)
                tile_face_iri, _     = self.uuidDB.addUUID(self.crystOntoPrefix + "TileFace",
                                                           self.zeoOntoPrefix + "TileFace" + suffix)
                tile_face_num_iri, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "TileFaceNumber",
                                                           self.zeoOntoPrefix + "TileFaceNumber" + suffix)

                output.append([tile_face_iri, "Instance",
                               self.crystOntoPrefix + "TileFace",
                               "" , "", ""])

                output.append([tile_iri,   "Instance", tile_face_iri,
                               self.crystOntoPrefix + "hasTileFace" , "", ""])

                output.append([tile_face_num_iri, "Instance",
                               self.crystOntoPrefix + "TileFaceNumber",
                               "" , "", ""])

                output.append([tile_iri, "Instance", tile_face_num_iri,
                               self.crystOntoPrefix + "hasTileFaceNumber" , "", ""])

                output.append([tile_face_num_iri, "Instance", tile_face_iri,
                               self.crystOntoPrefix + "isNumberOfTileFaces" , "", ""])

                output.append([self.crystOntoPrefix + "hasValue", "Data Property",
                               tile_face_num_iri, "", face[1], "integer"])

                output.append([self.crystOntoPrefix + "hasNumberOfEdges", "Data Property",
                               tile_face_iri, "", int(face[0]), "integer"])

                if f < len(signF):  # FIXME why f here can exceed len(signF)?
                    output.append([self.crystOntoPrefix + "hasFaceCode", "Data Property",
                                   tile_face_iri, "", signF[f], "string"])

                # Tile face code, like 14a, 14b, etc.
                # The actual shape (coordinates of atoms) of the faces even
                # for the same tile may be different in different frameworks,
                # Blatov name them using a,b,c...
                # For now this difference is not included, as a minor information.
                # Also, the data from Blatov in incomplete because
                # new frameworks have been discovered since publication.
                #output.append([self.crystOntoPrefix + "hasFaceCode", "Data Property",
                #               tile_face_iri, "", int(cell[0]), "string"])

        return output
        # === end of CsvMaker.arrTiles()

    def getTransitivity(self, strValue):
        if not isinstance(strValue, str):
            logging.error(" input must be string")
            return None

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

        xrdFile = os.path.join(DATA_DIR, "zeolite", "data", "xrd-ed5.csv")

        if os.path.isfile(xrdFile):
            #print("data_dir =", DATA_DIR)
            #print("xrd file =", xrdFile)
            xrdData = tools.readCsv(xrdFile)
            #print(xrdData[:2])

            nSpectra = 0
            xrdHead = xrdData[0]
            xrdData = xrdData[1:]
            for xrd in xrdData:

                #print(">>>>> Framework:", zeoname, xrd[2])
                if zeoname.replace("-", "_") == xrd[2]:
                    with open("xrd-spectrum.txt", "a", encoding="utf-8") as fp:
                        fp.write(xrd[2] + "\n")

                    nSpectra += 1

                    uuid_xrd, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "XRDSpectrum",
                                                      self.zeoOntoPrefix + "XRDSpectrum" + str(nSpectra) +"_" + zeoname)
                    output.append([uuid_xrd, "Instance", self.crystOntoPrefix + "XRDSpectrum", "", "", ""])

                    output.append([subject, "Instance", uuid_xrd, predicate, "", ""])

                    filename = os.path.join("ontozeolite", "zeolite", "data", xrd[5])
                    output += self.loadXRDPeaks(uuid_xrd,
                                   self.crystOntoPrefix + "hasCharacteristicPeak",
                                   zeoname + str(nSpectra), filename)

                    if xrd[6] != "" and xrd[6] is not None:
                        print("TODO Add DOI ")

                    break
        else:
            print("File ", xrdFile, "does not exist in csv_maker.csv")

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

                uuid_peak, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "CharacteristicPeak",
                               self.zeoOntoPrefix + "CharacteristicPeak_" + zeoname + "_peak_" + str(il+1))
                output.append([uuid_peak, "Instance",
                               self.crystOntoPrefix + "CharacteristicPeak",
                               "", "", ""])

                output.append([subject, "Instance", uuid_peak, predicate, "", ""])

                output.append([self.crystOntoPrefix + "hasRelativeIntensity",
                               "Data Property", uuid_peak, "", words[6], "decimal"])

                output.append([self.crystOntoPrefix + "isSimulated", "Data Property",
                               uuid_peak, "", True, "boolean"])

                miller = ocdt.OntoVector(
                         class_name = "MillerIndices",
                         item_name  = self.zeoOntoPrefix + "MillerIndices_" + zeoname + "_peak_" + str(il+1),
                         uuidDB = self.uuidDB,
                         unit = "om:dimensionOne")

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
    '''

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
                logging.info(" Found a comment in string '%s', %s.",
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
                                line.strip(), file_line)
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

        return countBrackets
        # === end of CsvMaker.readWithUncertainties()

    def makeCsvs(self):
        err_count = 0

        self.prepare()   # <- self.zeoList is defined inside

        # Load all general information about zeolitic materials:
        zeoData = ontozeolite.OntoZeolite(uuidDB = self.uuidDB,
                                          abox_prefix=self.zeoOntoPrefix)
        zeoData.load_zeolites()

        zeoDataBase = zeolite_db.ZeoliteDB(uuidDB = self.uuidDB)
        zeoDataBase.load(os.path.join(DATA_DIR, "zeolite", # "data",
                                      "a_final_species_nodup.json"))

        t_start = datetime.datetime.now()
        for iz, z in enumerate(self.zeoList):
            # Each framework is saved in a separate arr (and separate file):
            arr = []

            # The header (tbox and abox description):
            arr += self.arrInit("zeolite")

            # Description of the framework:
            arr += zeoDataBase.get_csv_arr_framework(z)

            zeoframe_iri = zeoDataBase.get_framework_iri(z)

            # CIF information for the given framework:
            framework_cif = crystalinfo.CrystalInfo(uuidDB = self.uuidDB,
                                                    abox_prefix=self.zeoOntoPrefix)

            path = os.path.join("ontozeolite", "crystal", "data", "cifdir", z + ".cif")

            arr += framework_cif.get_csv_arr_from_cif(path, z, new_uuid=None,
                                                      subject=zeoframe_iri,
                                                      predicate=self.crystOntoPrefix + "hasCrystalInformation")
            if not os.path.isfile(path):
                print("Error! in csv_maker (framework) >> Missing CIF file:", path)

            uuid_cif = framework_cif.get_uuid()
            if uuid_cif is None:
                print("Error: uuid_cif is None")

            tmp = self.arrTiles(self.zeoOntoPrefix + uuid_cif,
                                self.crystOntoPrefix + "hasTiledStructure", z)
            if tmp is None:
                logging.warning(" Missing Tiled Structure information! for %s", z)
            else:
                arr += tmp
                pass

            tmp = self.arrSpectrum(self.zeoOntoPrefix + uuid_cif,
                                   self.crystOntoPrefix + "hasXRDSpectrum", z)
            if tmp is None or tmp == []:
                # logging.warning(" Missing Spectrum information! for %s", z)
                pass
            else:
                arr += tmp
                pass

            # Descibe topological priperties of selected zeolite
            arr += zeoDataBase.get_csv_arr_topology(zeoframe_iri,
                   ontozeolite.zeoOntoPrefix + "hasTopologicalProperties", z)

            # Description of all the zeolites for this framework:
            for zeolite in zeoDataBase.get_framework_materials(z):

                arr += zeolite.get_csv_arr_material(zeoframe_iri,
                                                    self.zeoOntoPrefix + "hasZeoliticMaterial")

                zeolite_iri = zeolite.get_material_iri()

                # Add CIF data, if exists:
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

                if len(paths) == 0:
                    print("  Not found CIF in zeolite",
                          zeolite.data["safe_name"], zeolite.data["uuid"])

                for cif_path in paths:
                    try:
                        material_cif = crystalinfo.CrystalInfo(uuidDB=self.uuidDB,
                                                               abox_prefix=self.zeoOntoPrefix)
                        safe_name = zeolite.get_iri()
                        arr += material_cif.get_csv_arr_from_cif(cif_path,
                               new_uuid=None, subject=zeolite_iri,
                               predicate=self.crystOntoPrefix + "hasCrystalInformation")
                        pass
                    except :
                        logging.error("=================================================")
                        logging.error(" Failed to read data from '%s' cif file...", cif_path)
                        logging.error("=================================================")
                        with open("failed_cif_files.txt", "a", encoding="utf-8") as fp:
                            fp.write(cif_path + "\n")

                # Description of the chemical constituent for the given zeolite:
                #logging.warning(" FIXME: no constituent data")
                #arr += zeoData.getCsvArrConstituent(cif_line, zeolite_iri, "")
                arr += zeoDataBase.getCsvArrConstituent(zeolite_iri, "predicate")

                # Description of the recipe for the given zeolite (if exists):
                if "recipe" in zeolite.data:
                    # Example of the recipe file:
                    # cif_file = os.path.join("ontozeolite", "zeolite", "data", "recipes", "p403.txt")
                    cif_file = zeolite.data["recipe"]
                    arr += zeoData.get_csv_arr_recipe(cif_file, zeolite_iri,
                                                      "", new_uuid=zeolite.get_uuid())
                else:
                    # logging.warning(" For zeolite no recipe data")
                    pass

            # Saving the framework in a file:
            path = os.path.join(self.outputDir, z + ".csv")
            tools.writeCsv(path, arr)

            if iz > 0 and iz % 10 == 0:
                self.finalize()
                t_finish = datetime.datetime.now()
                t_delta = t_finish - t_start
                t_delta = t_delta.total_seconds()
                print("Finished", iz, "compounds. Saved uuidDB in",
                      round(t_delta,1), "sec.")

        self.finalize()

        if err_count > 0:
            logging.warning(" CSV_maker: Detected %d errors.", err_count)

        # === end of CsvMaker.makeCsvs()

    # === end of class CsvMaker


if __name__ == "__main__":


    a = CsvMaker()

    a.settings = CommandLine()

    a.makeCsvs()
