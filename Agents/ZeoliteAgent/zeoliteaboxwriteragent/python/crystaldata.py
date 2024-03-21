import os
import logging

from pymatgen.core.structure import Structure, Lattice
from pymatgen.symmetry.analyzer import SpacegroupAnalyzer
import pymatgen

import ontocrystal_datatypes as ocdt

import tools

#logging.basicConfig(level = logging.DEBUG)
#logging.basicConfig(level = logging.INFO)
#logging.basicConfig(level = logging.WARNING)
logging.basicConfig(level = logging.ERROR)


entriesWithUncertainties = [
             "_cell_length_a",    "_cell_length_b",   "_cell_length_c",
             "_cell_angle_alpha", "_cell_angle_beta", "_cell_angle_gamma",
             "_cell_volume", "_cell_measurement_temperature",
             "_diffrn_ambient_temperature",
             "_atom_site_fract_x", "_atom_site_fract_y", "_atom_site_fract_z",
             "_atom_site_U_iso_or_equiv",
             "_atom_site_aniso_U_11", "_atom_site_aniso_U_22",
             "_atom_site_aniso_U_33", "_atom_site_aniso_U_23",
             "_atom_site_aniso_U_13", "_atom_site_aniso_U_12",
             "_geom_bond_distance",   "_geom_angle",
             "_refine_ls_extinction_coef"
                         ]
SPACE_GROUP_SYMBOL = {
    1: "P1", 2: "P-1", 3: "P2", 4: "P21", 5: "C2", 6: "Pm", 7: "Pc", 8: "Cm",
    9: "Cc", 10: "P2/m", 11: "P21/m", 12: "C2/m", 13: "P2/c", 14: "P21/c",
    15: "C2/c", 16: "P222", 17: "P2221", 18: "P21212", 19: "P212121",
    20: "C2221", 21: "C222", 22: "F222", 23: "I222", 24: "I212121", 25: "Pmm2",
    26: "Pmc21", 27: "Pcc2", 28: "Pma2", 29: "Pca21", 30: "Pnc2", 31: "Pmn21",
    32: "Pba2", 33: "Pna21", 34: "Pnn2", 35: "Cmm2", 36: "Cmc21", 37: "Ccc2",
    38: "Amm2", 39: "Aem2", 40: "Ama2", 41: "Aea2", 42: "Fmm2", 43: "Fdd2",
    44: "Imm2", 45: "Iba2", 46: "Ima2", 47: "Pmmm", 48: "Pnnn", 49: "Pccm",
    50: "Pban", 51: "Pmma", 52: "Pnna", 53: "Pmna", 54: "Pcca", 55: "Pbam",
    56: "Pccn", 57: "Pbcm", 58: "Pnnm", 59: "Pmmn", 60: "Pbcn", 61: "Pbca",
    62: "Pnma", 63: "Cmcm", 64: "Cmce", 65: "Cmmm", 66: "Cccm", 67: "Cmme",
    68: "Ccce", 69: "Fmmm", 70: "Fddd", 71: "Immm", 72: "Ibam", 73: "Ibca",
    74: "Imma", 75: "P4", 76: "P41", 77: "P42", 78: "P43", 79: "I4", 80: "I41",
    81: "P-4", 82: "I-4", 83: "P4/m", 84: "P42/m", 85: "P4/n", 86: "P42/n",
    87: "I4/m", 88: "I41/a", 89: "P422", 90: "P4212", 91: "P4122",
    92: "P41212", 93: "P4222", 94: "P42212", 95: "P4322", 96: "P43212",
    97: "I422", 98: "I4122", 99: "P4mm", 100: "P4bm", 101: "P42cm",
    102: "P42nm", 103: "P4cc", 104: "P4nc", 105: "P42mc", 106: "P42bc",
    107: "I4mm", 108: "I4cm", 109: "I41md", 110: "I41cd", 111: "P-42m",
    112: "P-42c", 113: "P-421m", 114: "P-421c", 115: "P-4m2", 116: "P-4c2",
    117: "P-4b2", 118: "P-4n2", 119: "I-4m2", 120: "I-4c2", 121: "I-42m",
    122: "I-42d", 123: "P4/mmm", 124: "P4/mcc", 125: "P4/nbm", 126: "P4/nnc",
    127: "P4/mbm", 128: "P4/mnc", 129: "P4/nmm", 130: "P4/ncc", 131: "P42/mmc",
    132: "P42/mcm", 133: "P42/nbc", 134: "P42/nnm", 135: "P42/mbc",
    136: "P42/mnm", 137: "P42/nmc", 138: "P42/ncm", 139: "I4/mmm",
    140: "I4/mcm", 141: "I41/amd", 142: "I41/acd", 143: "P3", 144: "P31",
    145: "P32", 146: "R3 ", 147: "P-3", 148: "R-3", 149: "P312", 150: "P321",
    151: "P3112", 152: "P3121", 153: "P3212", 154: "P3221", 155: "R32",
    156: "P3m1", 157: "P31m", 158: "P3c1", 159: "P31c", 160: "R3m", 161: "R3c",
    162: "P-31m", 163: "P-31c", 164: "P-3m1", 165: "P-3c1", 166: "R-3m",
    167: "R-3c", 168: "P6", 169: "P61", 170: "P65", 171: "P62", 172: "P64",
    173: "P63", 174: "P-6", 175: "P6/m", 176: "P63/m", 177: "P622",
    178: "P6122", 179: "P6522", 180: "P6222", 181: "P6422", 182: "P6322",
    183: "P6mm", 184: "P6cc", 185: "P63cm", 186: "P63mc", 187: "P-6m2",
    188: "P-6c2", 189: "P-62m", 190: "P-62c", 191: "P6/mmm", 192: "P6/mcc",
    193: "P63/mcm", 194: "P63/mmc", 195: "P23", 196: "F23", 197: "I23",
    198: "P213", 199: "I213", 200: "Pm-3", 201: "Pn-3", 202: "Fm-3",
    203: "Fd-3", 204: "Im-3", 205: "Pa-3", 206: "Ia-3", 207: "P432",
    208: "P4232", 209: "F432", 210: "F4132", 211: "I432", 212: "P4332",
    213: "P4132", 214: "I4132", 215: "P-43m", 216: "F-43m", 217: "I-43m",
    218: "P-43n", 219: "F-43c", 220: "I-43d", 221: "Pm-3m", 222: "Pn-3n",
    223: "Pm-3n", 224: "Pn-3m", 225: "Fm-3m", 226: "Fm-3c", 227: "Fd-3m",
    228: "Fd-3c", 229: "Im-3m", 230: "Ia-3d"}


def cleanString(line):
    """
    """
    if not isinstance(line, str):
        logging.error("Input line '%s' is not a string in cleanString()",
                      str(line))
        return str(line)
    pos = line.find("#")
    if pos < 0:
        tmp = line.strip()
    else:
        tmp = line[:pos].strip()

    return tmp

def splitErrorBar(value, file_line):
    #print("Starting splitErrorBar() ================== ")
    v_out = value
    e_out = ""

    if not isinstance(value, str):
        logging.error(" Impossible error: not a string %s %s.",
                      str(value), file_line)
        return v_out,e_out

    pos1 = value.find("(")
    pos2 = value.find(")")
    if pos1 < 0 and pos2 < 0:
        #logging.info(" Brackets are not detected. " + file_line)
        pass
    elif (pos1 >= 0) and (pos2 >= 0) and (pos1 < pos2):
        #print("pos1 = ", pos1, " pos2 = ", pos2)
        v_out, e_out = splitStr(value)
    else:
        logging.error(" Something is wrong with brackets: '%s' %s",
                      value, file_line)

    #print("value, v_out, e_out =", value, v_out, e_out)
    return v_out, e_out

def splitErrorBarLoop(line, headers, file_line):
    """
    FIXME Warning! Error bar here is only in brackets: i.e. 1.23(4).
                   Need to implement syntax: 1.23,4

    Remove error bars from the CIF file and save the result to a temporary file.
    Return a tuple:
    line made of values (no error bars), and number of removed error bars.
    """
    #print("Running error bar loop", line)
    #words = line.split()
    nBracket = 0
    words = tools.strSplit(line)
    #words = tools.strSplit(line.relpace("\t", " ")) # Some CIFs have a tab in a line
    #print (">>>>>>>>>>> ", words)

    if len(words) == 0:
        logging.error(" Empty line %s.", file_line)
        return line, 0

    if len(words) != len(headers):
        logging.error(" Number of elements on line is different from the " +
                      "definition of the loop: %d vs %d: %s vs %s %s.",
                      len(words), len(headers), str(words), str(headers),
                      file_line)
        return line, 0

    line_new = line
    #print(self.entriesWithUncertainties)
    #for i in range(len(headers)):
    for ih, h in enumerate(headers):
        #print("=== '" + h + "' ===")
        if h in entriesWithUncertainties:
            #print("   need to process")
            #logging.warning(" Found one of the entries in a loop")

            v_out, e_out = splitErrorBar(words[ih].strip(), file_line)
            if e_out != "":
                nBracket += 1
                #self._setValueAndError(cifName, h, v_out, e_out)

            #pos = line.find(words[ih])
            line_new = line_new.replace(words[ih], v_out)

    #print("line_new =", line_new)
    return line_new, nBracket
    # === end of splitErrorBarLoop()


def splitStr(value):
    """
    Function splits a string of a form "12.345(6)" into two strings: "12.345" and "0.006".
    TODO:
    Sometimes the uncertainty is displayed as 12.345,6
              (see for example ccdcfiles\10.1002_anie.200704222.cif)
    """

    pos1 = value.find("(")
    pos2 = value.find(")")
    v_out = value[:pos1] + value[pos2+1:]
    e_out = value[pos1+1:pos2]

    ''' This sometimes return value like 0.007000000001, last digit round effect
    n = len(eOut)
    v_out = v_out
    v = 0
    factor = ""
    for ix, x in enumerate(v_out):
      if ix == len(v_out) - 1:
        factor += "1"
      elif "." == x:
        factor += "."
      else:
        factor += "0"
    e_out = str(float(factor) * float(e_out))
    '''

    #iv = len(v_out) - 1
    #ie = len(e_out) - 1
    ie = 1
    factor = []
    for ix, x in enumerate(v_out[::-1]):
        if "." == x:
            factor.insert(0, ".")
        elif ie <= len(e_out):
            factor.insert(0, e_out[-ie])
            ie += 1
        else:
            factor.insert(0, "0")
            ie += 1

    #print(factor)
    factor = "".join(factor)
    if factor.find(".") < 0:
        e_out = str(int(factor))
    else:
        e_out = str(float(factor))
    #for i in range(len(e_out)):
      #v_out[-i-1] = e_out[-i-1]
    return v_out, e_out



class AtomInformation:
    """
    cifName  - is the name of the molecule, as it will be save in abox.
               Withing the molecule an atom can be addressed by the label.
    cifLabel - label of the atom within the compound (cifName).
               The unique id of the atom is cifName+_+cifLabel.
    element  - The chemical element (from cif: _atom_site_type_symbol)

    """
    __slots__ = ["uuidDB", "cifName", "cifLabel",
                 "frac", "cart", "element", "occupancy",
                 "crystOntoPrefix"]
    def __init__(self, uuidDB, compound):
        self.uuidDB    = uuidDB
        self.cifName   = compound

        self.frac      = None # save as array, converted to vector only before saving.
        self.cart      = None # save as array, converted to vector only before saving.
        self.element   = None # A string (1-2 letters), the atom name from Mendeleev table.
        self.occupancy = None # Optional.
        self.cifLabel  = None # This is directly from cif. Optional value.

        self.crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
        pass # AtomInformation.__init__()

    def setCoordFrac(self, x, y, z):
        self.frac = [x, y, z]
        pass # AtomInformation.setCoordFrac()

    def setCoordCart(self, x, y, z):
        self.cart = [x, y, z]
        pass # AtomInformation.setCoordFrac()

    def setProp(self, element = None, occupancy = None, label = None):

        if element is not None:
            if not isinstance(element, str):
                logging.warning(" Atom element is not a string: '%s'.", str(element))
            self.element = element

        if occupancy is not None:
            if not isinstance(occupancy, float) and not isinstance(occupancy, int):
                logging.warning(" Atom occupancy is not a number: '%s'.", str(occupancy))
            self.occupancy = occupancy

        if label is not None:
            if not isinstance(label, str):
                logging.warning(" Atom label is not a string: '%s'.", str(label))
            self.cifLabel = str(label)

        pass # AtomInformation.setProp()

    def getArrAtom(self, subject, predicate, label = None):
        #print("================")
        """
        subject   - Is the full hame of instance of class,
                    which contains this Vector class.
        predicate - Is the Object Property linking the Subject and the current UnitCell.
                    Typically is should be contain "has".
        """
        if subject.find("AtomicStructure") < 0:
            logging.warning(" Atom expects a subject 'AtomicStrucure', " +
                            "but got '%s'.", subject)
        if predicate.find("hasAtomSite") < 0:
            logging.warning(" Atom expects a predicate 'hasAtomSite', " +
                            "but got '%s'.", predicate)

        def to_round(value_in):
            return str(round(float(value_in), 10))

        output = []

        if self.cifLabel is not None:
            atomLabel = self.cifLabel
        elif label is not None:
            atomLabel = str(label)
        else:
            logging.warning(" Neither cifLabel, nor atom label is specified. " +
                            "I use a random number as label.")
            atomLabel = tools.getUUID_random("")


        uuid_atom,_ = self.uuidDB.addUUID("AtomSite",
                                          "Atom_" + self.cifName + "_" + atomLabel)
        #uuid_atom = tools.getUUID(self.uuidDB, "AtomSite", "Atom_" + self.cifName + atomLabel)
        output.append([uuid_atom, "Instance", "AtomSite", "", "", ""])

        # Define relation between the class instances:
        output.append([subject, "Instance", uuid_atom, predicate, "", ""])

        ### Setting the available data:
        if self.cifLabel is not None:
            output.append([self.crystOntoPrefix + "hasAtomSiteLabel",
                           "Data Property", uuid_atom, "",
                           self.cifLabel, "xsd:string"])
            #print("cifLabel =", self.cifLabel)
        #else:
            #print("cifLabel =", self.cifLabel)

        if self.occupancy is not None:
            output.append([self.crystOntoPrefix + "hasOccupancy",
                           "Data Property", uuid_atom, "",
                           self.occupancy, "xsd:decimal"])

        if self.element is not None:
            # TODO add species
            #output.append("")
            #output.append([self.crystOntoPrefix + "has", "Data Property",
            #                 uuid_atom, "", self.occupancy, "xsd:decimal"])
            output.append([self.crystOntoPrefix + "hasAtomSiteLabel",
                           "Data Property", uuid_atom,
                           "", self.element, "xsd:string"])

        if self.frac is not None:
            atomPos = ocdt.OntoVector(
                            class_name = "PositionVector",
                            item_name = self.cifName + "_FracCoord_" + label, #+ cifName + label,
                            uuidDB = self.uuidDB,
                            unit = "om:dimensionOne")

            atomPos.addComponent(label = "x", value = to_round(self.frac[0])) #, error = error)
            atomPos.addComponent(label = "y", value = to_round(self.frac[1])) #, error = error)
            atomPos.addComponent(label = "z", value = to_round(self.frac[2])) #, error = error)
            #atomPos.addComponent(label = "x", value = str(round(float(self.frac[0]), 10))) #, error = error)
            #atomPos.addComponent(label = "y", value = str(round(float(self.frac[1]), 10))) #, error = error)
            #atomPos.addComponent(label = "z", value = str(round(float(self.frac[2]), 10))) #, error = error)
            output += atomPos.get_csv_arr(uuid_atom, self.crystOntoPrefix + "hasFractionalPosition")

        if self.cart is not None:
            atomPos = ocdt.OntoVector(
                            class_name = "PositionVector",
                            item_name  = self.cifName + "_CartCoord_" + label, #+ cifName + label,
                            uuidDB = self.uuidDB,
                            #myName  = self.cifName + "_CartCoord_" + label, #+ cifName + label,
                            unit  = "om:angstrom")

            atomPos.addComponent(label = "x", value = to_round(self.cart[0])) #, error = error)
            atomPos.addComponent(label = "y", value = to_round(self.cart[1])) #, error = error)
            atomPos.addComponent(label = "z", value = to_round(self.cart[2])) #, error = error)
            #atomPos.addComponent(label = "x", value = str(round(float(self.cart[0]), 10))) #, error = error)
            #atomPos.addComponent(label = "y", value = str(round(float(self.cart[1]), 10))) #, error = error)
            #atomPos.addComponent(label = "z", value = str(round(float(self.cart[2]), 10))) #, error = error)
            output += atomPos.get_csv_arr(uuid_atom, self.crystOntoPrefix + "hasCartesianPosition")

        return output
        # === end of AtomInformation.getArrAtom()

    # === end of class AtomInformation

class CrystalData:
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

    __slots__ = ["uuidDB", "algorithm", "cifPath", "cifName", # the unique id in owl file
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
    def __init__(self, algType, uuidDB):
        # Must be one of 'PyMatGen' or 'ValAndErr', depending how it was created:
        if   "PyMatGen"  == algType:
            self.algorithm  = algType
        elif "ValAndErr" == algType:
            self.algorithm  = algType
        else:
            logging.error(" Invalid algorithm type in CrystalInformation: " +
                          " '%s'. Must be one of: 'PyMatGen', 'ValAndErr'.",
                          algType)
            self.algorithm = None

        # The uuid database to generate unique uuid:
        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            logging.error(" Invalid entry '" + str(uuidDB) + "', expecting a database.")
            self.uuidDB = None
        #if isinstance(uuidDB, dict):
        #  logging.error(" Invalid entry '" + str(uuidDB) + "', expecting a database.")

        #self.cifStandard = None
        self.cifStandard = self.readStandardFile("CIF_standard_2.4.5.txt")

        self.cifName = None
        self.cifPath = None

        self.struct               = None # entity of PyMatGen Structure() class.

        self.unitCellLengths      = None
        self.unitCellAngles       = None
        self.unitCellRecipLengths = None
        self.unitCellRecipAngles  = None
        self.unitCellVolume       = None

        self.unitCellVectorA = None
        self.unitCellVectorB = None
        self.unitCellVectorC = None

        self.unitCellRecipVectorA = None
        self.unitCellRecipVectorB = None
        self.unitCellRecipVectorC = None

        self.listAtomRaw  = None
        self.listAtomAll  = None

        # FIXME here should be None, but it crashes
        self.listAtomSymm = []

        self.symmLatticeSystem = None
        self.symmITNumber      = None

        pass # CrystalInformation.__init__()

    def readStandardFile(self, path):
        output = []
        if not os.path.isfile(path):
            logging.error("CIF standard file does not exist: '%s'.", path)
            return []
        f = open(path, encoding="utf8")
        for line in f:
            short = cleanString(line)
            pos = short.find("_")
            if 0 == pos:
                pos = short.find("_[]")
                if pos < 0:
                    output.append(short)
                else:
                    pass

        f.close()
        return output

        pass # CrystalInformation.readStandardFile()

    def loadData(self, cifPath, cifName):
        """

        """
        if self.algorithm is None:
            logging.error(" Failed CrystalInformation.loadData(%s)," +
                          " due to missing algorithm.", cifPath)
            return

        self.cifName = cifName
        self.cifPath = cifPath

        where = " in '" + cifPath + "', algorithm '" + self.algorithm + "'"
        if self.unitCellLengths is not None:
            logging.warning(" Overwriting 'unitCellLengths' %s.", where)
        if self.unitCellAngles is not None:
            logging.warning(" Overwriting 'unitCellAngles' %s.", where)
        if self.unitCellRecipLengths is not None:
            logging.warning(" Overwriting 'unitCellRecipLengths' %s.", where)
        if self.unitCellRecipAngles is not None:
            logging.warning(" Overwriting 'unitCellRecipAngles' %s.", where)
        if self.unitCellVolume is not None:
            logging.warning(" Overwriting 'unitCellVolume' %s.", where)
        if self.listAtomRaw is not None:
            logging.warning(" Overwriting 'listAtomRaw' %s.", where)
        if self.listAtomAll is not None:
            logging.warning(" Overwriting 'listAtomAll' %s.", where)
        if self.listAtomSymm != []:
            logging.warning(" Overwriting 'listAtomSymm' %s.", where)
            self.listAtomSymm = []

        if "PyMatGen" == self.algorithm:
            self.loadPyMatGen(cifPath, cifName)

        elif "ValAndErr" == self.algorithm:
            self.loadValAndErr(cifPath, cifName)

        else:
            logging.error(" Unknown algorithm '%s'. Expecting 'PyMatGen'" +
                          " or 'ValAndErr'.", str(self.algorithm))

        pass # CrystalInformation.loadData()

    def loadPyMatGen(self, cif_path, cif_name):
        if not os.path.isfile(cif_path):
            logging.error(" Failed to load CIF data, no input file '%s'.", cif_path)
            return

        self.struct = Structure.from_file(cif_path)

        try:
            pass
        except :
            logging.error("=================================================")
            logging.error(" Failed to read data from '%s' cif file", cif_path)
            logging.error("=================================================")
            with open("failed_cif_files.txt", "a", encoding="utf-8") as fp:
                fp.write(cif_path + "\n")

            #self.struct = None

        #logging.error(" Not implemented def loadPyMatGen (self, path): ")

        pass # CrystalInformation.loadPyMatGen()

    def evalPyMatGenUnitCell(self):
        """
        Convert the PyMatGet-structure data into unit-cell information.
        """
        if self.struct is None:
            return
    #logging.error(" Not implemented eeetttwww  def loadPyMatGenUnitCell (self, path): ")


        # The Unit Cell Lengths:
        self.unitCellLengths = ocdt.OntoVector(
                                      class_name = "UnitCellLengths",
                                      item_name  = "UnitCellLengths_" + self.cifName,
                                      uuidDB    = self.uuidDB,
                                      #myName  = "UnitCellLengths_" + self.cifName,
                                      unit      = "om:angstrom")
         #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom")

        self.unitCellLengths.addComponent(label = "a",
             value = str(round(self.struct.lattice.a, 12)), error = "")

        self.unitCellLengths.addComponent(label = "b",
             value = str(round(self.struct.lattice.b, 12)), error = "")

        self.unitCellLengths.addComponent(label = "c",
             value = str(round(self.struct.lattice.c, 12)), error = "")

        # The Unit Cell Angles:
        self.unitCellAngles = ocdt.OntoVector(class_name="UnitCellAngles",
                                              item_name="UnitCellAngles_" + self.cifName,
                                          uuidDB = self.uuidDB,
                                          unit  = "om:degree")

        self.unitCellAngles.addComponent(label = "alpha",
             value = str(round(self.struct.lattice.alpha, 12)), error = "")

        self.unitCellAngles.addComponent(label = "beta",
             value = str(round(self.struct.lattice.beta , 12)), error = "")

        self.unitCellAngles.addComponent(label = "gamma",
             value = str(round(self.struct.lattice.gamma, 12)), error = "")

        # The Reciprocal Unit Cell Lengths:
        self.unitCellRecipLengths = ocdt.OntoVector(
                                           class_name = "UnitCellLengths",
                                           item_name  = "ReciprocalUnitCellLengths_" + self.cifName,
                                           uuidDB = self.uuidDB,
                                           unit  = "om:reciprocalAngstrom")

        self.unitCellRecipLengths.addComponent(label = "a",
             value = str(round(self.struct.lattice.reciprocal_lattice.a, 12)), error = "")

        self.unitCellRecipLengths.addComponent(label = "b",
             value = str(round(self.struct.lattice.reciprocal_lattice.b, 12)), error = "")

        self.unitCellRecipLengths.addComponent(label = "c",
             value = str(round(self.struct.lattice.reciprocal_lattice.c, 12)), error = "")

        # The Reciprocal Unit Cell Angles:
        self.unitCellRecipAngles = ocdt.OntoVector(uuidDB = self.uuidDB,
                                           class_name = "UnitCellAngles",
                                           item_name  = "ReciprocalUnitCellAngles_" + self.cifName,
                                           unit  = "om:degree")

        self.unitCellRecipAngles.addComponent(label = "alpha",
             value = str(round(self.struct.lattice.reciprocal_lattice.alpha, 12)), error = "")

        self.unitCellRecipAngles.addComponent(label = "beta",
             value = str(round(self.struct.lattice.reciprocal_lattice.beta , 12)), error = "")

        self.unitCellRecipAngles.addComponent(label = "gamma",
             value = str(round(self.struct.lattice.reciprocal_lattice.gamma, 12)), error = "")

        # Vectors to keep three Unit Cell vectors (a,b,c):
        self.unitCellVectorA = ocdt.OntoVector(uuidDB = self.uuidDB,
                                           class_name = "UnitCellLatticeVector",
                                           item_name  = "UnitCellVectorA_" + self.cifName,
                                           unit  = "om:angstrom",
                                           vectorLabel = "a")

        self.unitCellVectorA.addComponent(label = "x",
             value = str(round(self.struct.lattice.matrix[0][0], 12)), error = "")

        self.unitCellVectorA.addComponent(label = "y",
             value = str(round(self.struct.lattice.matrix[0][1], 12)), error = "")

        self.unitCellVectorA.addComponent(label = "z",
             value = str(round(self.struct.lattice.matrix[0][2], 12)), error = "")

        self.unitCellVectorB = ocdt.OntoVector(uuidDB = self.uuidDB,
                                       class_name = "UnitCellLatticeVector",
                                       item_name  = "UnitCellVectorB_" + self.cifName,
                                       unit  = "om:angstrom",
                                       vectorLabel = "b")

        self.unitCellVectorB.addComponent(label = "x",
             value = str(round(self.struct.lattice.matrix[1][0], 12)), error = "")

        self.unitCellVectorB.addComponent(label = "y",
             value = str(round(self.struct.lattice.matrix[1][1], 12)), error = "")

        self.unitCellVectorB.addComponent(label = "z",
             value = str(round(self.struct.lattice.matrix[1][2], 12)), error = "")


        self.unitCellVectorC = ocdt.OntoVector(uuidDB = self.uuidDB,
                                       class_name = "UnitCellLatticeVector",
                                       item_name  = "UnitCellVectorC_" + self.cifName,
                                       unit  = "om:angstrom",
                                       vectorLabel = "c")

        self.unitCellVectorC.addComponent(label = "x",
             value = str(round(self.struct.lattice.matrix[2][0], 12)), error = "")

        self.unitCellVectorC.addComponent(label = "y",
             value = str(round(self.struct.lattice.matrix[2][1], 12)), error = "")

        self.unitCellVectorC.addComponent(label = "z",
             value = str(round(self.struct.lattice.matrix[2][2], 12)), error = "")


        # Vectors to keep three Reciprocal Unit Cell vectors (a,b,c):
        self.unitCellRecipVectorA = ocdt.OntoVector(uuidDB = self.uuidDB,
                                  class_name = "UnitCellLatticeVector",
                                  item_name  = "ReciprocalUnitCellLatticeVectorA_" + self.cifName,
                                  unit  = "om:reciprocalAngstrom",
                                  vectorLabel = "a")

        self.unitCellRecipVectorA.addComponent(label = "x",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[0][0], 12)), error = "")

        self.unitCellRecipVectorA.addComponent(label = "y",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[0][1], 12)), error = "")

        self.unitCellRecipVectorA.addComponent(label = "z",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[0][2], 12)), error = "")

        self.unitCellRecipVectorB = ocdt.OntoVector(uuidDB = self.uuidDB,
                                  class_name = "UnitCellLatticeVector",
                                  item_name  = "ReciprocalUnitCellLatticeVectorB_" + self.cifName,
                                  unit  = "om:reciprocalAngstrom",
                                  vectorLabel = "a")

        self.unitCellRecipVectorB.addComponent(label = "x",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[1][0], 12)), error = "")

        self.unitCellRecipVectorB.addComponent(label = "y",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[1][1], 12)), error = "")

        self.unitCellRecipVectorB.addComponent(label = "z",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[1][2], 12)), error = "")

        self.unitCellRecipVectorC = ocdt.OntoVector(uuidDB = self.uuidDB,
                                  class_name = "UnitCellLatticeVector",
                                  item_name  = "ReciprocalUnitCellLatticeVectorC_" + self.cifName,
                                  unit  = "om:reciprocalAngstrom",
                                  vectorLabel = "a")

        self.unitCellRecipVectorC.addComponent(label = "x",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[2][0], 12)), error = "")

        self.unitCellRecipVectorC.addComponent(label = "y",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[2][1], 12)), error = "")

        self.unitCellRecipVectorC.addComponent(label = "z",
             value = str(round(self.struct.lattice.reciprocal_lattice.matrix[2][2], 12)), error = "")


        self.unitCellVolume = ocdt.OntoMeasureWithUncertainty(class_name = "UnitCellVolume",
                                      item_name = "CellVolume_" + self.cifName,
                                      uuidDB = self.uuidDB)

        self.unitCellVolume.setValue(round(self.struct.lattice.volume, 4),
                                     unit = "om:cubicAngstrom")

        sga = pymatgen.symmetry.analyzer.SpacegroupAnalyzer(self.struct)

        """
        print(">>>>>>>>>>> Zeo Name = ")
        print("SG number:", sga._space_group_data)
        print("SG number:", sga.get_space_group_number())
        print("SG number:", sga.get_crystal_system())
        print("SG number:", sga._space_group_data["number"], sga.get_space_group_number(), sga.get_crystal_system())
        """
        if isinstance(sga._space_group_data, dict):
            if "number" in sga._space_group_data.keys():

                self.symmLatticeSystem = sga.get_crystal_system()
                self.symmITNumber = sga.get_space_group_number()
                """
                print("SG number:", sga._space_group_data["number"], sga.get_space_group_number(), sga.get_crystal_system())
                print("   ", #sga._get_symmetry(),
                      sga.get_hall(),
                      sga.get_lattice_type(), #sga.get_symmetry_dataset()
                      #sga.int_symbol()
                      #sga._abc_impl)
                """
                """
            #if isinstance(sga.get_crystal_system(), str) :
            output.append([self.crystOntoPrefix + "hasLatticeSystem",
                               "Data Property", uuid_cif_uc, "",
                               sga.get_crystal_system() , "string"])
            output.append([self.crystOntoPrefix + "hasSymmetryNumber",
                               "Data Property", uuid_cif_uc, "",
                               sga.get_space_group_number() , "xsd:integer"])

        if self.cifOutput.symmLatticeSystem != None:
          output += self.cifOutput.symmLatticeSystem.getArr(uuid_uc_r_vec_abc,
                    self.crystOntoPrefix + "hasLatticeSystem")

        if self.cifOutput.symmITNumber != None:
          output += self.cifOutput.symmITNumber.getArr(uuid_uc_r_vec_abc,
                    self.crystOntoPrefix + "hasSymmetryNumber")

                """

            pass # CrystalInformation.evalPyMatGenUnitCell()

    def evalPyMatGen(self):
        #logging.error(" Not implemented eeetttwww2  def evalPyMatGen (self, path): ")

        if "PyMatGen" != self.algorithm:
            logging.error(" Invalid algorithm '%s', expectec 'PyMatGen'",
                          self.algorithm)

        self.evalPyMatGenUnitCell()
        self.evalPyMatGenAtom()

        pass # CrystalInformation.evalPyMatGen()

    def evalPyMatGenAtom(self):
        """
        Convert the PyMatGet-structure data into internal array listAtomAll.


        """
        if self.struct is None:
            return
    #logging.error(" Not implemented eeetttrrr  def loadPyMatGenAtom (self, path): ")
    #print("Number of sites =", len(self.struct.sites))

        self.listAtomAll = []
        for site in self.struct.sites:
            atom = AtomInformation(self.uuidDB, compound = self.cifName)
            atom.setCoordFrac(site.frac_coords[0], site.frac_coords[1], site.frac_coords[2])
            atom.setCoordCart(site.coords[0], site.coords[1], site.coords[2])
            atom.setProp(element = site.species_string)

            #atom.x = site.coords[0]
            #atom.y = site.coords[1]
            #atom.z = site.coords[2]
            #atom.element   = site.species_string
            # FIXME no occupancy ??
            #atom.occupancy = site.species_and_occupancy[0].occupancy
            #print(type(site), site, ";", site.species_string, ";") #, vars(site._species))
            #print(site, type(site), vars(site))
            #print(" >>>>  ", vars(site._species))
            #print(" >>>>  ", vars(site))

            self.listAtomAll.append(atom)

        #print(self.struct.sites, type(self.struct.sites))

        pass # CrystalInformation.evalPyMatGenAtom()

    def evalValAndErr(self):
        logging.error(" Not implemented eeetttwww3  def evalValAndErr(): ")

        if "ValAndErr" != self.algorithm:
            logging.error(" Invalid algorithm '%s', expectec 'ValAndErr'",
                          self.algorithm)

        self.evalValAndErrUnitCell()
        #self.evalValAndErrAtom()

        pass # CrystalInformation.evalValAndErr()

    def evalValAndErrUnitCell(self):

        """
        self.unitCellLengths.addComponent(label = "a",
             value = str(round(self.struct.lattice.a, 12)), error = "")

        self.unitCellLengths.addComponent(label = "b",
             value = str(round(self.struct.lattice.b, 12)), error = "")

        self.unitCellLengths.addComponent(label = "c",
             value = str(round(self.struct.lattice.c, 12)), error = "")


        # Vectors to keep three Unit Cell vectors (a,b,c):
        self.unitCellVectorA = OntoVector(uuidDB = self.uuidDB,
                                       className = "UnitCellLatticeVector",
                                       itemName  = "UnitCellVectorA_" + self.cifName,
                                       unit  = "om:angstrom",
             #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                                       vectorLabel = "a")

        self.unitCellVectorA.addComponent(label = "x",
             value = str(round(self.struct.lattice.matrix[0][0], 12)), error = "")

        self.unitCellVectorA.addComponent(label = "y",
             value = str(round(self.struct.lattice.matrix[0][1], 12)), error = "")

        self.unitCellVectorA.addComponent(label = "z",
             value = str(round(self.struct.lattice.matrix[0][2], 12)), error = "")

        """


        pass # CrystalInformation.evalValAndErrUnitCell()

    def loadValAndErr(self, cifPath, cifName):
        if not os.path.isfile(cifPath):
            logging.error(" Failed to load CIF data, no input file '" + cifPath + "'.")
            return

        #self.
        #logging.error(" Not implemented def loadValAndErr (self, path): ")

        #All loading is done in one go:
        nBracket = self.readWithUncertainties(cifPath, cifName, save=True)
        #print("Found brackets:",  nBracket, "in '" + cifName + "'.")

        #self.loadValAndErrUnitCell()
        #self.loadValAndErrAtom()
        #self.loadValAndErr


        pass # CrystalInformation.loadValAndErr()

    def readWithUncertainties(self, fileIn, cifName, lineFr=None, lineTo=None,
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

        if not os.path.isfile(fileIn):
            logging.error(" Input file '%s' does not exist in cleanCif().", fileIn)
            return -1

        fIn = open(fileIn)

        if len(fileOut) > 0:
            fOut = open(fileOut, "w", encoding="utf8")
        else:
            logging.info(" Output filename is not specified, no output.") #'" + fileOut + "' ")

        inLoop = False
        countBrackets = 0

        for il, line in enumerate(fIn):
            file_line = "In file '" + fileIn + "' line " + str(il+1)
            #print(file_line)

            if il >= 20:
                logging.debug(" Break after 75 lines for testing. %s",
                              file_line)
                #break
                pass

            pos1 = line.find("#")
            pos2 = line.find(";")
            if   0 == pos1:
                logging.info(" Found a comment in string '%s' %s.",
                             line.strip(), file_line)
                #short = line[:pos] + "\n"
                if len(fileOut) > 0:
                    fOut.write(line)
                continue

            elif 0 == pos2:
                logging.info(" Found a comment in string '%s' %s.",
                             line.strip(), file_line)
                #short = line[:pos] + "\n"
                if len(fileOut) > 0:
                    fOut.write(line)
                continue

            elif pos1 > 0 or pos2 > 0:
                logging.warning(" Comment starts not from beginning of" +
                                " the line: '%s' %s.", line.strip(), file_line
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
                        lineNew, nbrac = splitErrorBarLoop(line, self.loopHeaders, file_line)
                        countBrackets += nbrac
                        #print("after splitErrorBarLoop():", lineNew, nbrac)
                        if len(fileOut) > 0:
                            fOut.write(lineNew)
                        continue

                #print("headers =", self.loopHeaders)

            elif len(words) > 2:
                #logging.info(" Length of string = " + str(len(words)) + " " + file_line)
                if len(fileOut) > 0:
                    fOut.write(line)
                pass

            elif "_" == words[0][0]:
                #print("Checking property", words[0], "is it in list of known?",
                #      words[0] in self.entriesWithUncertainties)
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

                    elif words[0] in entriesWithUncertainties:
                        logging.info(" Found one of the entries: %s", words[0])
                        vOut, eOut = splitErrorBar(words[1], file_line)
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
                        #print(">>>>> ", words[0])
                        self._setCifStandardValue(cifName, words)

                        if len(fileOut) > 0:
                            fOut.write(line)
                    else:
                        logging.warning(" Unknown situation. Line = '%s'. %s.",
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
                pass

            else:
                #logging.warning(" default else option.")
                if len(fileOut) > 0:
                    fOut.write(line)
                pass

        fIn.close()
        if len(fileOut) > 0:
            fOut.close()

        #print("Number of brackets =", countBrackets)
        return countBrackets
        # === end of CrystalInformation.readWithUncertainties()


    def _setCifStandardValue(self, cif_name, words):
        #print(">>>>> ", words[0])

        if "_symmetry_cell_setting" == words[0] or \
           "_space_group_crystal_system" == words[0]:
            self.symmLatticeSystem = words[1]

        elif "_symmetry_Int_Tables_number" == words[0] or \
             "_space_group_IT_number" == words[0]:
            self.symmITNumber = int(words[1])

        elif "_symmetry_space_group_name_H-M" == words[0] or \
             "_space_group_name_H-M_alt" == words[0]:
            # TODO not implemented H-M space group
            #self.symm space_group = " ".join(words[1:])
            pass

        elif "_symmetry_space_group_name_Hall" == words[0] or \
             "_space_group_name_Hall" == words[0]:
            # TODO not implemented Hall space group
            #self.symmLatticeSystem = words[1]
            pass

        else:
            logging.error(" Not processed keyword '%s' in '%s'",
                          words[0], cif_name)

        # === end of CrystalInformation._setCifStandardValue()

    def _setValueAndError(self, cifName, entry, value, error):
        """
        Setting the value and error values to the internal parameters.

        """

        if ("_cell_length_a" == entry or "_cell_length_b" == entry or
            "_cell_length_c" == entry) and  (self.unitCellLengths is None):
            self.unitCellLengths = ocdt.OntoVector(class_name = "UnitCellLengths",
                                         item_name  = "UnitCellLengths_" + cifName,
                                         uuidDB    = self.uuidDB,
                                         unit      = "om:angstrom")
       #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom")

        if ("_cell_angle_alpha" == entry or "_cell_angle_beta" == entry or
            "_cell_angle_gamma" == entry) and (self.unitCellAngles is None):
            self.unitCellAngles = ocdt.OntoVector(
                                        class_name = "UnitCellAngles",
                                        item_name  = "UnitCellAngles_" + cifName,
                                        uuidDB    = self.uuidDB,
                                        unit      = "om:degree")
        #myUnit = "http://www.ontology-of-units-of-measure.org/resource/om-2/degree")

        #logging.warning(" _setValueAndError(): cifName = '%s'," +
        #                " entry = '%s', value = '%s', error = '%s'.",
        #                str(cifName), str(entry), str(value), str(error))

        if "_cell_length_a" == entry:
            self.unitCellLengths.addComponent(label = "a", value = value, error = error)

        elif "_cell_length_b" == entry:
            self.unitCellLengths.addComponent(label = "b", value = value, error = error)

        elif "_cell_length_c" == entry:
            self.unitCellLengths.addComponent(label = "c", value = value, error = error)

        elif "_cell_angle_alpha" == entry:
            self.unitCellAngles.addComponent(label = "alpha", value = value, error = error)

        elif "_cell_angle_beta" == entry:
            self.unitCellAngles.addComponent(label = "beta", value = value, error = error)

        elif "_cell_angle_gamma" == entry:
            self.unitCellAngles.addComponent(label = "gamma", value = value, error = error)


        elif "_cell_volume" == entry:
            self.unitCellVolume = ocdt.OntoMeasureWithUncertainty(
                    tbox_prefix = "", abox_prefix = "",
                    uuidDB = self.uuidDB,
                    class_name = "UnitCellVolume",
                    item_name = "UC_Volume_" + str(cifName))

            self.unitCellVolume.setValue(value=value, error=error,
                                         unit="om:cubicAngstrom")

        elif "_symmetry_equiv_pos_as_xyz" == entry or "_space_group_symop_operation_xyz":
            # '_space_group_symop_operation_xyz'
            # May appear in list containing _space_group_symop_id
            # '_symmetry_equiv_pos_as_xyz'
            # This definition has been superseded and is retained here only for archival purposes.
            self.listAtomSymm.append(value)

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
            logging.error("Unknown entry to store data with error: '%s'.",
                          entry)

        #print(" >>>> entry ", entry, self.algorithm)
        #print(self.unitCellLengths)

        pass # CrystalInformation._setValueAndError()


        """
    if None != self.listAtomRaw:
      logging.warning(" Overwriting 'listAtomRaw' " + where + ".")
    if None != self.listAtomAll:
      logging.warning(" Overwriting 'listAtomAll' " + where + ".")
    if [] != self.listAtomSymm:
      logging.warning(" Overwriting 'listAtomSymm' " + where + ".")
        """

    pass # class CrystalInformation

if __name__ == "__main__":
    pass
