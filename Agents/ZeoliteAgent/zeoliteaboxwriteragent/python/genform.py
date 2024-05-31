"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import os
import sys
# import csv

import tools


def remove_numbers(line):
    output = line
    for num in range(10):
        output = output.replace(str(num), "")
    output = output.replace(".", "")
    return output


def read_command_line():
    cmnd = {}
    cmnd["save"] = False

    if len(sys.argv) > 1:
        for v in sys.argv:
            # print("Command line arg:", v)
            if "save" == v:
                cmnd["save"] = True

    # print("Commands =", cmd)
    return cmnd


#
# Return value:
#   1 if no number detected
#   number if integer/float number is detected
def get_number(line, from_pos):
    output = None
    if len(line) <= from_pos:
        output = None
    else:
        word = ""
        to_pos = from_pos
        # print(line, ", to_pos =", to_pos)
        while line[to_pos] in "0123456789.":
            word = word + line[to_pos]
            # print(line, ", to_pos =", to_pos, ", word =", word)
            to_pos += 1
            if to_pos == len(line):
                break

        if len(word) > 0:
            output = float(word)
        else:
            output = 1

    return output

def parse_formula(formula):
    """ A function to extract elements and their indices from a string.
        Some tests are in file 'formula.py'

    H2SO4            => [('H', 2), ('S', 1), ('O', 4)]
    Al5Si3O.2        => [('Al', 5), ('Si', 3), ('O', 0.2)]
    HF               => [('H', 1), ('F', 1)]
    AlxP1-x          => [('Al', 'x'), ('P', '1-x')]
    SxOyT1-x-y       => [('S', 'x'), ('O', 'y'), ('T', '1-x-y')]
    """

    chars_low = "abcdefghijklmnopqrstuvwxyz"
    chars_up = chars_low.upper()

    elements = [
    "H", "He",
    "Li", "Be", "B", "C", "N", "O", "F", "Ne",
    "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar",
    "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga",
    "Ge", "As", "Se", "Br", "Kr",
    "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd",
    "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce",
    "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
    "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir",
    "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac",
    "Th", "Pa", "U", "Np", "Pu", "Am", "Cm",
    "Bk", "Cf", "Es", "Fm", "Md", "No", "Lr"
    ]

    # Prepare the formula:
    formula = formula.replace(" ", "")

    start_indices = []
    for i, char in enumerate(formula):
        if char in chars_up:
            start_indices.append(i)
    start_indices.append(len(formula))
    # "Al3.5Si4O"
    # [0,   5, 8, 9]
    #print(formula, "start_indices =", start_indices)

    output = []
    for i in range(len(start_indices)-1):
        part = formula[start_indices[i] : start_indices[i+1]]
        #print(part)

        element = part[0]
        if len(part) == 1:
            if part in elements:
                output.append( (element, 1))
            else:
                output.append(None)

            continue

        if part[1] in chars_low:
            if element + part[1] in elements:
                element += part[1]
                rest = part[2:]
            else:
                rest = part[1:]
        else:
                rest = part[1:]

        #print(element, rest)

        if len(rest) == 0:
            index = 1
        else:
            index = None

        if index is None:
            try:
                index = int(rest)
            except:
                #print("invalid value:", rest)
                pass
        if index is None:
            try:
                index = float(rest)
            except:
                #print("Invalid value:", rest)
                pass
        if index is None:
            index = rest


        #if "." in rest:
        output.append( (element, index) )

    return output

class GenFormula:
    __slots__ = ["fullFormula", "framework", "formula", "status", "guest",
                 "verification", "M1", "M2", "M3", "T1", "T2", "O1", "O2",
                 "x", "y", "z", "s", "t", "h", "n", "n2",
                 "nx", "ny", "nz", "ns", "nt", "nu", "nv", "nh"]

    def __init__(self, inArr=[]):
        if len(inArr) == 0:
            # print("No data provided for GenericFormula")
            for s in self.__slots__:
                setattr(self, s, "")
            return

        self.fullFormula = inArr[0]
        self.framework   = inArr[1]
        self.formula     = inArr[2]
        self.status      = inArr[3]
        self.verification = inArr[4]
        self.M1 = inArr[5]
        self.M2 = inArr[6]
        self.M3 = inArr[7]
        self.T1 = inArr[8]
        self.T2 = inArr[9]
        self.O1 = inArr[10]
        self.O2 = inArr[11]
        self.x = inArr[12]
        self.y = inArr[13]
        self.z = inArr[14]
        self.s = inArr[15]
        self.t = inArr[16]
        self.h = inArr[17]
        self.n = inArr[18]
        self.n2 = inArr[19]
        self.nx = inArr[20]
        self.ny = inArr[21]
        self.nz = inArr[22]
        self.ns = inArr[23]
        self.nt = inArr[24]
        self.nu = inArr[25]
        self.nv = inArr[26]
        self.nh = inArr[27]

        # print("Got one formula")
        # === end of GenFormula.__init__()

    def to_arr(self):
        temp = []
        for s in self.__slots__:
            temp.append(getattr(self, s))
        return temp
        # === end of GenFormula.to_arr()

    def to_str(self):
        # print(self.__slots__)
        temp = []
        for s in self.__slots__:
            temp.append(str(getattr(self, s)))
        output = "\t".join(temp)
        # output = " ".join(self.__slots__)

        return output
        # === end of GenFormula.to_str()

    def get_elements(self):
        slots = "M1", "M2", "M3", "T1", "T2", "O1", "O2"
        output = []
        for s in slots:
            if getattr(self, s) != "":
                output.append(getattr(self, s))
        return output
        # === end of GenFormula.get_elements()

    def getFrame(self):
        if "" == self.framework:
            self.framework = "NO_FRAMEWORK"
            # pos = self.fullFormula.find("]-")
            # if pos >= 0:
            #    self.framework = self.fullFormula[pos+2:].strip()
            #    return self.framework
            pos = self.fullFormula.find("]")
            pos = self.fullFormula[pos:].find("-") + pos
            if pos >= 0:
                self.framework = self.fullFormula[pos+1:].strip()

            if "-" == self.framework[0]:
                self.framework = "_" + self.framework[1:4]
            else:
                self.framework = self.framework[0:3]
            # else:
            #  print("Empty framework?")

            pass
        return self.framework
        # === end of GenFormula.getFrame()

    def getFormula(self):
        fr = self.fullFormula.find("[")
        to = self.fullFormula.find("]")
        if fr < 0:
            print("For '", self.fullFormula, "' no '['", sep="")
        if to < 0:
            print("For '", self.fullFormula, "' no ']'", sep="")
        if fr > 0 and to > 0 and fr >= to:
            print("For '", self.fullFormula, "' something is wrong ",
                  "in brackets (base)", sep="")
            print("     fr,to =", fr, to)

        self.formula = self.fullFormula[fr+1: to].strip()
        self.n2 = get_number(self.fullFormula, to)
        if self.n2 is None:
            self.n2 = 1

        # === end of GenFormula.getFormula()
    def getGuest(self):
        self.guest = ""
        fr = self.fullFormula.find("|")
        to = self.fullFormula[fr + 1:].find("|") + fr + 1
        if fr < 0:
            print("For '", self.fullFormula, "' no opening '|'", sep="")
        elif to < 0:
            print("For '", self.fullFormula, "' no closing '|'", sep="")
        if fr > 0 and to > 0 and fr >= to:
            print("For '", self.fullFormula, "' something is wrong ",
                  "in brackets (guest)", sep="")
            print("     fr,to =", fr, to)

        if fr >= 0 and to > 0 and fr < to:
            self.guest = self.fullFormula[fr+1: to].strip()
        #print("In getGuest: fr, to=", fr, to)
        #print("   ", self.fullFormula)
        #print("   ", self.guest)

        #self.n2 = get_number(self.fullFormula, to)
        #if self.n2 is None:
        #    self.n2 = 1

    def getElements(self):
        listM = ["Li", "Be", "B", "Al", "Ca", "Ga", "Mg", "Zn", "Mn", "Fe",
                 "Ni", "Co", "V", "Cu", "Ti", "Cr", "Sn", "Ta"]
        listT = ["Si", "Ge", "P", "As", "S"]  # , "X"]
        listO = ["O", "N"]

        if self.status != "NODATA" and self.status != "PASS":
            tmp_form = self.formula

            for m in listM:
                pos = tmp_form.find(m)
                if pos >= 0:
                    if "" == self.M1:
                        self.M1 = m
                        tmp_form = tmp_form.replace(m, "")
                    elif "" == self.M2:
                        self.M2 = m
                        tmp_form = tmp_form.replace(m, "")
                    elif "" == self.M3:
                        self.M3 = m
                        tmp_form = tmp_form.replace(m, "")
                    else:
                        print("For '", self.fullFormula, "': Found 4 metals!",
                              " '" + self.M1 + "' '" + self.M2 + "' '",
                              m + "'.", sep="")
                    # for i = range(self.formula

            for t in listT:
                pos = tmp_form.find(t)
                if pos >= 0:
                    if "X" == t:
                        print("Component X in", tmp_form)
                    if "" == self.T1:
                        self.T1 = t
                        tmp_form = tmp_form.replace(t, "")
                    elif "" == self.T2:
                        self.T2 = t
                        tmp_form = tmp_form.replace(t, "")
                    else:
                        print("For '", self.fullFormula, "': ",
                              "Found 3 non-metals!", sep="")

            tmp_form = tmp_form.replace("(OH)", "")
            tmp_form = tmp_form.replace("OH", "")

            # Removing the oxygen/nitrogen:
            for o in listO:
                pos = tmp_form.find(o)
                if pos >= 0:
                    if "" == self.O1:
                        self.O1 = o
                        tmp_form = tmp_form.replace(o, "")
                    elif "" == self.O2:
                        self.O2 = o
                        tmp_form = tmp_form.replace(o, "")
                    else:
                        print("For '", self.fullFormula, "': ",
                              "Found 3 oxygen-like atoms!", sep="")

            # Removing brackets (usually appear in (OH) group):
            # tmp_form = tmp_form.replace("(", "")
            # tmp_form = tmp_form.replace(")", "")

            tmp_form = remove_numbers(tmp_form)
            if len(tmp_form) != 0:
                print("Warning: tmp_form = '", tmp_form, "'. ", "In '",
                      self.fullFormula, "'.", sep="")

        # === end of GenFormula.getElements()

    def updateStatus(self):
        if self.fullFormula.find(" - ") >= 0:
            self.status = "NODATA"

        # === end of GenFormula.updateStatus()

    def getCoeff(self):
        if self.M1 != "":
            self.nx = get_number(self.formula,
                                 self.formula.find(self.M1) + len(self.M1))
            if self.nx is None:
                self.nx = 1

        if self.M2 != "":
            self.ny = get_number(self.formula,
                                 self.formula.find(self.M2) + len(self.M2))
            if self.ny is None:
                self.ny = 1

        if self.M3 != "":
            self.nz = get_number(self.formula,
                                 self.formula.find(self.M3) + len(self.M3))
            if self.nz is None:
                self.nz = 1

        if self.T1 != "":
            self.nt = get_number(self.formula,
                                 self.formula.find(self.T1) + len(self.T1))
            if self.nt is None:
                self.nt = 1

        if self.T2 != "":
            self.ns = get_number(self.formula,
                                 self.formula.find(self.T2) + len(self.T2))
            if self.ns is None:
                self.ns = 1

        if self.O1 != "":
            self.nu = get_number(self.formula,
                                 self.formula.find(self.O1) + len(self.O1))
            if self.nu is None:
                self.nu = 1

        if self.O2 != "":
            self.nv = get_number(self.formula,
                                 self.formula.find(self.O2) + len(self.O2))
            if self.nv is None:
                self.nv = 1

        # print("'" + str(self.nu) + "'")
        # print("'" + str(self.nv) + "'")
        self.n = 0.0
        # if isinstance(self.nu, int) or isinstance(self.nu, float):
        if isinstance(self.nu, (int, float)):
            self.n += self.nu
        # if isinstance(self.nv, int) or isinstance(self.nv, float):
        if isinstance(self.nv, (int, float)):
            self.n += self.nv
        self.n *= 0.5 * self.n2

        # FIXME this is an error!
        if "(OH)" != "":
            self.nh = get_number(self.formula,
                                 self.formula.find("(OH)") + len("(OH)"))
            if self.nh is None:
                self.nh = 1
        if "OH" != "":
            self.nh = get_number(self.formula,
                                 self.formula.find("OH") + len("OH"))
            if self.nh is None:
                self.nh = 1
        # if "O" != "":
        #    self.n = 0.5 * get_number(self.formula,
        #                         self.formula.find("O") + len("O"))
        '''
    if "H" != "":
      self.nh = get_number(self.formula, self.formula.find("H") + len("H"))
      if None == self.nh:
        self.nh = 1
    if "F" != "":
      self.nh = get_number(self.formula, self.formula.find("F") + len("F"))
      if None == self.nh:
        self.nh = 1
    '''

        # === end of GenFormula.getCoeff()

    def verify(self):
        pass  # === end of GenFormula.verify()

    # === end of class GenFormula

# ZEOList = ["ABW"]


def findLatestCsv(filebase="ForGenericData"):

    version = -1
    for v in range(10):
        filename = filebase + "-" + str(v) + ".csv"
        # print("Testing '" + filename + "' ")
        if os.path.isfile(filename):
            version = v
        else:
            break

    return version

if __name__ == "__main__":
    cmd = read_command_line()
    # print("Commands =", cmd)

    # 1. Choose the file to load data:
    ver = findLatestCsv()
    print("The latest version = '" + str(ver) + "'.")

    # 2. Read data from the .csv file (most recent or one of the previous):
    filenameIn = "ForGenericData-" + str(ver) + ".csv"
    # inArr = loadCSV(filenameIn)
    inArr = tools.readCsv(filenameIn)
    fArr = []
    for fIn in inArr[1:]:
        f = GenFormula(fIn)
        fArr.append(f)

    # 3. Do correction/calculation:
    tmpCount = 0
    for f in fArr[:]:
        # print(f.getFrame())
        f.getFormula()
        f.updateStatus()
        f.getElements()
        f.getCoeff()
        f.verify()
        # tmpStr = " ".join([f.M1, f.M2, f.M3, f.T1, f.T2, f.O1, f.O2])
        #             "verification", "M1", "M2", "M3", "T1", "T2", "O1", "O2",
        tmpStr = " ".join([f.M1, f.M2, f.M3, f.T1, f.T2])
        if len(tmpStr.split()) == 2:
            tmpCount += 1

    print("Count ternary: ", tmpCount)
    # 4. Save backup and save the current data file:
    if cmd["save"]:
        # print("Slots:", fArr[0].__slots__)
        filenameOut = "ForGenericData-" + str(ver+1) + ".csv"

        fOut = []
        if len(fArr) > 0:
            tmp = fArr[0]
        else:
            tmp = GenFormula("")
        fOut.append(tmp.__slots__)  # This is the header

        # print("Number of elements in fArr:", len(fArr))
        for i, f in enumerate(fArr):
            # if 0 == i:
            #    print(f.to_str)
            fOut.append(f.to_arr())
        tools.writeCSV(filenameOut, fOut)

    # for f in fArr:
    for f in fArr[0:10]:
        print(f.to_str())
        print(f.get_elements())
        pass

    # print("Test: Number from string =", get_number("ad34gt7", 2))
    # === end of __name__


def get_family(frame):
    famList = {}
    famList["ZZ"] = ["ABW", "ATN", "ATO", "ATS", "BCT", "BIK", "CAN", "CAS",
                     "CFI", "-CHI", "DAC", "EPI", "EUO", "GON", "ITW", "JBW",
                     "MTT", "MTW", "NES", "NON", "NPO", "NSI", "OSI", "SFE",
                     "SFH", "SFN", "SSY", "TON", "VET"]

    famList["SAW"] = ["ATT", "ATV", "AWO", "CDO", "DAC", "EON", "EPI", "FER",
                      "JBW", "LTL", "MAZ", "MFS", "MOR", "OFF", "RWR", "UEI"]

    famList["CRSHFT"] = ["ACO", "AEL", "AET", "AFI", "AFO", "AHT", "APC",
                         "APD", "ATT", "ATV", "AWO", "DFT", "DON", "GIS",
                         "-LIT", "MER", "PHI", "GME", "UEI", "VFI"]

    famList["S3/4R"] = ["EDI", "ITE", "LOV", "MEI", "MON", "NAB", "NAT", "NPO",
                        "OBW", "OSO", "PAR", "PON", "-RON", "RSN", "RTH",
                        "RWY", "THO", "VNI", "VSV", "WEI"]

    famList["D4R"] = ["ACO", "AFI", "AFN", "AFR", "AFS", "AFY", "APC", "APD",
                      "AST", "ASV", "BOG", "BPH", "BRE", "CGF", "CGS", "-CLO",
                      "DFO", "DFT", "ETR", "GIS", "GOO", "HEU", "ITW", "LAU",
                      "LTA", "MEI", "MER", "OWE", "PHI", "RRO", "SAS", "SFO",
                      "STI", "TER", "UOZ", "USI", "YUG", "ZON"]

    famList["5RINGS"] = ["BIK", "CAS", "CDO", "CFI", "-CHI", "CON", "DAC",
                         "DON", "EPI", "ESV", "FER", "GON", "IWR", "MAZ",
                         "MEL", "MFI", "MFS", "MOR", "MTF", "MTT", "MTW",
                         "NSI", "RTE", "SFE", "SFF", "SFH", "SFN", "SGT",
                         "SSY", "STF", "STT", "TON"]

    famList["D6R"] = ["AEI", "AEN", "AFI", "AFO", "AFT", "AFX", "ATT", "ATV",
                      "AWO", "AWW", "BOG", "CGS", "CHA", "EMT", "ETR", "FAU",
                      "GME", "IFR", "KFI", "LAU", "-LIT", "MSO", "RTE", "RUT",
                      "SAO", "SAS", "SAV", "SOS", "TSC", "UEI"]

    famList["ABC"] = ["AFG", "AFT", "AFX", "CAN", "CHA", "EAB", "ERI", "FRA",
                      "GIU", "GME", "LEV", "LIO", "LOS", "MAR", "OFF", "SAT",
                      "SOD"]

    famList["BET"] = ["*BEA", "BEC", "CON", "ISV", "ITH", "IWR", "IWW"]

    famList["CLAT"] = ["DDR", "DOH", "MEP", "MTN"]

    famList["CAGES"] = ["AST", "ATN", "AWW", "-CLO", "DDR", "DOH", "EMT",
                        "FAU", "KFI", "LTA", "LTN", "MEP", "MER", "MTN",
                        "PAU", "RHO", "SBE", "SBS", "SBT", "SOD", "TSC"]

    famList["MISCEL"] = ["ANA", "CZP", "SFG", "UFI", "UTL"]

    total = 0
    # for k in famList.keys():
    # for k in famList:
    for k, value in famList.items():
        # total += len(famList[k])
        total += len(value)
        # print(k)
        pass  # for(k)
    print("Total frameworks:", total)

    inFam = "NONE"
    # for k in famList.keys():
    # for k in famList:
    for k, value in famList.items():
        if frame in value:
            # FIXME - special character '-', '*' in framework..
            if inFam != "NONE":
                print("Warning: double counted framework '" + k + "'")
            else:
                inFam = k

    return inFam
    # === end of get_family()

if __name__ == "__main__":
    print("AAA in ", get_family("AAA"))
    print("CZP in ", get_family("CZP"))
