import os
import math
import logging
import time

#import uuid

import tools

import crystaldata

import ontocrystal_datatypes as ocdt

#logging.basicConfig(level = logging.DEBUG)
#logging.basicConfig(level = logging.INFO)
logging.basicConfig(level = logging.WARNING)
#logging.basicConfig(level = logging.ERROR)


crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
zeoOntoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"


CIF_IRI_FILE = "cif_iri_list.csv"  # Format: filename, count/unique_id, iri, uuid
CIF_IRI_LIST = None
'''
def get_cif_iri(cif_path):
    """

    """
    #if doi.startswith("bibfiles\\"):
    #    doi = doi.replace("bibfiles\\", "")
    #if doi.endswith(".bib"):
    #    doi = doi.replace(".bib", "")

    global CIF_IRI_LIST
    if CIF_IRI_LIST is None:
        CIF_IRI_LIST = tools.readCsv(CIF_IRI_FILE)

    for cif_iri in CIF_IRI_LIST:
        key = cif_iri[0].lower().strip()
        if cif_path == key:
            return cif_iri[2].strip()

    logging.error(" crystalinfo.py: Not found cif_iri '%s' in '%s'.",
                  cif_path, CIF_IRI_FILE)
    return None
'''

def has_xyz_line(file_path):
        with open(file_path, encoding="utf-8") as fp:
            got_symm = False
            for line in fp:
                if "_symmetry_equiv_pos_as_xyz" in line or \
                   "_space_group_symop_operation_xyz" in line:
                    got_symm = True
                    break
        return got_symm

def get_cif_iri(cif_path):
    """

    """
    global CIF_IRI_LIST
    if CIF_IRI_LIST is None:
        CIF_IRI_LIST = tools.readCsv(CIF_IRI_FILE)

    tofind = cif_path.lower().strip()
    for cif in CIF_IRI_LIST:
        if cif[0].lower() == tofind:
            return cif[2].strip(), cif[3].strip()
        #if cif[1].lower() == tofind:  ??
        #    return cif[2].strip()

    import uuid
    uuid_str = str(uuid.uuid4())
    iri = "CIF_data_" + cif_path + "_" + uuid_str
    folder = "folder"
    CIF_IRI_LIST.append([cif_path, folder, iri, uuid_str])
    tools.writeCsv(CIF_IRI_FILE, CIF_IRI_LIST)
    return iri, uuid_str
    # === end of get_cif_iri()


class CifIriData:
    """ Data structure to convert a given file_path to iri,
    based on the database in file CIF_IRI_FILE.
    Main functions:
    get_entry(cif_path) - return iri and uuid for given cif_path, 
                          from existing entry.
                          Return None,None if it does nto exist.
                          or create a new if it does not exist
    set_entry(cif_path, uuid) - assign iri and uuid for given cif_path.
    """

    def __init__(self, filename=None):
        """
        """
        if filename is None:
            self.filename = CIF_IRI_FILE
        else:
            self.filename = filename

        self._load()

        pass

    def _load(self):
        """
        """

        if os.path.isfile(self.filename):
            self.data = tools.readCsv(self.filename)
            if len(self.data) > 1:
                self.data = self.data[1:]
        else:
            self.data = []

        return self.data

    def save(self, filename=None):
        """ Save the current data to filename.
        If no filename provided - save to 
        """
        header = [["CIF file", "id", "iri", "uuid", "Folder", "Errors" ]]
        output = header + self.data
        
        if filename:
            filepath = filename
        else:
            filepath = self.filename

        tools.writeCsv(filepath, output)
        # === end of CifIriData.save()

    def self_test(self):
        """ 
        1. Check that all slashes are \\, not /
        2. Remove empty cif line
        TODO
        """
        err_count = 0

        # Self test 1: wrong slash
        for i_line, line in enumerate(self.data):
            if line[0].find("/") >= 0:
                print("CifIriData has wrong slash:", line[0])
                err_count += 1
                self.data[i_line][0] = line[0].replace("/", "\\")

        # Self test 2: completeness of the database 
        for i_line, line in enumerate(self.data):
            if len(line) < 5:
                print("CifIriData has too few entries:", line)
                err_count += 1
                continue
            elif len(line) == 5:
                print("CifIriData missing error count")
                self.data[i_line].append(-1)
                err_count += 1
            else:
                pass
            # TODO check format of iri, uuid?

        ###### Self test 3: does the file exist?

        return err_count

    def set_entry(self, cif_path, folder_id, err_count=-1):
        """ Change an existing or add a new line to data base. 
        """

        ### header = [["CIF file", "id", "iri", "uuid", "Folder", "Errors" ]]

        cif_path = cif_path.replace("/", "\\")

        found = False
        for i_line, line in enumerate(self.data):
            if line[0] == cif_path:
                self.data[i_line][4] = folder_id
                if err_count >= 0:
                    self.data[i_line][5] = err_count
                found = True
                break

        if not found:
            uuid = tools.new_uuid()
            iri = "CrystalInformation_" + uuid

            data = [cif_path, len(self.data), iri, uuid, folder_id, -1] 
            if err_count >= 0:
                data[5] = err_count
            self.data.append(data)

        pass

    def get_entry_iri(self, cif_path):
        """
        """
        iri = None
        uid = None
        cif_path = cif_path.replace("/", "\\").lower().strip()
        for line in self.data:
            if line[0].lower().strip() == cif_path:
                iri = line[2].strip()
                uid = line[3].strip()
                break

        return iri, uid

    def del_entry(self, cif_path):
        """
        """
        cif_path = cif_path.replace("/", "\\")
        for i_line in reversed(range(len(self.data))):
            line = self.data[i_line]
            if line[0] == cif_path:
                del self.data[i_line]
 
#CIF_IRI_DATA = CifIriData(CIF_IRI_FILE)
CIF_IRI_DATA = CifIriData()

BLACK_LIST = []
"""
              'pdffiles\\10.1107_S0108768192009005-supp.cif',

              'LI-CIF\\Li-ATN.cif',
              'LI-CIF\\Li-AWO.cif',
              'LI-CIF\\Li-CON.cif',
              'LI-CIF\\Li-EUO.cif',
              'LI-CIF\\Li-GIU.cif',
              'LI-CIF\\Li-ITG.cif',
              'LI-CIF\\Li-MRT.cif',
              'LI-CIF\\Li-OBW.cif',
              'LI-CIF\\Li-RSN.cif',
              'LI-CIF\\Li-SFE.cif',
              'LI-CIF\\Li-STW.cif',
              'LI-CIF\\Li-UWY.cif',

]
"""
"""
              'LI-CIF\\Li-BIK.cif',
              'LI-CIF\\Li-BOG.cif',
              'LI-CIF\\Li-BPH.cif',
              'LI-CIF\\Li-DON.cif',
              'LI-CIF\\Li--EWT.cif',
              'LI-CIF\\Li-FAR.cif',
              'LI-CIF\\Li-FRA.cif',
              'LI-CIF\\Li--HOS.cif',
              'LI-CIF\\Li--ITV.cif',
              'LI-CIF\\Li-JRY.cif',
              'LI-CIF\\Li-JSR.cif',
              'LI-CIF\\Li--LIT.cif',
              'LI-CIF\\Li-LTA.cif',
              'LI-CIF\\Li-MAR.cif',
              'LI-CIF\\Li-MSO.cif',
              'LI-CIF\\Li-MWF.cif',
              'LI-CIF\\Li-OSO.cif',
              'LI-CIF\\Li-PON.cif',
              'LI-CIF\\Li-PTF.cif',
              'LI-CIF\\Li-RWY.cif',
              'LI-CIF\\Li-SFW.cif',
              'LI-CIF\\Li-SOF.cif',
              'LI-CIF\\Li--SSO.cif',
              'LI-CIF\\Li--SVR.cif',
              'LI-CIF\\Li--SYT.cif',
              'LI-CIF\\Li-TUN.cif',
              'LI-CIF\\Li-UOV.cif',
              'LI-CIF\\Li-VNI.cif',
              'LI-CIF\\Li-VSV.cif',
              'LI-CIF\\Li-YUG.cif',
              'LI-CIF\\Li-ZON.cif',
              'LI-CIF\\Li-JZT.cif',
              'LI-CIF\\Li-EOS.cif',
              'LI-CIF\\Li--ION.cif',
#              'LI-CIF\\Li-RFE.cif'

              #'LI-CIF\\Li--CHI.cif',
              #'LI-CIF\\Li--CLO.cif',
              #'LI-CIF\\Li--IFT.cif',
              #'LI-CIF\\Li--IFU.cif',
              #'LI-CIF\\Li--IRT.cif',
              #'LI-CIF\\Li--IRY.cif',
              #'LI-CIF\\Li--PAR.cif',
              #'LI-CIF\\Li--RON.cif',
              #'LI-CIF\\Li--WEN.cif',


]
"""

class CrystalInfo:
    """ Manipulation with crystal data from CIF file.

    Creates a csv structure for abox writer containing CIF information:
    Contains instances of classes:
    - OntoCrystal.CrystalInformation
        default name CrystalInformation_UUID, where UUID is a uuid4() string.
        UUID may be specified by the user (see argument new_uuid).
    - OntoCrystal.UnitCell
        default name UnitCell_UUID, where UUID is the same as above.
    - OntoCrystal.AtomicStructure
        default name AtomicStructure_UUID, where UUID is the same

    TODO write more doc for CrystalInfo
    """

    def __init__(self, uuidDB,
                 # tbox_prefix=None,
                 abox_prefix=None):

        self.uuidDB = uuidDB
        self.iri = None

        self.cifOutput = None
        self.cifPyMatGen = None
        self.cifValAndErr = None

        """
        if tbox_prefix:
            self.tbox_prefix = tbox_prefix
        else:
            self.tbox_prefix = ""
        """

        if abox_prefix:
            self.abox_prefix = abox_prefix
        else:
            self.abox_prefix = ""

        # === end of CrystalInfo.()

    def get_uuid(self):
        if self.iri is None:
            logging.error(" In crystalinfo.CrystalInfo uuid_cif = None.")
            return "None"
        return self.iri
        # === end of CrystalInfo.()

    def in_black_list(self, file_path):
        if file_path in BLACK_LIST:
            return True
        return False
        # === end of CrystalInfo.()

    def load_cif_file_py_mat_gen(self, file_path, options=None):
        """ Load a CIF file and store the data in internal variables.

            options - is a dictionary with keys:
            ""?

            Returns the number of errors.
        """
        err_count = 0

        if self.in_black_list(file_path):
            logging.error(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            logging.error(" Badly formed cif file: '%s', skip it", file_path)
            logging.error(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            return 1

        if os.path.isfile(file_path):
            path = file_path
        if os.path.isfile(file_path.replace("--", "-")):
            path = file_path.replace("--", "-")
        else:
            logging.error(" CIF file does not exist: '%s'.", file_path)
            return 1

        path = self.cleanCif(path)[0]

        # print("path =", path)
        name = "TODO-remove-name in crystalinfo"
        name = ""

        # First load data to (struct in cifPyMatGen and unitCellLengths in cifValAndErr)
        self.loadPyMatGen( path, name)
        self.loadValAndErr(path, name)
        try:
            pass

        except :
            logging.error("=================================================")
            logging.error(" Failed to read data from '%s' cif file", path)
            logging.error("=================================================")
            with open("failed_cif_files.txt", "a", encoding="utf-8") as fp:
                fp.write(path + "\n")

        # Second evaluate data (create all arrays and internal data):
        self.evalPyMatGen()
        self.evalValAndErr()

        #print(" - Size ot listAtomAll =", len(self.cifValAndErr.listAtomAll))
        # Third choose which to save from which crystal information:
        #print(" ==> ", self.cifValAndErr.unitCellLengths)
        self.mergeCrystInfo()

        #print(" - Size ot listAtomAll =", len(self.cifValAndErr.listAtomAll))
        #print(" - Size ot listAtomAll =", len(self.cifOutput.listAtomAll))

        #self.cifOutput = self.cifValAndErr
        #print(" -->  ", self.cifOutput.unitCellLengths)

        #self.loadCifZeolite(z)
        #self.evalCifData()

        return err_count
        # === end of CrystalInfo.load_cif_file()


    def load_cif_file_val_and_err(self, file_path, options=None):
        """ Load a CIF file and store the data in internal variables.

            options - is a dictionary with keys:
            ""?

            Returns the number of errors.
        """
        err_count = 0

        if self.in_black_list(file_path):
            logging.error(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            logging.error(" Badly formed cif file: '%s', skip it", file_path)
            logging.error(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            return 1

        if os.path.isfile(file_path):
            path = file_path
        if os.path.isfile(file_path.replace("--", "-")):
            path = file_path.replace("--", "-")
        else:
            logging.error(" CIF file does not exist: '%s'.", file_path)
            return 1

        #path = self.cleanCif(path)[0]

        # print("path =", path)
        name = "TODO-remove-name in crystalinfo"
        name = ""

        # First load data to (struct in cifPyMatGen and unitCellLengths in cifValAndErr)
        #self.loadPyMatGen( path, name)
        self.loadValAndErr(path, name)
        try:
            pass

        except :
            logging.error("=================================================")
            logging.error(" Failed to read data from '%s' cif file", path)
            logging.error("=================================================")
            with open("failed_cif_files.txt", "a", encoding="utf-8") as fp:
                fp.write(path + "\n")

        # Second evaluate data (create all arrays and internal data):
        #self.evalPyMatGen()
        self.evalValAndErr()

        #print(" - Size ot listAtomAll =", len(self.cifValAndErr.listAtomAll))
        # Third choose which to save from which crystal information:
        #print(" ==> ", self.cifValAndErr.unitCellLengths)
        self.mergeCrystInfo()

        #print(" - Size ot listAtomAll =", len(self.cifValAndErr.listAtomAll))
        #print(" - Size ot listAtomAll =", len(self.cifOutput.listAtomAll))

        #self.cifOutput = self.cifValAndErr
        #print(" -->  ", self.cifOutput.unitCellLengths)

        #self.loadCifZeolite(z)
        #self.evalCifData()

        return err_count
        # === end of CrystalInfo.load_cif_file()


    def get_csv_arr_from_cif(self, file_path,
                             name="CrystalInformation", new_uuid=None,
                             subject="", predicate="hasCrystalInformation"):
        """ A function to create a csv array for abox writer.
        Supports two main options:
        1) Create a new array based on the cif file (file_path),
           store the iri of this structure.
           Activated by input subject="", predicate="".
        2) provide a subject-preficate-iri for given file_path, if 
           such structure exists in the database.

        Here subject is the element of class crystal_uuid
        new_uuid - optional str parameter to specify the UUID of this instance
                   and it's sub-structures (unit cell, vectors, atoms, etc)
                   If not specified, then a new UUID value will be created.
        name     - Optional name of the instance. It will be generated as 
                   name + "_" + new_uuid.

        """
        #print("Cryst Info abox =", self.abox_prefix)

        output = []

        logging.info("Starting crystalinfo.csv_arr_material(), new_iri", new_uuid)
        print("Starting crystalinfo.csv_arr_material(), new_iri", new_uuid)

        if new_uuid is None:
            #print(123)
            # Check for existing uuids in the database of cif_files:
            #self.iri = get_cif_iri(file_path)
            self.iri, self.uuid = CIF_IRI_DATA.get_entry_iri(file_path)

            #print(file_path, self.iri)

            if not self.iri:
                self.iri, self.uuid = tools.addUUID(self.uuidDB.uuidDB,
                                                    crystOntoPrefix + "CrystalInformation",
                                                    "ZeoliteCIF_" + name)
        else:
            #print(456)
            self.uuid = new_uuid
            self.iri = name + "_" + self.uuid
        """
        """

        #print(">>> In crystalinfo: subj =", subject, "pred =", predicate, zeoOntoPrefix, self.iri)
        if subject != "" and predicate != "":
            output.append([zeoOntoPrefix + self.iri, "Instance",
                           crystOntoPrefix + "CrystalInformation", "", "", ""])
            output.append([subject, "Instance", zeoOntoPrefix + self.iri, predicate, "", ""])
            #print(">>> In crystalinfo: Added a single line for CIF iri")
            return output

        logging.info("Continue initializing CIF", file_path, self.iri)
        print("Full initialization of CIF", file_path, self.iri)

        if True:
            pass
        try:
            err_count = self.load_cif_file_py_mat_gen(file_path)
            pass
        except :    
            #logging.error("=================================================")
            logging.error(" Failed to read data by PyMatGen '%s', use ValAndErr", file)
            #logging.error("=================================================")

            err_count = self.load_cif_file_val_and_err(file_path)

            #if not got_symm:
            if has_xyz_line(file_path):
                with open("list_of_cif_fails.txt", "a", encoding="utf-8") as fp:
                    fp.write(file + "\n")

        if err_count > 0:
            logging.error(" Failed to load cif file '%s', aborting", file_path)

            with open("list_of_cif_fails.txt", "a", encoding="utf-8") as fp:
                fp.write(file + "\n")
            return output

        #print("Starting get_csv_arr_from_cif() for", file_path, name)
        # Output:
            #This does not work: there is no zeolite<->cif correspondance.

        if new_uuid:
            self.iri = self.abox_prefix + "CrystalInformation_"
            if name is not None and name != "":
                #self.iri += name + "_"
                pass
            self.iri += new_uuid
        else:
            self.iri, _ = self.uuidDB.addUUID(crystOntoPrefix + "CrystalInformation",
                                              self.abox_prefix + "CrystalInformation_" + name)

        if True:
            #print("         crystalinfo.csv_arr_material(), cif_iri", self.iri)

            # Define relation between the class instances:
            output.append([self.iri, "Instance",
                           crystOntoPrefix + "CrystalInformation", "", "", ""])

            #print("sublect =", subject, type(subject))
            #predicate = crystOntoPrefix + "hasCrystalInformation"
            #output.append([subject, "Instance", self.iri, predicate, "", ""])

            #self.loadUnitCellPyMatGen(zeoname)

            #arr += self.arrInitZeolite(uuid_zeolite)

            #print("self.iri =", self.iri, type(self.iri))
            tmp = self.arrUnitCell(self.iri,
                                   crystOntoPrefix + "hasUnitCell", name,
                                   new_uuid=new_uuid)
            if tmp is None:
                logging.warning(" Missing Unit Cell information!")
            else:
                output += tmp

            #FIXME
            tmp = self.arrTransform(self.iri, crystOntoPrefix +
                                    "hasCoordinateTransformation", name,
                                    new_uuid=new_uuid)
            if tmp is None:
                logging.warning(" Missing Transformation information!")
            else:
                output += tmp

            tmp = self.arrAtomSite(self.iri, crystOntoPrefix +
                                   "hasAtomicStructure", name,
                                   new_uuid=new_uuid)
            if tmp is None:
                logging.warning(" Missing Atom Site information!")
            else:
                output += tmp

            """
            print("starting arrTile for", name)
            tmp = self.arrTiles(self.iri, crystOntoPrefix + "hasTiledStructure", name)
            if tmp is None:
                logging.warning(" Missing Tiled Structure information!")
            else:
                output += tmp

            tmp = self.arrSpectrum(self.iri, crystOntoPrefix + "hasXRDSpectrum", name)
            if tmp is None:
                logging.warning(" Missing Spectrum information!")
            else:
                output += tmp
            # FIXME
            """
            #print("Finished hasCrystalInformation for", subject)
            #for line in output[:20]:
            #    print(line)
            #    pass
            #1/0

        return output
        # === end of CsvMaker.get_csv_arr_from_cif()

    """ Redundant??
    def loadUnitCellPyMatGen(self, zeoname):
        #TWOPI = 2 * math.pi

        # TODO to add get_crystal_system(), get_space_group_number(),
        #      https://pymatgen.org/pymatgen.symmetry.html

        path = os.path.join("CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

        #print("Using a test case 913885.cif for the UnitCell class")
        #path = os.path.join("test", "913885.cif")
        #dataIn = tools.readCsv(path)
        if not os.path.isfile(path):
            logging.error("File not found '%s'.", path)
            return

        structure = Structure.from_file(path)

        #print(zeoname)
        #print("  ", structure.lattice) # These are 3 unit cell vectors
        #print("  ", structure.lattice.gamma) # angles, similarly a,b,c
        #print("  ", structure.lattice.volume) # volume
        #print("  ", structure.lattice.reciprocal_lattice.a) # reciprocal parameters
        #print("  ", structure.lattice.reciprocal_lattice.matrix[0][0]) # reciprocal parameters
        #print("  ", structure.lattice.reciprocal_lattice.matrix) # reciprocal parameters
        #print("  ", structure.lattice.matrix) # reciprocal parameters
        #print(TWOPI * numpy.linalg.inv(structure.lattice.matrix))

        ###########################################
        ###########################################

        pass # CsvMaker.loadUnitCellPyMat()
    """

    def arrUnitCell(self, subject, predicate, zeoname, new_uuid=None):
        """
        subject   - Is the full hame of instance of class CrystalInformation,
                    which contains this UnitCell class.
        predicate - Is the Object Property linking the Subject and the current UnitCell.
                    Typically is should be equal to "hasUnitCell".
        """

        if subject.find("CrystalInformation") < 0 and subject.find("CIF") < 0:
            logging.warning(" Subject in arrUnitCell() is '%s', expecting" +
                            " the name to contain 'CrystalInfromation'.",
                            subject)

        if predicate.find("hasUnitCell") < 0:
            logging.warning(" Predicate in arrUnitCell() is '%s'," +
                            " but expecting 'hasUnitCell'.", predicate)

        output = []

        if new_uuid:
            uuid_cif_uc = self.abox_prefix + "UnitCell_"
            if zeoname is not None and zeoname != "":
                #uuid_cif_uc += zeoname + "_"
                pass
            uuid_cif_uc += new_uuid
        else:
            uuid_cif_uc, _ = self.uuidDB.addUUID(crystOntoPrefix + "UnitCell",
                                                 "UnitCell_" + zeoname)

        output.append([uuid_cif_uc, "Instance", crystOntoPrefix + "UnitCell", "", "", ""])

        output.append([subject, "Instance", uuid_cif_uc, predicate, "", ""])

        if self.cifOutput.unitCellLengths is not None:
            output += self.cifOutput.unitCellLengths.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasUnitCellLengths", new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell lengths for %s", uuid_cif_uc)

        if self.cifOutput.unitCellAngles is not None:
            output += self.cifOutput.unitCellAngles.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasUnitCellAngles", new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell angles for %s", uuid_cif_uc)

        if self.cifOutput.unitCellRecipLengths is not None:
            output += self.cifOutput.unitCellRecipLengths.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasReciprocalUnitCellLengths", new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell reciprocal lengths for %s", uuid_cif_uc)

        if self.cifOutput.unitCellRecipAngles is not None:
            output += self.cifOutput.unitCellRecipAngles.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasReciprocalUnitCellAngles", new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell reciprocal angles for %s", uuid_cif_uc)

        #output += self.cifOutput.unitCellVolume.get_csv_arr(uuid_cif_uc,
        #          self.crystOntoPrefix + "hasUnitCellVolume")

        if self.cifOutput.unitCellVolume is not None:
            output += self.cifOutput.unitCellVolume.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasUnitCellVolume", new_uuid=new_uuid)
        else:
            logging.warning(" Missing volume for %s", uuid_cif_uc)
            pass

        #################################################
        # Vector to keep three Unit Cell vectors (a,b,c):
        if new_uuid:
            uuid_uc_vec_abc = self.abox_prefix + "UnitCellVectorSet" + \
                              "_" + new_uuid
                              # "_" + zeoname +
        else:
            uuid_uc_vec_abc, _ = self.uuidDB.addUUID(crystOntoPrefix + "UnitCellVectorSet",
                                                     "UnitCellVectorSet_" + zeoname)
        output.append([uuid_uc_vec_abc, "Instance", crystOntoPrefix + "UnitCellVectorSet", "", "", ""])

        output.append([uuid_cif_uc, "Instance", uuid_uc_vec_abc,
                       crystOntoPrefix + "hasUnitCellVectorSet", "", ""])

        if self.cifOutput.unitCellVectorA:
            output += self.cifOutput.unitCellVectorA.get_csv_arr(uuid_uc_vec_abc,
                                     crystOntoPrefix + "hasUnitCellVector", new_uuid)
        else:
            print("No unitCellVectorA 12345")
        if self.cifOutput.unitCellVectorB:
            output += self.cifOutput.unitCellVectorB.get_csv_arr(uuid_uc_vec_abc,
                                     crystOntoPrefix + "hasUnitCellVector", new_uuid)
        if self.cifOutput.unitCellVectorC:
            output += self.cifOutput.unitCellVectorC.get_csv_arr(uuid_uc_vec_abc,
                                     crystOntoPrefix + "hasUnitCellVector", new_uuid)

        # Vector to keep three Reciprocal Unit Cell vectors (a,b,c):

        if new_uuid:
            uuid_uc_r_vec_abc = self.abox_prefix + "ReciprocalUnitCellVectorSet" + \
                                "_" + new_uuid
                                #"_" + zeoname + 
        else:
            uuid_uc_r_vec_abc, _ = self.uuidDB.addUUID(crystOntoPrefix + "UnitCellVectorSet",
                                                       "ReciprocalUnitCellVectorSet_" + zeoname)

        output.append([uuid_uc_r_vec_abc, "Instance", crystOntoPrefix + "UnitCellVectorSet", "", "", ""])

        output.append([uuid_cif_uc, "Instance", uuid_uc_r_vec_abc,
                       crystOntoPrefix + "hasReciprocalUnitCellVectorSet", "", ""])
        if self.cifOutput.unitCellRecipVectorA:
            output += self.cifOutput.unitCellRecipVectorA.get_csv_arr(uuid_uc_r_vec_abc,
                      crystOntoPrefix + "hasUnitCellVector", new_uuid)
        else:
            print("No unitCellRecipVectorA 12345")
        if self.cifOutput.unitCellRecipVectorB:
            output += self.cifOutput.unitCellRecipVectorB.get_csv_arr(uuid_uc_r_vec_abc,
                      crystOntoPrefix + "hasUnitCellVector", new_uuid)
        if self.cifOutput.unitCellRecipVectorC:
            output += self.cifOutput.unitCellRecipVectorC.get_csv_arr(uuid_uc_r_vec_abc,
                      crystOntoPrefix + "hasUnitCellVector", new_uuid)

        if self.cifOutput.symmLatticeSystem is not None:
            output.append([crystOntoPrefix + "hasLatticeSystem",
                           "Data Property", uuid_cif_uc, "",
                           self.cifOutput.symmLatticeSystem, "string"])
        else:
            logging.warning(" Missing hasLatticeSystem value")


        if self.cifOutput.symmSpaceGroupHM is not None:
            output.append([crystOntoPrefix + "hasSpaceGroupHM",
                           "Data Property", uuid_cif_uc, "",
                           self.cifOutput.symmSpaceGroupHM, "string"])
        else:
            logging.warning(" Missing hasSpaceGroupHM value")


        if self.cifOutput.symmITNumber is not None:
            #output += self.cifOutput.symmITNumber.getArr(uuid_uc_r_vec_abc,
            #          self.crystOntoPrefix + "hasSymmetryNumber")

            output.append([crystOntoPrefix + "hasSymmetryNumber",
                           "Data Property", uuid_cif_uc, "",
                           self.cifOutput.symmITNumber, "integer"])

            #print("IT number", self.cifOutput.symmITNumber, type(self.cifOutput.symmITNumber))

            sym_code = crystaldata.SPACE_GROUP_SYMBOL[self.cifOutput.symmITNumber]
            #print("sym_code", sym_code)

            output.append([crystOntoPrefix + "hasSpaceGroupSymbol",
                           "Data Property", uuid_cif_uc, "",
                           sym_code, "string"])

        else:
            logging.warning(" Missing hasSymmetryNumber value")

        # The symmetry information of the unit cell.
        # TODO to add get_crystal_system(), get_space_group_number(),
        #      https://pymatgen.org/pymatgen.symmetry.html

        return output
        pass # CsvMaker.arrUnitCell()

    '''
    def arrUnitCellOld(self, subject, predicate, zeoname):

        """
        subject   - Is the full hame of instance of class CrystalInformation,
                    which contains this UnitCell class.
        predicate - Is the Object Property linking the Subject and the current UnitCell.
                    Typically is should be equal to "hasUnitCell".
        """

        path = os.path.join("CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")
        if not os.path.isfile(path):
            logging.error("File not found '%s'.", path)
            return

        structure = Structure.from_file(path)

        output = []
        #print("'" + subject + "'", "'" + predicate + "'")
        if subject.find("CrystalInformation") < 0:
            logging.warning(" Subject in arrUnitCell() is '%s'," +
                            " expecting the name to contain 'CrystalInfromation'.", subject)

        if predicate.find("hasUnitCell") < 0:
            logging.warning(" Predicate in arrUnitCell() is '%s'," +
                            " but expecting 'hasUnitCell'.", predicate)

        uuid_cif_uc = tools.getUUID(self.uuidDB.uuidDB,
                                    "UnitCell", "UnitCell_" + zeoname)
        output.append([uuid_cif_uc, "Instance", "UnitCell", "", "", ""])

        output.append([subject, "Instance", uuid_cif_uc,
                       predicate, "", ""])

        # Unit Cell volume, single value, not vector:
        uuid_uc_volume = tools.getUUID(self.uuidDB.uuidDB,
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure",
                       "UnitCellVolume_" + zeoname)

        output.append([uuid_uc_volume, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure",
                       "", "", ""])

        output.append([uuid_cif_uc, "Instance", uuid_uc_volume,
                       self.crystOntoPrefix + "hasUnitCellVolume", "", ""])

        output.append(["http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue",
                       "Data Property", uuid_uc_volume, "",
                       round(structure.lattice.volume, 4) , "decimal"])
                       # FIXME

        output.append([uuid_uc_volume, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/cubicAngstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                        "", ""])

    ###########################################

    # The symmetry information of the unit cell.
    # TODO to add get_crystal_system(), get_space_group_number(),
    #      https://pymatgen.org/pymatgen.symmetry.html


        sga = pymatgen.symmetry.analyzer.SpacegroupAnalyzer(structure)

        #print("Zeo Name = ", zeoname)
        if isinstance(sga._space_group_data, dict):
            if "number" in sga._space_group_data.keys():
                """
              print("SG number:", sga._space_group_data["number"],
                    sga.get_space_group_number(), sga.get_crystal_system())
              print("   ", #sga._get_symmetry(),
                          sga.get_hall(),
                          sga.get_lattice_type(), #sga.get_symmetry_dataset()
                          #sga.int_symbol()
                          #sga._abc_impl
      )
                """
                #if isinstance(sga.get_crystal_system(), str) :
                output.append([self.crystOntoPrefix + "hasLatticeSystem",
                               "Data Property", uuid_cif_uc, "",
                               sga.get_crystal_system() , "string"])
                output.append([self.crystOntoPrefix + "hasSymmetryNumber",
                               "Data Property", uuid_cif_uc, "",
                               sga.get_space_group_number() , "integer"])

        return output
        pass # CsvMaker.arrUnitCellOld()
    '''

    def arrTransform(self, subject, predicate, zeoname, new_uuid=None):
        if subject.find("CrystalInformation") < 0 and subject.find("CIF") < 0:
            logging.warning(" Subject in arrCoordinateTransformation() is" +
                            " '%s', expecting the name to contain" +
                            " 'CrystalInfromation'.", subject)

        if predicate.find("hasCoordinateTransformation") < 0:
            logging.warning(" Predicate in arrTransform() is '%s', but" +
                            " expecting 'hasCoordinateTransformation'.", predicate)

        output = []

        if new_uuid:
            uuid_cif_core_trans = self.abox_prefix + "CoordinateTransformation_" + new_uuid
        else:
            uuid_cif_core_trans, _ = self.uuidDB.addUUID(crystOntoPrefix + "CoordinateTransformation",
                                                     "CoordinateCIFCoreTransform_" + zeoname)
    
        output.append([uuid_cif_core_trans, "Instance", crystOntoPrefix + "CoordinateTransformation", "", "", ""])
        output.append([subject, "Instance", uuid_cif_core_trans, predicate, "", ""])

        if self.cifOutput.matrixFracToCart:
            output += self.cifOutput.matrixFracToCart.get_csv_arr(uuid_cif_core_trans,
                      crystOntoPrefix + "hasTransformationMatrixToCartesian", new_uuid=new_uuid)

        if self.cifOutput.vectorFracToCart:
            output += self.cifOutput.vectorFracToCart.get_csv_arr(uuid_cif_core_trans,
                      crystOntoPrefix + "hasTransformationVectorToCartesian", new_uuid=new_uuid)

        if self.cifOutput.matrixCartToFrac:
            output += self.cifOutput.matrixCartToFrac.get_csv_arr(uuid_cif_core_trans,
                      crystOntoPrefix + "hasTransformationMatrixToFractional", new_uuid=new_uuid)

        if self.cifOutput.vectorCartToFrac:
            output += self.cifOutput.vectorCartToFrac.get_csv_arr(uuid_cif_core_trans,
                      crystOntoPrefix + "hasTransformationVectorToFractional", new_uuid=new_uuid)


        return output
        ################# Fractional to Cartesian ########################
        #uuid_m_frac_to_cart, _ = self.uuidDB.addUUID(crystOntoPrefix + "TransformationMatrix",
        #                          zeoname + "_TransfMatrixToCart")


        # === end of CsvMaker.arrTransform()

    def arrTransformOld(self, subject, predicate, zeoname, new_uuid=None):
        """
        subject   - Is the full hame of instance of class CrystalInformation,
                    which contains this CoordinateTransformation class.
        predicate - Is the Object Property linking the Subject and the current
                    CoordinateTransformation.
                    Typically is should be equal to "hasCoordinateTransformation".
        """

        if subject.find("CrystalInformation") < 0 and subject.find("CIF") < 0:
            logging.warning(" Subject in arrCoordinateTransformation() is" +
                            " '%s', expecting the name to contain" +
                            " 'CrystalInfromation'.", subject)

        if predicate.find("hasCoordinateTransformation") < 0:
            logging.warning(" Predicate in arrTransform() is '%s', but" +
                            " expecting 'hasCoordinateTransformation'.", predicate)

        #print("arrTransform started1)
        TWOPI = 2 * math.pi
        DIRS = "xyz"

        output = []

        structure = self.cifPyMatGen.struct
        #structure = Structure.from_file(path)

        uuid_cif_core_trans, _ = self.uuidDB.addUUID(crystOntoPrefix + "CoordinateTransformation",
                                                     "CoordinateCIFCoreTransform_" + zeoname)

        output.append([uuid_cif_core_trans, "Instance", crystOntoPrefix + "CoordinateTransformation", "", "", ""])
        output.append([subject, "Instance", uuid_cif_core_trans, predicate, "", ""])

        ################# Fractional to Cartesian ########################
        uuid_m_frac_to_cart, _ = self.uuidDB.addUUID(crystOntoPrefix + "TransformationMatrix",
                                  zeoname + "_TransfMatrixToCart")

        output.append([uuid_m_frac_to_cart, "Instance",
                       crystOntoPrefix + "TransformationMatrix", "", "", ""])
        output.append([uuid_cif_core_trans, "Instance", uuid_m_frac_to_cart,
                       crystOntoPrefix + "hasTransformationMatrixToCartesian", "", ""])

        output.append([uuid_m_frac_to_cart, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                       "", ""])

        for iy in range(3):
            for ix in range(3):
                uuid_m_comp, _ = self.uuidDB.addUUID(crystOntoPrefix + "MatrixComponent",
                            "MatrixComponentToCartesian"+str(ix)+str(iy)+"_" + zeoname)
                output.append([uuid_m_comp, "Instance",
                               crystOntoPrefix + "MatrixComponent", "", "", ""])
                               #"CIFCoreTransformationMatrixToCartesian", "", "", ""])
                output.append([uuid_m_frac_to_cart, "Instance", uuid_m_comp,
                               crystOntoPrefix + "hasMatrixComponent", "", ""])

                output.append([crystOntoPrefix + "hasComponentLabel", "Data Property",
                                 uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string"])

                output.append([crystOntoPrefix + "hasRowIndex", "Data Property",
                                 uuid_m_comp, "", iy, "xsd:integer"])

                output.append([crystOntoPrefix + "hasColumnIndex", "Data Property",
                                 uuid_m_comp, "", ix, "xsd:integer"])

                if structure is not None:
                    output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                                   uuid_m_comp, "",
                                   round(structure.lattice.matrix[ix][iy], 12), "decimal"])

        uuid_v_frac_to_cart, _ = self.uuidDB.addUUID(crystOntoPrefix + "Vector",
                                 zeoname + "_TransfVectorToCart")
        output.append([uuid_v_frac_to_cart, "Instance",
                       crystOntoPrefix + "PositionVector", "", "", ""])
        output.append([uuid_cif_core_trans, "Instance", uuid_v_frac_to_cart,
                       crystOntoPrefix + "hasTransformationVectorToCartesian", "", ""])

        output.append([uuid_v_frac_to_cart, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                       "", ""])

        for ix in range(3):
            uuid_v_comp = self.uuidDB.getUUID(crystOntoPrefix + "VectorComponent",
                          "VectorComponentToCartesian" + str(ix) + "_" + zeoname)
            output.append([uuid_v_comp, "Instance",
                           crystOntoPrefix + "VectorComponent", "", "", ""])
                           #"CIFCoreTransformationVectorToCartesian", "", "", ""])
            output.append([uuid_v_frac_to_cart, "Instance", uuid_v_comp,
                           crystOntoPrefix + "hasVectorComponent", "", ""])

            output.append([crystOntoPrefix + "hasComponentLabel", "Data Property",
                           uuid_v_comp, "", DIRS[ix], "string"])

            output.append([crystOntoPrefix + "hasComponentIndex", "Data Property",
                           uuid_v_comp, "", ix, "xsd:integer"])

            output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                           uuid_v_comp, "",
                           0.0, "decimal"])

        ################# Cartesian to Fractional ########################
        uuid_m_cart_to_frac, _ = self.uuidDB.addUUID(crystOntoPrefix + "TransformationMatrix",
                                 zeoname + "TransfMatrixToFrac")
        output.append([uuid_m_cart_to_frac, "Instance",
                       crystOntoPrefix + "TransformationMatrix", "", "", ""])
                               #"CIFCoreTransformationMatrixToCartesian", "", "", ""])
        output.append([uuid_cif_core_trans, "Instance", uuid_m_cart_to_frac,
                       crystOntoPrefix + "hasTransformationMatrixToFractional", "", ""])

        output.append([uuid_m_cart_to_frac, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                       "", ""])

        for iy in range(3):
            for ix in range(3):
                uuid_m_comp, _ = self.uuidDB.addUUID(crystOntoPrefix + "MatrixComponent",
                            "MatrixComponentToFractional"+str(ix)+str(iy)+"_" + zeoname)
                output.append([uuid_m_comp, "Instance",
                               crystOntoPrefix + "MatrixComponent", "", "", ""])
                               #"CIFCoreTransformationMatrixToCartesian", "", "", ""])
                output.append([uuid_m_cart_to_frac, "Instance", uuid_m_comp,
                               crystOntoPrefix + "hasMatrixComponent", "", ""])

                output.append([crystOntoPrefix + "hasComponentLabel", "Data Property",
                               uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string"])

                output.append([crystOntoPrefix + "hasRowIndex", "Data Property",
                               uuid_m_comp, "", iy, "xsd:integer"])

                output.append([crystOntoPrefix + "hasColumnIndex", "Data Property",
                               uuid_m_comp, "", ix, "xsd:integer"])

                if structure is not None:
                    output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                                   uuid_m_comp, "",
                                   round(structure.lattice.reciprocal_lattice.matrix[iy][ix]/TWOPI, 12), "decimal"])

        uuid_v_cart_to_frac, _ = self.uuidDB.addUUID(crystOntoPrefix + "PositionVector",
                              zeoname + "_TransfVectorToFrac")
        output.append([uuid_v_cart_to_frac, "Instance",
                       crystOntoPrefix + "PositionVector", "", "", ""])
                       #"CIFCoreTransformationVectorToFractional", "", "", ""])

        output.append([uuid_cif_core_trans, "Instance", uuid_v_cart_to_frac,
                       crystOntoPrefix + "hasTransformationVectorToFractional", "", ""])

        output.append([uuid_v_cart_to_frac, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                       "", ""])

        for ix in range(3):
            uuid_v_comp, _ = self.uuidDB.addUUID("VectorComponent",
                        "VectorComponentToFractional" + str(ix) + "_" + zeoname)
            output.append([uuid_v_comp, "Instance",
                           "VectorComponent", "", "", ""])
                           #"CIFCoreTransformationVectorToCartesian", "", "", ""])
            output.append([uuid_v_cart_to_frac, "Instance", uuid_v_comp,
                           crystOntoPrefix + "hasVectorComponent", "", ""])

            output.append([crystOntoPrefix + "hasComponentLabel", "Data Property",
                           uuid_v_comp, "", DIRS[ix], "string"])

            output.append([crystOntoPrefix + "hasComponentIndex", "Data Property",
                           uuid_v_comp, "", ix, "xsd:integer"])

            output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                           uuid_v_comp, "",
                           0.0, "decimal"])

        return output
        # === end of CsvMaker.arrTransform()

    def arrAtomSite(self, subject, predicate, zeoname, new_uuid=None):
        """
        subject   - Is the full hame of instance of class CrystalInformation,
                    which contains this UnitCell class.
        predicate - Is the Object Property linking the Subject and the current UnitCell.
                    Typically is should be equal to "hasUnitCell".
        """

        if subject.find("CrystalInformation") < 0 and subject.find("CIF") < 0:
            logging.warning(" Subject in arrAtomSite() is '%s'," +
                            " expecting the name to contain" +
                            " 'CrystalInfromation' or 'CIF'.", subject)

        if predicate.find("hasAtomicStructure") < 0:
            logging.warning(" Predicate in arrAtomSite() is '%s'," +
                            " but expecting 'hasAtomicStructure'.", predicate)

        #logging.warning("arrAtomSite() is not implemented yet")

        output = []

        if new_uuid:
            uuid_atomic = self.abox_prefix + "AtomicStructure_"
            if zeoname is not None and zeoname != "":
                #uuid_atomic += zeoname + "_"
                pass
            uuid_atomic += new_uuid
        else:
            uuid_atomic, _ = self.uuidDB.addUUID(crystOntoPrefix + "AtomicStructure",
                                                 "AtomicStructure_" + zeoname)

        #print(">>>>> in getArrAtom:", uuid_atomic)
        output.append([uuid_atomic, "Instance", crystOntoPrefix + "AtomicStructure", "", "", ""])
        output.append([subject, "Instance", uuid_atomic, predicate, "", ""])

        #print("Size ot listAtomAll =", len(self.cifValAndErr.listAtomAll))
        for ia, atom in enumerate(self.cifValAndErr.listAtomAll):
            #print(">>>> getArrAtom", ia, atom.element)
            output += atom.getArrAtom(uuid_atomic, crystOntoPrefix + "hasAtomSite",
                                      label = str(ia), new_uuid=new_uuid)
            pass

        return output
        # === end of CsvMaker.arrAtomSite()

    def loadPyMatGen(self, path, name):
        """
        Load the CIF by the standard PyMatGen
        and save it to CrystalInformation class.
        """

        #logging.error(" Not implemented def CsvMaker.loadPyMatGen(self, path): ")

        #if self.cifPyMatGen:
        if True:
            self.cifPyMatGen = crystaldata.CrystalData("PyMatGen", self.uuidDB,
                                                       abox_prefix=self.abox_prefix)
            self.cifPyMatGen.loadData(path, name)

        # === end of CsvMaker.loadPyMatGen()

    def loadValAndErr(self, path, name):
        """
        Load the CIF data written by this hand-made class (with uncertainty)
        and save it to CrystalInformation class.
        """

        #logging.error(" Not implemented def loadValAndErr(self, path): ")

        #if self.cifValAndErr:
        if True:
            self.cifValAndErr = crystaldata.CrystalData("ValAndErr", self.uuidDB,
                                                        abox_prefix=self.abox_prefix)
            self.cifValAndErr.listAtomRaw = []
            self.cifValAndErr.listAtomAll = []
            self.cifValAndErr.loadData(path, name)

        #print(self.cifValAndErr.unitCellLengths)
        # === end of CsvMaker.loadValAndErr()

    def evalPyMatGen(self):
        if self.cifPyMatGen:
            self.cifPyMatGen.evalPyMatGen()

        # === end of CsvMaker.evalPyMatGen()

    def evalValAndErr(self):
        if self.cifValAndErr:
            self.cifValAndErr.evalValAndErr()

        # === end of CsvMaker.evalValAndErr()

    def mergeCrystInfo(self):
        """
        Run through both versions: cifPyMatGen and cifValAndErr and fix by rules:
        j) For cifValAndErr use math to compute not available data,
        2) I there is uncertainty - use it,
        3) If only one CIF has data - use it,
        4)

        """

        #print(self.cifValAndErr.unitCellLengths)
        if self.cifPyMatGen:
            print("pymatget to output crystal")
            self.cifOutput = self.cifPyMatGen
        else:
            print("starting ValAndErr merge")
            self.cifOutput = crystaldata.CrystalData("ValAndErr", self.uuidDB,
                                                     abox_prefix=self.abox_prefix)
            self.cifOutput = self.cifValAndErr

        attrs = [
                 #"unitCellLengths", "unitCellRecipLengths",
                 "symmLatticeSystem", "symmITNumber", #"", "",
                 "unitCellLengths",  "unitCellRecipLengths",
                 "unitCellAngles",  "unitCellRecipAngles",
                 "unitCellVectorA",      "unitCellVectorB",      "unitCellVectorC",
                 "unitCellRecipVectorA", "unitCellRecipVectorB", "unitCellRecipVectorC",
                 "unitCellVolume",
]
        """
                "listAtomRaw", "listAtomAll", "listAtomSymm",
                # other properties:
                "cifStandard", "loopHeaders",
        """

        for attr in attrs:
            self._add_val_and_err_to_output(attr)
            pass

        '''
        self.cifOutput.unitCellLengths      = self.cifValAndErr.unitCellLengths
        self.cifOutput.unitCellAngles       = self.cifValAndErr.unitCellAngles
        self.cifOutput.unitCellRecipLengths = self.cifValAndErr.unitCellRecipLengths
        self.cifOutput.unitCellRecipAngles  = self.cifValAndErr.unitCellRecipAngles
        self.cifOutput.unitCellVolume       = self.cifValAndErr.unitCellVolume
        '''

        #logging.error(" Not implemented def mergeCrystInfo(self): ")
        #print(" >>> ", self.cifPyMatGen.listAtomAll)

        # === end of CsvMaker.mergeCrystInfo()

    def _add_val_and_err_to_output(self, attr):

        if not hasattr(self.cifOutput, attr):
            logging.warning(" Missing attribute '%s' in cifPyMatGen", attr)

        value = None
        if hasattr(self.cifValAndErr, attr):
            value = getattr(self.cifValAndErr, attr)

        if hasattr(self.cifOutput, attr):
            if getattr(self.cifOutput, attr) is None:
                if value is not None:
                    setattr(self.cifOutput, attr, value)
        else:
            setattr(self.cifOutput, attr, value)

        # === end of CsvMaker._add_val_and_err_to_output()

    def getCifLineRanges(self, fileIn):

        return [0, 100, 400]
        # === end of

    def cleanCif(self, file_in, dir_out=""):
        """
        There are three situations when CIF file is not directly readable:
        1) Single file contains several CIF data structures (concatenation of several files)
        2) CIF file contains uncertainties (a.k.a. error bars)
        3) CIF file contains unreadable characters or misprints. Also <br>.

        In all cases I read the file line-by-line and save new (corrected) file(s)
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
        lineRanges = tools.getCifLineRanges(file_in)
        files_out = []

        if not os.path.isfile(file_in):
            logging.error(" In cleanCif(): input file '%s' does not exist.", file_in)
            return files_out


        if "" == dir_out:
            tmp_dir = "tmp"
        else:
            tmp_dir = dir_out

        if os.path.isdir(tmp_dir):
            # Do nothing
            pass
        elif os.path.isfile(tmp_dir):
            logging.error(" Cannot create dir '%s', because there is a file" +
                          "with same name.", tmp_dir)
        else:
            os.makedirs(tmp_dir)

        # 1) Remove <br> from the file:
        with open(file_in, encoding="utf-8") as fp:
            lines = fp.readlines()
            # print("In cleanCif(): n lines =", len(lines))

        file_base, ext = os.path.splitext(os.path.basename(file_in))
        i = 0
        file_out = os.path.join(tmp_dir, "temp_" + str(i+1) + "_" + file_base + ext)

        time.sleep(0.1)
        with open(file_out, "w", encoding="utf-8") as fp:
            for line in lines:
                tmp = line.replace("<br>", "\n")
                tmp = tmp.rstrip()
                fp.write(tmp + "\n")
        files_out.append(file_out)

        return files_out

        # TODO
        # 2) Remove brackets, i.e. the uncertainty:
        for i in range(len(lineRanges)-1):
            #print("i =", i, ", ", lineRanges[i])

            # TODO?
            n_bracket = self.readWithUncertainties(file_in, "zeoname",
                                                  lineRanges[i], lineRanges[i+1])
            if (n_bracket > 0) or (len(lineRanges) - 1 > 1):
                file_base, ext = os.path.splitext(os.path.basename(file_in))
                # print("Input file name and extension:", fileBase, ext)

                file_out = os.path.join(tmp_dir, file_base + "_" + str(i+1) + ext)
                files_out.append(file_out)
                self.readWithUncertainties(file_in, "zeoname", lineRanges[i],
                                           lineRanges[i+1], file_out = fileOut)

        #else:
        #fileOut = "after-913885.cif"

        return files_out
        # === end of CsvMaker.cleanCif()

    # === end of class CrystalData

def list_files_with_extension(directory, extension):
    if not os.path.isdir(directory):
        print(f"Missing directory '{directory}' in crystalInfo")
    files = []
    for file in os.listdir(directory):
        if file.endswith(extension):
            files.append(file)
    return files

if __name__ == "__main__":
    # Algorithm to process the cif files:
    # 1. Get a directory with cif files and the list of files

    #files = []
    #tmp = list_files_with_extension( os.path.join("LI-CIF"), ".cif")
    #for f in tmp[:]:
    #    files.append(os.path.join("LI-CIF", f))

    #files += list_files_with_extension( os.path.join("CIF"), ".cif")
    #files += list_files_with_extension( os.path.join("ccdcfiles"), ".cif")

    files = []
    data = tools.readCsv("a_cifs.csv")
    data = data[1:]  # <= removed the header line
    for line in data[:]:
        if line[5] != "" and line[5].lower() != "none":
            if line[5].lower() not in files:
                files += line[5].lower().split()

    print("Found files:", len(files))
    files = list(set(files))
    print("Found files:", len(files))

    tmp = list(files)
    tmp.sort()
    for i_line in range(len(tmp)-1):
        line = tmp[i_line]
        if line.lower() == tmp[i_line+1].lower():
            print("Repeating file ", line)
            pass
    #1/0
    # FIXME
    #print("testing for aaaaaaaaaaaaaaaaaaaaa")
    #files = files[:200]
    #files = files[:20]
    #files = files[246+35+320+30+85+225+55+54+370+230:]
    #files = ["cifextra\\1169950.cif"]
    '''
    files = ["CIF\\AFI.cif",
             "CIF\\ANO.cif",
             "CIF\\AVE.cif"   ]
    files = []
    with open("list_of_cif_fails-4.txt", encoding="utf-8") as fp:
        for line in fp:
            if len(line.strip()) > 0:
                files.append(line.strip())
    '''

    # 2. Start processing files one by one:
    #    2a. If size exceeds size 20Mb move to the next folder
    #    2b. Keep a list of CIF uuid
    #    2c. Once too many lines in the output - create a new file

    DB_FOLDER = "crystals"
    if not os.path.isdir(DB_FOLDER):
        os.mkdir(DB_FOLDER)

    CSV_FOLDER = os.path.join(DB_FOLDER, "csv")
    if not os.path.isdir(CSV_FOLDER):
        os.mkdir(CSV_FOLDER)

    UUID_FOLDER = os.path.join(DB_FOLDER, "uuid")
    if not os.path.isdir(UUID_FOLDER):
        os.mkdir(UUID_FOLDER)

    output = []
    output += tools.get_csv_init("base",
                                 t_prefix=crystOntoPrefix + "OntoCrystal.owl",
                                 a_prefix=crystOntoPrefix)

    #print("ssssssssss")
    CIF_IRI_LIST = []  # TODO possibility to load from file for extension.
    db_count = 0
    CIF_IRI_DATA = CifIriData()
    CIF_IRI_DATA.self_test()

    #print("ssssssssss")
    uuid_file = os.path.join(UUID_FOLDER, "crystal_uuid_" + str(db_count) +".csv")
    uuidDB = tools.UuidDB(filename=uuid_file)
    #print("ssssssssss")
    for i_file, file in enumerate(files):
        print("----------------------------------------------------------")
        print("PyMatGenStarting cif ", i_file, " of ", len(files), ": '", file, "'.", sep="")

        i_cif = len(CIF_IRI_LIST)

        #uuid_str = str(uuid.uuid4())
        #uuid_str = tools.new_uuid()
        iri, uuid_str = CIF_IRI_DATA.get_entry_iri(file)
        if iri is None and uuid_str is None:
            CIF_IRI_DATA.set_entry(file, db_count)
            iri, uuid_str = CIF_IRI_DATA.get_entry_iri(file)

        cryst = CrystalInfo(uuidDB=uuidDB, abox_prefix=zeoOntoPrefix)
        #iri = "CrystalInformation_" + uuid_str
        CIF_IRI_LIST.append([file, i_file, iri, uuid_str, db_count])

        cif_name = "CIF_" + str(i_file)
        tmp = cryst.get_csv_arr_from_cif(file, cif_name,
                                         subject="", predicate="",
                                         new_uuid=uuid_str)
        output += tmp


        if len(output) > 150000 or i_file == len(files) - 1:
            file_out = os.path.join(CSV_FOLDER, "cif_twa_" + str(db_count) + ".csv")
            tools.writeCsv(file_out, output)
            output = []
            output += tools.get_csv_init("base",
                                         t_prefix=crystOntoPrefix + "OntoCrystal.owl",
                                         a_prefix=crystOntoPrefix)
            db_count += 1

            # Update the uuid database:
            uuidDB.saveDB()
            uuid_file = os.path.join(UUID_FOLDER, "crystal_uuid_" + str(db_count) +".csv")
            if i_file < len(files) - 1:
                uuidDB = tools.UuidDB(filename=uuid_file)

            # Regularly saving the iri:
            #tools.writeCsv(CIF_IRI_FILE, CIF_IRI_LIST)

        #cryst.get_uuid()
    CIF_IRI_DATA.save()

    pass
CIF_IRI_FILE = "cif_iri_list.csv"  # Format: filename, folder, iri, uuid
CIF_IRI_LIST = None
