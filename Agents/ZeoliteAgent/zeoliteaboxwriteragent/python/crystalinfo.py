"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import os
import sys
# import math
import time
import logging
import argparse

import tools
import crystaldata
# import ontocrystal_datatypes as ocdt
import xrd_spectrum as xrds
import xrd_simulation

# logging.basicConfig(level=logging.DEBUG)
# logging.basicConfig(level=logging.INFO)
logging.basicConfig(level=logging.WARNING)
# logging.basicConfig(level=logging.ERROR)


crystOntoPrefix = "https://www.theworldavatar.com/kg/ontocrystal/"


CIF_IRI_FILE = os.path.join("cif_iri_list.csv")


def read_command_line():
    parser = argparse.ArgumentParser(description="")

    parser.add_argument("--cif", type=str, default="",
                        help="File name or a directory containing cif files. " +
                        "Required argument.")
    parser.add_argument("--outDir", type=str, default="",
                        help="Directory to save the csv files. " +
                        "Default: the same folder as the input cif.")
    parser.add_argument("--xrdDir", type=str, default="",
                        help="Directory to save the .xrd files. " +
                        "Default: 'xrd'.")
    parser.add_argument("--abox-prefix", type=str, default="",
                        help="The prefix for the entities in the abox. " +
                        "Default: no prefix.")
    parser.add_argument("--flags", type=str, default="CURVTAX",
                        help="The flags to specify what to save to the abox csv file. " +
                        "Default: CX.")

    return parser.parse_args()
    # === end of read_command_line()


def has_xyz_line(file_path):
    """ Some keywords in CIF standard have been changed over the versions.
    This function checks the keywords related to the symmetry position.
    """
    with open(file_path, encoding="utf-8") as fp:
        got_symm = False
        for line in fp:
            if "_symmetry_equiv_pos_as_xyz" in line or \
               "_space_group_symop_operation_xyz" in line:
                got_symm = True
                break
    return got_symm


class CifIriData:
    """
    A safe way to create iri for cif and generate if necessary.

    Data structure to convert a given file_path to iri,
    based on the database saved in file CIF_IRI_FILE.
    Main functions:
    get_entry_iri(cif_path) - return iri and uuid for given cif_path,
                              from existing entry.
                              Return None,None,None if it does not exist.
                              or create a new if it does not exist
    set_entry(cif_path, cif_iri, xrd_iri) - assign iri and uuid for given cif_path.
    """

    def __init__(self, filename=None):
        """
        """
        if filename is None:
            self.filename = CIF_IRI_FILE
        else:
            self.filename = filename

        self._load()

        # === end of CifIriData.__init__()

    def _load(self):
        """
        """

        if os.path.isfile(self.filename):
            self.data = tools.readCsv(self.filename)
            if len(self.data) > 1:
                self.data = self.data[1:]
        else:
            self.data = []
            logging.warning(" Not found file '%s', creating new file.", self.filename)

        return self.data
        # === end of CifIriData._load()

    def save(self, filename=None):
        """ Save the current data to filename.
        If no filename provided - save to
        """
        header = [["CIF file", "CIF IRI", "CIF UUID", "XRD file", "XRD IRI"]]
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
                # Do nothing. Correct behaviour.
                pass
            else:
                # Do nothing. Not implemented.
                pass

        ###### Self test 3: check whether the file exist?

        return err_count
        # === end of CifIriData.self_test()

    def set_entry(self, cif_path, cif_iri, cif_uuid, xrd_path, xrd_iri):
        """ Change an existing or add a new line to data base.
        """

        cif_path = cif_path.replace("/", "\\").lower().strip()
        xrd_path = xrd_path.replace("/", "\\").lower().strip()

        found = False
        for i_line, line in enumerate(self.data):
            if line[0] == cif_path:
                found = True
                break

        if not found:
            cif_uuid = tools.new_uuid()
            cif_iri = crystOntoPrefix + "CrystalInformation_" + cif_uuid

            xrd_uuid = tools.new_uuid()
            xrd_iri = "XRDSpectrum_" + xrd_uuid
            data = [cif_path, cif_iri, cif_uuid, xrd_path, xrd_iri]
            self.data.append(data)

        # === end of CifIriData.set_entry()

    def get_entry_iri(self, cif_path):
        """
        """
        cif_iri = None
        xrd_iri = None
        cif_uuid = None
        xrd_uuid = None
        cif_path = cif_path.replace("/", "\\").lower().strip()
        for line in self.data:
            if line[0].lower().strip() == cif_path:
                cif_iri  = line[1].strip()
                cif_uuid = line[2].strip()
                xrd_iri  = line[3].strip()
                xrd_uuid = line[4].strip()
                break

        return cif_iri, cif_uuid, xrd_iri, xrd_uuid
        # === end of CifIriData.get_entry_iri()

    def del_entry(self, cif_path):
        """
        """
        cif_path = cif_path.replace("/", "\\")
        for i_line in reversed(range(len(self.data))):
            line = self.data[i_line]
            if line[0] == cif_path:
                del self.data[i_line]
        # === end of CifIriData.del_entry()

   # === end of class CifIriData


CIF_IRI_DATA = CifIriData()

BLACK_LIST = []
"""
             ['pdffiles\\10.1107_S0108768192009005-supp.cif',
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

    """
    __slots__ = ["uuidDB", "iri", "uuid",
                 "cifOutput", "cifPyMatGen", "cifValAndErr",
                 "abox_prefix", "cif_path", "cifStandard",
                 "unitcell_iri", ]

    def __init__(self, uuidDB,
                 # tbox_prefix=None,
                 abox_prefix=None):

        self.uuidDB = uuidDB
        self.iri = None
        self.uuid = None

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

        self.unitcell_iri = ""
        self.cifStandard = None
        self.cif_path = ""
        self.unitcell_iri = None

        # === end of CrystalInfo.__init__()

    def get_uuid(self):
        if self.iri is None:
            logging.error(" In crystalinfo.CrystalInfo uuid_cif = None.")
            return "None"
        return self.iri
        # === end of CrystalInfo.get_uuid()

    def in_black_list(self, file_path):
        if file_path in BLACK_LIST:
            return True
        return False
        # === end of CrystalInfo.in_black_list()

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
            logging.error(" CIF file not found: '%s'.", file_path)
            return 1

        path = self.cleanCif(path)[0]

        self.cif_path = path

        name = "UNDEF"

        # 1. load data to (struct in cifPyMatGen and unitCellLengths in cifValAndErr)
        self.loadPyMatGen(path, name)
        self.loadValAndErr(path, name)

        # 2. evaluate data (create all arrays and internal data):
        self.evalPyMatGen()
        self.evalValAndErr()

        # 3. choose which to save from which crystal information:
        self.mergeCrystInfo()

        return err_count
        # === end of CrystalInfo.load_cif_file()

    def load_cif_file_val_and_err(self, file_path, options=None):
        """ Load a CIF file and store the data in internal variables.

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
            logging.error(" CIF file not found: '%s'.", file_path)
            return 1

        #path = self.cleanCif(path)[0]
        self.cif_path = path

        name = "UNDEF"

        # 1. load data as ValueAndError:
        self.loadValAndErr(path, name)

        # 2. evaluate data (create all arrays and internal data):
        self.evalValAndErr()

        # 3. choose which to save from which crystal information:
        self.mergeCrystInfo()

        return err_count
        # === end of CrystalInfo.load_cif_file()

    def get_csv_arr_from_cif(self, file_path,
                             name="CrystalInformation", new_uuid=None,
                             subject="", predicate="hasCrystalInformation",
                             flags="CURVTA"):
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
        flags    - Choice of ontology parts to be saved.
                   "C" - CrystalInformation iri. Basic iri, usually required.
                   "U" - UnitCell (only basic data: a,b,c,alpha,beta,gamma,volume),
                   "R" - Reciprocal properties of the UnitCell (requires "U"),
                   "V" - UnitCell Vectors (requires "U"),
                   "T" - Transformation matrix and vector,
                   "A" - AtomicStructure: list of all atoms.
        """
        output = []

        logging.info("Starting crystalinfo.csv_arr_material(), new_iri '%s'", str(new_uuid))

        if new_uuid is None:
            # Check for existing uuids in the database of cif_files:
            self.iri, self.uuid, _, __ = CIF_IRI_DATA.get_entry_iri(file_path)
            new_uuid = self.uuid

            logging.info(" >>> Start CIF file: '%s', iri = '%s'.", file_path, self.iri)

            if not self.iri:
                self.iri, self.uuid = self.uuidDB.addUUID(
                                           crystOntoPrefix + "CrystalInformation",
                                           self.abox_prefix + "CrystalInformation_" + name)
            else:
                # Do nothing, self.iri is already defined
                pass

        else:
            self.uuid = new_uuid
            self.iri = "CrystalInformation_" + self.uuid

        if subject != "" and predicate != "":
            output.append([self.abox_prefix + self.iri, "Instance",
                           crystOntoPrefix + "CrystalInformation", "", "", ""])
            output.append([subject, "Instance", self.abox_prefix + self.iri,
                           predicate, "", ""])
            return output

        logging.info(" Continue initializing CIF:", file_path, self.iri)

        self.iri = self.abox_prefix + self.iri
        output.append([self.iri, "Instance",
                       crystOntoPrefix + "CrystalInformation", "", "", ""])

        try:
            err_count = self.load_cif_file_py_mat_gen(file_path)
            # logging.warning(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            # logging.warning(">>>> Done CIF '%s' by PyMatGen >>>>", file_path)
            # logging.warning(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            pass
        except Exception as e:
            logging.warning("===============================================")
            logging.warning(" Failed to read data by PyMatGen '%s'," +
                            " start reading by ValAndErr.", file_path)
            # logging.warning(" More details: %s", str(e))
            logging.warning("===============================================")

            err_count = self.load_cif_file_val_and_err(file_path)

            if has_xyz_line(file_path):
                with open("list_of_cif_fails.txt", "a", encoding="utf-8") as fp:
                    fp.write(file + "\n")
                    pass

        if err_count > 0:
            logging.error(" Failed to load cif file '%s', aborting", file_path)

            with open("list_of_cif_fails.txt", "a", encoding="utf-8") as fp:
                fp.write(file + "\n")
                pass
            return output

        #if self.uuid:
        #    self.iri = self.abox_prefix + "CrystalInformation_"
        #    self.iri += self.uuid
        #else:
        #    self.iri, _ = self.uuidDB.addUUID(crystOntoPrefix + "CrystalInformation",
        #                                      self.abox_prefix + "CrystalInformation_" + name)

        if True:
            # Define relation between the class instances:

            # Redundant declaration of CrystalInformation:
            #output.append([self.iri, "Instance",
            #               crystOntoPrefix + "CrystalInformation", "", "", ""])

            if "U" in flags or "R" in flags or "V" in flags:
                tmp = self.arrUnitCell(self.iri,
                                       crystOntoPrefix + "hasUnitCell",
                                       name, new_uuid=self.uuid)
                if tmp is None:
                    logging.warning(" Missing Unit Cell information" +
                                    " in cif file '%s'.", file_path)
                else:
                    output += tmp

                #################################################
                if "V" in flags:
                    output += self.get_csv_arr_unit_cell_vector_set(self.unitcell_iri,
                                   crystOntoPrefix + "hasUnitCellVectorSet",
                                   new_uuid=self.uuid)

                #################################################
                if "R" in flags:
                    output += self.get_csv_arr_reciprocal_unit_cell(self.unitcell_iri,
                                                                    new_uuid=self.uuid)

            if "T" in flags:
                tmp = self.arrTransform(self.iri, crystOntoPrefix +
                                        "hasCoordinateTransformation",
                                        name, new_uuid=self.uuid)
                if tmp is None:
                    logging.warning(" Missing Transformation information!")
                else:
                    output += tmp

            if "A" in flags:
                tmp = self.arrAtomSite(self.iri, crystOntoPrefix +
                                       "hasAtomicStructure", name,
                                       new_uuid=self.uuid)
                if tmp is None:
                    logging.warning(" Missing Atom Site information!")
                else:
                    output += tmp

        return output
        # === end of CrystalInfo.get_csv_arr_from_cif()

    def get_csv_arr_unit_cell_vector_set(self, subject, predicate, new_uuid):
        """
        subject   - Is the full hame of instance of class UnitCell,
                    which contains this UnitCell class.
        predicate - Is the Object Property linking the Subject and the current UnitCellVectorSet.
                    Typically it should be equal to "hasUnitCellVectorSet".
        """

        if subject.find("UnitCell") < 0:
            logging.warning(" Subject in get_csv_arr_unit_cell_vector_set() is '%s', expecting" +
                            " the name to contain 'UnitCell'.",
                            subject)

        if predicate.find("hasUnitCellVectorSet") < 0:
            logging.warning(" Predicate in get_csv_arr_unit_cell_vector_set() is '%s'," +
                            " but expecting 'hasUnitCellVectorSet'.", predicate)

        output = []

        cif_uc_iri = subject

        # Vector to keep three Unit Cell vectors (a,b,c):
        if new_uuid:
            uc_vec_abc_iri = self.abox_prefix + "UnitCellVectorSet" + \
                              "_" + new_uuid
        else:
            uc_vec_abc_iri, _ = self.uuidDB.addUUID(crystOntoPrefix + "UnitCellVectorSet",
                                                    "UnitCellVectorSet_" + new_uuid)

        output.append([uc_vec_abc_iri, "Instance",
                       crystOntoPrefix + "UnitCellVectorSet", "", "", ""])

        output.append([cif_uc_iri, "Instance", uc_vec_abc_iri,
                       predicate, "", ""])

        if self.cifOutput.unitCellVectorA:
            output += self.cifOutput.unitCellVectorA.get_csv_arr(uc_vec_abc_iri,
                                     crystOntoPrefix + "hasUnitCellVector",
                                     new_uuid)
        else:
            logging.warning(" Missing unit cell vector A for %s", cif_uc_iri)

        if self.cifOutput.unitCellVectorB:
            output += self.cifOutput.unitCellVectorB.get_csv_arr(uc_vec_abc_iri,
                                     crystOntoPrefix + "hasUnitCellVector",
                                     new_uuid)
        else:
            logging.warning(" Missing unit cell vector A for %s", cif_uc_iri)

        if self.cifOutput.unitCellVectorC:
            output += self.cifOutput.unitCellVectorC.get_csv_arr(uc_vec_abc_iri,
                                     crystOntoPrefix + "hasUnitCellVector",
                                     new_uuid)
        else:
            logging.warning(" Missing unit cell vector A for %s", cif_uc_iri)

        return output
        # === end of CrystalInfo.get_csv_arr_unit_cell_vector_set(self, subject, predicate):

    def get_csv_arr_reciprocal_unit_cell(self, subject, new_uuid):
        """
        subject   - Is the full hame of instance of class UnitCell,
                    which contains this UnitCell class.
        Note: there is no predicate, because there are many properties
              with different predicates added in this function.
        """

        if subject.find("UnitCell") < 0:
            logging.warning(" Subject in get_csv_arr_reciprocal_unit_cell() is '%s', expecting" +
                            " the name to contain 'UnitCell'.",
                            subject)

        output = []
        cif_uc_iri = subject

        if self.cifOutput.unitCellRecipLengths is not None:
            output += self.cifOutput.unitCellRecipLengths.get_csv_arr(cif_uc_iri,
                      crystOntoPrefix + "hasReciprocalUnitCellLengths",
                      new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell reciprocal lengths for %s",
                            cif_uc_iri)

        if self.cifOutput.unitCellRecipAngles is not None:
            output += self.cifOutput.unitCellRecipAngles.get_csv_arr(cif_uc_iri,
                      crystOntoPrefix + "hasReciprocalUnitCellAngles",
                      new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell reciprocal angles for %s",
                            cif_uc_iri)

        # Vector to keep three Reciprocal Unit Cell vectors (a,b,c):
        if new_uuid:
            uc_r_vec_abc_iri = self.abox_prefix + "ReciprocalUnitCellVectorSet" + \
                                "_" + new_uuid
        else:
            uc_r_vec_abc_iri, _ = self.uuidDB.addUUID(crystOntoPrefix + "UnitCellVectorSet",
                                                      "ReciprocalUnitCellVectorSet_" + new_uuid)

        output.append([uc_r_vec_abc_iri, "Instance",
                       crystOntoPrefix + "UnitCellVectorSet", "", "", ""])

        output.append([cif_uc_iri, "Instance", uc_r_vec_abc_iri,
                       crystOntoPrefix + "hasReciprocalUnitCellVectorSet", "", ""])

        if self.cifOutput.unitCellRecipVectorA:
            output += self.cifOutput.unitCellRecipVectorA.get_csv_arr(
                      uc_r_vec_abc_iri,
                      crystOntoPrefix + "hasUnitCellVector", new_uuid)
        else:
            logging.warning(" Missing unit cell reciprocal vector A for %s", cif_uc_iri)

        if self.cifOutput.unitCellRecipVectorB:
            output += self.cifOutput.unitCellRecipVectorB.get_csv_arr(uc_r_vec_abc_iri,
                      crystOntoPrefix + "hasUnitCellVector", new_uuid)
        else:
            logging.warning(" Missing unit cell reciprocal vector B for %s", cif_uc_iri)

        if self.cifOutput.unitCellRecipVectorC:
            output += self.cifOutput.unitCellRecipVectorC.get_csv_arr(uc_r_vec_abc_iri,
                      crystOntoPrefix + "hasUnitCellVector", new_uuid)
        else:
            logging.warning(" Missing unit cell reciprocal vector C for %s", cif_uc_iri)

        return output
        # === end of CrystalInfo.get_csv_arr_reciprocal_unit_cell()

    def arrUnitCell(self, subject, predicate, zeoname, new_uuid=None):
        """
        subject   - Is the full hame of instance of class CrystalInformation,
                    which contains this UnitCell class.
        predicate - Is the Object Property linking the Subject and the current UnitCell.
                    Typically it should be equal to "hasUnitCell".
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
            cif_uc_iri = self.abox_prefix + "UnitCell_"
            cif_uc_iri += new_uuid

        else:
            cif_uc_iri, _ = self.uuidDB.addUUID(crystOntoPrefix + "UnitCell",
                                                "UnitCell_" + zeoname)

        output.append([cif_uc_iri, "Instance", crystOntoPrefix + "UnitCell", "", "", ""])

        output.append([subject, "Instance", cif_uc_iri, predicate, "", ""])

        if self.cifOutput.unitCellLengths is not None:
            output += self.cifOutput.unitCellLengths.get_csv_arr(cif_uc_iri,
                      crystOntoPrefix + "hasUnitCellLengths", new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell lengths for %s", self.cif_path)

        if self.cifOutput.unitCellAngles is not None:
            output += self.cifOutput.unitCellAngles.get_csv_arr(cif_uc_iri,
                      crystOntoPrefix + "hasUnitCellAngles", new_uuid=new_uuid)
        else:
            logging.warning(" Missing unit cell angles for %s", self.cif_path)

        if self.cifOutput.unitCellVolume is not None:
            output += self.cifOutput.unitCellVolume.get_csv_arr(cif_uc_iri,
                      crystOntoPrefix + "hasUnitCellVolume", new_uuid=new_uuid)
        else:
            logging.warning(" Missing volume for %s", cif_uc_iri)

        if self.cifOutput.symmSpaceGroupHM is not None:
            output.append([crystOntoPrefix + "hasSpaceGroupHM",
                           "Data Property", cif_uc_iri, "",
                           self.cifOutput.symmSpaceGroupHM, "string"])
        else:
            logging.warning(" Missing hasSpaceGroupHM value in '%s'", self.cif_path)

        if self.cifOutput.symmITNumber is not None:

            output.append([crystOntoPrefix + "hasSymmetryNumber",
                           "Data Property", cif_uc_iri, "",
                           self.cifOutput.symmITNumber, "integer"])

            sym_code = crystaldata.SPACE_GROUP_SYMBOL[self.cifOutput.symmITNumber]

            output.append([crystOntoPrefix + "hasSpaceGroupSymbol",
                           "Data Property", cif_uc_iri, "",
                           sym_code, "string"])

        else:
            logging.warning(" Missing hasSymmetryNumber value in '%s'", self.cif_path)

        if self.cifOutput.symmLatticeSystem is not None:
            output.append([crystOntoPrefix + "hasLatticeSystem",
                           "Data Property", cif_uc_iri, "",
                           self.cifOutput.symmLatticeSystem, "string"])
        else:
            logging.warning(" Missing hasLatticeSystem value in '%s'", self.cif_path)

        self.unitcell_iri = cif_uc_iri

        return output
        # === end of CrystalInfo.arrUnitCell()

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
            cif_core_trans_iri = self.abox_prefix + "CoordinateTransformation_" + new_uuid
        else:
            cif_core_trans_iri, _ = self.uuidDB.addUUID(
                                          crystOntoPrefix + "CoordinateTransformation",
                                          "CoordinateCIFCoreTransform_" + zeoname)

        output.append([cif_core_trans_iri, "Instance",
                       crystOntoPrefix + "CoordinateTransformation", "", "", ""])
        output.append([subject, "Instance", cif_core_trans_iri,
                       predicate, "", ""])

        if self.cifOutput.matrixFracToCart:
            output += self.cifOutput.matrixFracToCart.get_csv_arr(cif_core_trans_iri,
                           crystOntoPrefix + "hasTransformationMatrixToCartesian",
                           new_uuid=new_uuid)

        if self.cifOutput.vectorFracToCart:
            output += self.cifOutput.vectorFracToCart.get_csv_arr(cif_core_trans_iri,
                           crystOntoPrefix + "hasTransformationVectorToCartesian",
                           new_uuid=new_uuid)

        if self.cifOutput.matrixCartToFrac:
            output += self.cifOutput.matrixCartToFrac.get_csv_arr(cif_core_trans_iri,
                           crystOntoPrefix + "hasTransformationMatrixToFractional",
                           new_uuid=new_uuid)

        if self.cifOutput.vectorCartToFrac:
            output += self.cifOutput.vectorCartToFrac.get_csv_arr(cif_core_trans_iri,
                           crystOntoPrefix + "hasTransformationVectorToFractional",
                           new_uuid=new_uuid)

        return output
        # === end of CrystalInfo.arrTransform()

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

        output = []

        if new_uuid:
            atomic_iri = self.abox_prefix + "AtomicStructure_"
            atomic_iri += new_uuid
        else:
            atomic_iri, _ = self.uuidDB.addUUID(crystOntoPrefix + "AtomicStructure",
                                                 "AtomicStructure_" + zeoname)

        output.append([atomic_iri, "Instance", crystOntoPrefix + "AtomicStructure", "", "", ""])
        output.append([subject, "Instance", atomic_iri, predicate, "", ""])

        for ia, atom in enumerate(self.cifValAndErr.listAtomAll):
            output += atom.getArrAtom(atomic_iri, crystOntoPrefix + "hasAtomSite",
                                      label = str(ia), new_uuid=new_uuid)
            pass

        return output
        # === end of CrystalInfo.arrAtomSite()

    def loadPyMatGen(self, path, name):
        """
        Load the CIF by the standard PyMatGen
        and save it to CrystalInformation class.
        """

        #if self.cifPyMatGen:
        if True:
            self.cifPyMatGen = crystaldata.CrystalData("PyMatGen", self.uuidDB,
                                                       abox_prefix=self.abox_prefix,
                                                       cif_standard=self.cifStandard)
            self.cifPyMatGen.loadData(path, name)
            self.cifStandard = self.cifPyMatGen.cifStandard

        # === end of CrystalInfo.loadPyMatGen()

    def loadValAndErr(self, path, name):
        """
        Load the CIF data written by this hand-made class (with uncertainty)
        and save it to CrystalInformation class.
        """

        #if self.cifValAndErr:
        if True:
            self.cifValAndErr = crystaldata.CrystalData("ValAndErr", self.uuidDB,
                                                        abox_prefix=self.abox_prefix,
                                                        cif_standard=self.cifStandard)
            self.cifValAndErr.listAtomRaw = []
            self.cifValAndErr.listAtomAll = []
            self.cifValAndErr.loadData(path, name)

            self.cifStandard = self.cifValAndErr.cifStandard
        # === end of CrystalInfo.loadValAndErr()

    def evalPyMatGen(self):
        if self.cifPyMatGen:
            self.cifPyMatGen.evalPyMatGen()

        # === end of CrystalInfo.evalPyMatGen()

    def evalValAndErr(self):
        if self.cifValAndErr:
            self.cifValAndErr.evalValAndErr()

        # === end of CrystalInfo.evalValAndErr()

    def mergeCrystInfo(self):
        """
        Run through both versions: cifPyMatGen and cifValAndErr and fix by rules:
        j) For cifValAndErr use math to compute not available data,
        2) I there is uncertainty - use it,
        3) If only one CIF has data - use it,
        4)

        """

        if self.cifPyMatGen:
            self.cifOutput = self.cifPyMatGen
        else:
            self.cifOutput = crystaldata.CrystalData("ValAndErr", self.uuidDB,
                                                     abox_prefix=self.abox_prefix)
            self.cifOutput = self.cifValAndErr

        attrs = [
                 "symmLatticeSystem", "symmITNumber",
                 "unitCellLengths",  "unitCellRecipLengths",
                 "unitCellAngles",  "unitCellRecipAngles",
                 "unitCellVectorA",      "unitCellVectorB",      "unitCellVectorC",
                 "unitCellRecipVectorA", "unitCellRecipVectorB", "unitCellRecipVectorC",
                 "unitCellVolume"]
        """
                 "listAtomRaw", "listAtomAll", "listAtomSymm",
                 # other properties:
                 "cifStandard", "loopHeaders",
        """

        for attr in attrs:
            self._add_val_and_err_to_output(attr)
            pass

        # === end of CrystalInfo.mergeCrystInfo()

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

        # === end of CrystalInfo._add_val_and_err_to_output()

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
            logging.error(" In cleanCif(): input file '%s' not found.", file_in)
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

        # 2) Remove brackets, i.e. the uncertainty:
        for i in range(len(lineRanges)-1):

            n_bracket = self.readWithUncertainties(file_in, "zeoname",
                                                   lineRanges[i], lineRanges[i+1])
            if (n_bracket > 0) or (len(lineRanges) - 1 > 1):
                file_base, ext = os.path.splitext(os.path.basename(file_in))
                # print("Input file name and extension:", fileBase, ext)

                file_out = os.path.join(tmp_dir, file_base + "_" + str(i+1) + ext)
                files_out.append(file_out)
                self.readWithUncertainties(file_in, "zeoname", lineRanges[i],
                                           lineRanges[i+1], file_out=fileOut)

        return files_out
        # === end of CrystalInfo.cleanCif()

    # === end of class CrystalInfo


def list_files_with_extension(directory, extension):
    if not os.path.isdir(directory):
        print(f"Missing directory '{directory}' in crystalInfo")
    files = []
    for file in os.listdir(directory):
        if file.endswith(extension):
            files.append(file)
    return files


def get_xrd_file(cif_file, xrd_dir):
    dir_name, file_name = os.path.split(cif_file)
    filebase, ext = os.path.splitext(file_name)

    xrd_path = os.path.join(xrd_dir, dir_name, filebase + ".xrd")
    return xrd_path
    # === end of get_xrd_file()


if __name__ == "__main__":

    LOG_FILE = "xrd.log"
    with open(LOG_FILE, "w", encoding="utf-8") as fp:
        # Do nothing. I.e. save an empty LOG_FILE file.
        # Overwriting any previous log file.
        pass

    # Algorithm to process the cif files:
    # 1. Get a directory with cif files and the list of files
    files = []

    args = read_command_line()

    FLAGS = args.flags.upper()

    if args.cif == "":
        print("Missing input cif file. Use command line argument --cif=<file>")
        sys.exit(0)
    elif os.path.isfile(args.cif):
        name, ext = os.path.splitext(args.cif)
        ext = ext.lower()
        if ext == ".cif":
            files = [args.cif]
        elif ext == ".csv" or ext == ".txt":
            with open(args.cif, encoding="utf-8") as fp:
                for line in fp:
                    value = line.strip()

                    if value.lower().endswith(".cif"):
                        files.append(value)
                    """
                    if os.path.isfile(value):
                        name, ext = os.path.splitext(value)
                        if ext.lower() == ".cif":
                            files.append(value)
                    else:
                        print("Not a cif. Skipping it:", value)
                    """

    elif os.path.isdir(args.cif):
        print("Reading cif files from directory:", args.cif)
        all_files = os.listdir(args.cif)
        for f in all_files:
            if value.lower().endswith(".cif"):
                files.append(value)
            """
            name, ext = os.path.splitext(f)
            if ext.lower() == ".cif":
                files.append(f)
            """
    else:
        print("Unkown value for command line argument --cif:", args.cif)
        sys.exit(0)

    files = list(set(files))

    # Check repeating files:
    tmp = list(files)
    tmp.sort()
    for i_line in range(len(tmp)-1):
        line = tmp[i_line]
        if line.lower() == tmp[i_line+1].lower():
            print("Repeating file ", line)

    # 2. Start processing files one by one:
    #    2a. If size exceeds size 20Mb move to the next folder
    #    2b. Keep a list of CIF uuid
    #    2c. Once too many lines in the output - create a new csv file.

    #if not os.path.isdir(DB_FOLDER):
    #    os.mkdir(DB_FOLDER)

    if args.outDir == "":
        CSV_FOLDER = os.path.join("csv")
    else:
        CSV_FOLDER = args.outDir

    if args.outDir == "":
        XRD_FOLDER = os.path.join("xrd")
    else:
        XRD_FOLDER = args.xrdDir

    if not os.path.isdir(CSV_FOLDER):
        os.mkdir(CSV_FOLDER)

    UUID_FOLDER = os.path.join("uuid")
    if not os.path.isdir(UUID_FOLDER):
        os.mkdir(UUID_FOLDER)

    output = []
    output += tools.get_csv_init("base",
                                 t_prefix=crystOntoPrefix + "OntoCrystal.owl",
                                 a_prefix=args.abox_prefix)

    db_count = 0
    CIF_IRI_DATA = CifIriData()
    CIF_IRI_DATA.self_test()

    uuid_file = os.path.join(UUID_FOLDER, "crystal_uuid_" + str(db_count) + ".csv")
    uuidDB = tools.UuidDB(filename=uuid_file)

    for i_file, file in enumerate(files):

        cif_iri = ""
        cif_uuid = ""
        if "C" in FLAGS or "U" in FLAGS or "R" in FLAGS or \
           "V" in FLAGS or "T" in FLAGS or "A" in FLAGS:
            cif_iri, cif_uuid, _, __ = CIF_IRI_DATA.get_entry_iri(file)
            #cif_uuid = tools.new_uuid()
            if cif_iri is None and cif_uuid is None:
                xrd_path = get_xrd_file(file, XRD_FOLDER)
                CIF_IRI_DATA.set_entry(file, cif_iri, cif_uuid, xrd_path, "")
                cif_iri, cif_uuid, _, __ = CIF_IRI_DATA.get_entry_iri(file)

            cryst = CrystalInfo(uuidDB=uuidDB, abox_prefix=args.abox_prefix)

            cif_name = str(file)
            tmp = cryst.get_csv_arr_from_cif(file, cif_name,
                                             subject="", predicate="",
                                             new_uuid=cif_uuid, flags=FLAGS)
            output += tmp

        if "X" in FLAGS:
            xrd_path = get_xrd_file(file, XRD_FOLDER)
            if not os.path.isfile(xrd_path):
                file_in = os.path.join("", file)
                file_out = xrd_path

                try:
                    if not os.path.isfile(file_out):
                        peaks = xrd_simulation.get_powder_xrd(file_in, 5, 70, 5)
                        xrd_simulation.save_xrd(file_out, peaks)
                except Exception as e:
                    print("Failed to read file:", file_in, "in try clause")
                    print("Exception details:", str(e))
                    with open(LOG_FILE, "a", encoding="utf-8") as flog:
                        flog.write("Failed to read " + file_in + "\n")

            xrd = xrds.XRDSpectrum(abox_prefix=args.abox_prefix)
            tmp = xrd.get_csv_arr(args.abox_prefix + cif_iri,
                                  crystOntoPrefix + "hasXRDSpectrum", xrd_path)
            if tmp != []:
                output += tmp
            CIF_IRI_DATA.set_entry(file, cif_iri, cif_uuid, xrd_path, xrd.iri)

        # To limit the file size, because blazegraph cannot accept big files.
        if len(output) > 150000 or i_file == len(files) - 1:
            file_out = os.path.join(CSV_FOLDER, "cif_twa_" + str(db_count) + ".csv")
            tools.writeCsv(file_out, output)
            output = []
            output += tools.get_csv_init("base",
                                         t_prefix=crystOntoPrefix + "OntoCrystal.owl",
                                         a_prefix=args.abox_prefix)

            db_count += 1

            # Update the uuid database:
            uuidDB.saveDB()
            uuid_file = os.path.join(UUID_FOLDER,
                                     "crystal_uuid_" + str(db_count) +".csv")
            if i_file < len(files) - 1:
                uuidDB = tools.UuidDB(filename=uuid_file)

            # Regular save IRI to reduce loss of data in case of crash (debug):
            #CIF_IRI_DATA.save()

            # print("=========  Early exit CrystalInfo.py (debug) ==========")
            # break

    CIF_IRI_DATA.save()

    pass
