import os
import math
import logging

import uuid

import tools

import crystaldata

import ontocrystal_datatypes as ocdt

#logging.basicConfig(level = logging.DEBUG)
#logging.basicConfig(level = logging.INFO)
logging.basicConfig(level = logging.WARNING)
#logging.basicConfig(level = logging.ERROR)

crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

CIF_IRI_FILE = "cif_iri_list.csv"  # Format: filename, count/unique_id, iri, uuid
CIF_IRI_LIST = None
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

def get_cif_uuid(cif_path):
    """

    """
    global CIF_IRI_LIST
    if CIF_IRI_LIST is None:
        CIF_IRI_LIST = tools.readCsv(CIF_IRI_FILE)

    tofind = cif_path.lower().strip()
    for cif in CIF_IRI_LIST:
        if cif[0].lower() == tofind:
            return cif[2].strip()
        #if cif[1].lower() == tofind:  ??
        #    return cif[2].strip()

    uuid_str = str(uuid.uuid4())
    iri = "CIF_data_" + uuid_str
    folder = "folder"
    CIF_IRI_LIST.append([cif_path, folder, iri, uuid_str])
    tools.writeCsv(CIF_IRI_FILE, CIF_IRI_LIST)
    return iri

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

    def __init__(self, uuidDB):

        self.uuidDB = uuidDB
        self.uuid_cif = None

        self.cifOutput = None
        self.cifPyMatGen = None
        self.cifValAndErr = None
        # === end of CrystalInfo.()

    def get_uuid(self):
        if self.uuid_cif is None:
            logging.error(" In crystalinfo.CrystalInfo uuid_cif = None.")
            return "None"
        return self.uuid_cif
        # === end of CrystalInfo.()

    def in_black_list(self, file_path):
        if file_path in BLACK_LIST:
            return True
        return False
        # === end of CrystalInfo.()

    def get_csv_arr_from_cif(self, file_path, name,
                             subject, predicate = "hasCrystalInformation",
                             new_iri=""):
        """
        Here subject is the element of class crystal_uuid
        """
        output = []

        if self.in_black_list(file_path):
            logging.warning(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            logging.warning(" Badly formed cif file: '%s', skip it", file_path)
            logging.warning(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            return output

        if os.path.isfile(file_path):
            path = file_path
        elif os.path.isfile(file_path.replace("--", "-")):
            path = file_path.replace("--", "-")
        else:
            logging.error(" CIF file does not exist: '%s'.", file_path)
            return output

        #print("Starting get_csv_arr_from_cif() for", file_path, name)

        path = self.cleanCif(path)[0]
        # print("path =", path)

        # First load data to (struct in cifPyMatGen and unitCellLengths in cifValAndErr)
        self.loadPyMatGen(path, name)
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

        # Third choose which to save from which crystal information:
        #print(" ==> ", self.cifValAndErr.unitCellLengths)
        self.mergeCrystInfo()

        #self.cifOutput = self.cifValAndErr
        #print(" -->  ", self.cifOutput.unitCellLengths)

        #self.loadCifZeolite(z)
        #self.evalCifData()

        # Output:
        if True:
            #This does not work: there is no zeolite<->cif correspondance.

            #self.uuid_cif = tools.getUUID(self.uuidDB.uuidDB, "CrystalInformation",
            #                         "ZeoliteCIF_" + name)

            print("Starting crystalinfo.csv_arr_material(), new_iri", new_iri)
            if new_iri == "" or new_iri is None:
                self.uuid_cif = get_cif_uuid(file_path)
                if not self.uuid_cif:
                    self.uuid_cif = tools.getUUID(self.uuidDB.uuidDB,
                                                  "CrystalInformation",
                                                  "ZeoliteCIF_" + name)
            else:
                self.uuid_cif = new_iri

            print("         crystalinfo.csv_arr_material(), cif_iri", self.uuid_cif)

            output.append([self.uuid_cif, "Instance", "CrystalInformation", "", "", ""])

            # Define relation between the class instances:
            #output.append([subject, "Instance", uuid_cif,

            predicate = crystOntoPrefix + "hasCrystalInformation"

            #print("sublect =", subject, type(subject))
            if subject != "" and predicate != "":
                output.append([subject, "Instance", self.uuid_cif, predicate, "", ""])
                return output
            print("Continue initializing CIF", file_path, self.uuid_cif)
            #output.append([subject, "Instance", self.uuid_cif, predicate, "", ""])

            #self.loadUnitCellPyMatGen(zeoname)

            #arr += self.arrInitZeolite(uuid_zeolite)

            #print("self.iri =", self.uuid_cif, type(self.uuid_cif))
            tmp = self.arrUnitCell(self.uuid_cif,
                                   crystOntoPrefix + "hasUnitCell", name)
            if tmp is None:
                logging.warning(" Missing Unit Cell information!")
            else:
                output += tmp

            tmp = self.arrTransform(self.uuid_cif, crystOntoPrefix +
                                    "hasCoordinateTransformation", name)
            if tmp is None:
                logging.warning(" Missing Transformation information!")
            else:
                output += tmp

            tmp = self.arrAtomSite(self.uuid_cif, crystOntoPrefix +
                                   "hasAtomicStructure", name)
            if tmp is None:
                logging.warning(" Missing Atom Site information!")
            else:
                output += tmp

            """
            print("starting arrTile for", name)
            tmp = self.arrTiles(self.uuid_cif, crystOntoPrefix + "hasTiledStructure", name)
            if tmp is None:
                logging.warning(" Missing Tiled Structure information!")
            else:
                output += tmp

            tmp = self.arrSpectrum(self.uuid_cif, crystOntoPrefix + "hasXRDSpectrum", name)
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

    def arrUnitCell(self, subject, predicate, zeoname):
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

        uuid_cif_uc = tools.getUUID(self.uuidDB.uuidDB, "UnitCell", "UnitCell_" + zeoname)
        output.append([uuid_cif_uc, "Instance", "UnitCell", "", "", ""])

        output.append([subject, "Instance", uuid_cif_uc, predicate, "", ""])

        if self.cifOutput.unitCellLengths is not None:
            output += self.cifOutput.unitCellLengths.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasUnitCellLengths")
        else:
            logging.warning(" Missing unit cell lengths for %s", uuid_cif_uc)
            pass

        if self.cifOutput.unitCellAngles is not None:
            output += self.cifOutput.unitCellAngles.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasUnitCellAngles")

        if self.cifOutput.unitCellRecipLengths is not None:
            output += self.cifOutput.unitCellRecipLengths.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasReciprocalUnitCellLengths")

        if self.cifOutput.unitCellRecipAngles is not None:
            output += self.cifOutput.unitCellRecipAngles.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasReciprocalUnitCellAngles")

        #output += self.cifOutput.unitCellVolume.get_csv_arr(uuid_cif_uc,
        #          self.crystOntoPrefix + "hasUnitCellVolume")

        if self.cifOutput.unitCellVolume is not None:
            output += self.cifOutput.unitCellVolume.get_csv_arr(uuid_cif_uc,
                      crystOntoPrefix + "hasUnitCellVolume")
        else:
            logging.warning(" Missing volume for %s", uuid_cif_uc)
            pass

        #################################################
        # Vector to keep three Unit Cell vectors (a,b,c):
        uuid_uc_vec_abc = tools.getUUID(self.uuidDB.uuidDB, "UnitCellVectorSet",
                                        "UnitCellVectorSet_" + zeoname)
        output.append([uuid_uc_vec_abc, "Instance", "UnitCellVectorSet", "", "", ""])

        output.append([uuid_cif_uc, "Instance", uuid_uc_vec_abc,
                       crystOntoPrefix + "hasUnitCellVectorSet", "", ""])

        if self.cifOutput.unitCellVectorA:
            output += self.cifOutput.unitCellVectorA.get_csv_arr(uuid_uc_vec_abc,
                                     crystOntoPrefix + "hasUnitCellVector")
        else:
            print("No unitCellVectorA 12345")
        if self.cifOutput.unitCellVectorB:
            output += self.cifOutput.unitCellVectorB.get_csv_arr(uuid_uc_vec_abc,
                                     crystOntoPrefix + "hasUnitCellVector")
        if self.cifOutput.unitCellVectorC:
            output += self.cifOutput.unitCellVectorC.get_csv_arr(uuid_uc_vec_abc,
                                     crystOntoPrefix + "hasUnitCellVector")

        # Vector to keep three Reciprocal Unit Cell vectors (a,b,c):
        uuid_uc_r_vec_abc = tools.getUUID(self.uuidDB.uuidDB, "UnitCellVectorSet",
                                          "ReciprocalUnitCellVectorSet_" + zeoname)
        output.append([uuid_uc_r_vec_abc, "Instance", "UnitCellVectorSet", "", "", ""])

        output.append([uuid_cif_uc, "Instance", uuid_uc_r_vec_abc,
                       crystOntoPrefix + "hasReciprocalUnitCellVectorSet", "", ""])
        if self.cifOutput.unitCellRecipVectorA:
            output += self.cifOutput.unitCellRecipVectorA.get_csv_arr(uuid_uc_r_vec_abc,
                      crystOntoPrefix + "hasUnitCellVector")
        else:
            print("No unitCellRecipVectorA 12345")
        if self.cifOutput.unitCellRecipVectorB:
            output += self.cifOutput.unitCellRecipVectorB.get_csv_arr(uuid_uc_r_vec_abc,
                      crystOntoPrefix + "hasUnitCellVector")
        if self.cifOutput.unitCellRecipVectorC:
            output += self.cifOutput.unitCellRecipVectorC.get_csv_arr(uuid_uc_r_vec_abc,
                      crystOntoPrefix + "hasUnitCellVector")

        if self.cifOutput.symmLatticeSystem is not None:
            #output += self.cifOutput.symmLatticeSystem.getArr(uuid_uc_r_vec_abc,
            #          self.crystOntoPrefix + "hasLatticeSystem")
            output.append([crystOntoPrefix + "hasLatticeSystem",
                           "Data Property", uuid_cif_uc, "",
                           self.cifOutput.symmLatticeSystem, "string"])
        else:
            logging.warning(" Missing hasLatticeSystem value")

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

    def arrTransform(self, subject, predicate, zeoname):
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

        uuid_cif_core_trans = tools.getUUID(self.uuidDB.uuidDB, "CoordinateTransformation",
                                             "CoordinateCIFCoreTransform_" + zeoname)
        output.append([uuid_cif_core_trans, "Instance", "CoordinateTransformation", "", "", ""])
        output.append([subject, "Instance", uuid_cif_core_trans, predicate, "", ""])

        ################# Fractional to Cartesian ########################
        uuid_m_frac_to_cart = tools.getUUID(self.uuidDB.uuidDB, "TransformationMatrix",
                              zeoname + "_TransfMatrixToCart")
        output.append([uuid_m_frac_to_cart, "Instance",
                       "TransformationMatrix", "", "", ""])
                               #"CIFCoreTransformationMatrixToCartesian", "", "", ""])
        output.append([uuid_cif_core_trans, "Instance", uuid_m_frac_to_cart,
                       crystOntoPrefix + "hasTransformationMatrixToCartesian", "", ""])

        output.append([uuid_m_frac_to_cart, "Instance",
                         "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                         "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                          "", ""])

        for iy in range(3):
            for ix in range(3):
                uuid_m_comp = tools.getUUID(self.uuidDB.uuidDB, "MatrixComponent",
                            "MatrixComponentToCartesian"+str(ix)+str(iy)+"_" + zeoname)
                output.append([uuid_m_comp, "Instance",
                               "MatrixComponent", "", "", ""])
                               #"CIFCoreTransformationMatrixToCartesian", "", "", ""])
                output.append([uuid_m_frac_to_cart, "Instance", uuid_m_comp,
                               crystOntoPrefix + "hasMatrixComponent", "", ""])

                output.append([crystOntoPrefix + "hasComponentLabel", "Data Property",
                                 uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string"])

                output.append([crystOntoPrefix + "hasRowIndex", "Data Property",
                                 uuid_m_comp, "", iy, "xsd:integer"])

                output.append([crystOntoPrefix + "hasColumnIndex", "Data Property",
                                 uuid_m_comp, "", ix, "xsd:integer"])

                output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                                 uuid_m_comp, "",
                                 round(structure.lattice.matrix[ix][iy], 12), "decimal"])

        uuid_v_frac_to_cart = tools.getUUID(self.uuidDB.uuidDB, "Vector",
                              zeoname + "_TransfVectorToCart")
        output.append([uuid_v_frac_to_cart, "Instance",
                       "PositionVector", "", "", ""])
        output.append([uuid_cif_core_trans, "Instance", uuid_v_frac_to_cart,
                       crystOntoPrefix + "hasTransformationVectorToCartesian", "", ""])

        output.append([uuid_v_frac_to_cart, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                       "", ""])

        for ix in range(3):
            uuid_v_comp = tools.getUUID(self.uuidDB.uuidDB, "VectorComponent",
                          "VectorComponentToCartesian" + str(ix) + "_" + zeoname)
            output.append([uuid_v_comp, "Instance",
                           "VectorComponent", "", "", ""])
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
        uuid_m_cart_to_frac = tools.getUUID(self.uuidDB.uuidDB, "TransformationMatrix",
                              zeoname + "TransfMatrixToFrac")
        output.append([uuid_m_cart_to_frac, "Instance",
                               "TransformationMatrix", "", "", ""])
                               #"CIFCoreTransformationMatrixToCartesian", "", "", ""])
        output.append([uuid_cif_core_trans, "Instance", uuid_m_cart_to_frac,
                       crystOntoPrefix + "hasTransformationMatrixToFractional", "", ""])

        output.append([uuid_m_cart_to_frac, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                       "", ""])

        for iy in range(3):
            for ix in range(3):
                uuid_m_comp = tools.getUUID(self.uuidDB.uuidDB, "MatrixComponent",
                            "MatrixComponentToFractional"+str(ix)+str(iy)+"_" + zeoname)
                output.append([uuid_m_comp, "Instance",
                               "MatrixComponent", "", "", ""])
                               #"CIFCoreTransformationMatrixToCartesian", "", "", ""])
                output.append([uuid_m_cart_to_frac, "Instance", uuid_m_comp,
                               crystOntoPrefix + "hasMatrixComponent", "", ""])

                output.append([crystOntoPrefix + "hasComponentLabel", "Data Property",
                               uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string"])

                output.append([crystOntoPrefix + "hasRowIndex", "Data Property",
                               uuid_m_comp, "", iy, "xsd:integer"])

                output.append([crystOntoPrefix + "hasColumnIndex", "Data Property",
                               uuid_m_comp, "", ix, "xsd:integer"])

                output.append([crystOntoPrefix + "hasComponentValue", "Data Property",
                               uuid_m_comp, "",
                               round(structure.lattice.reciprocal_lattice.matrix[iy][ix]/TWOPI, 12), "decimal"])

        uuid_v_cart_to_frac = tools.getUUID(self.uuidDB.uuidDB, "PositionVector",
                              zeoname + "_TransfVectorToFrac")
        output.append([uuid_v_cart_to_frac, "Instance",
                               "PositionVector", "", "", ""])
                               #"CIFCoreTransformationVectorToFractional", "", "", ""])

        output.append([uuid_cif_core_trans, "Instance", uuid_v_cart_to_frac,
                       crystOntoPrefix + "hasTransformationVectorToFractional", "", ""])

        output.append([uuid_v_cart_to_frac, "Instance",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                       "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                       "", ""])

        for ix in range(3):
            uuid_v_comp = tools.getUUID(self.uuidDB.uuidDB, "VectorComponent",
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
        pass # CsvMaker.arrTransform()

    def arrAtomSite(self, subject, predicate, zeoname):
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

        uuid_atomic = tools.getUUID(self.uuidDB.uuidDB, "AtomicStructure",
                                    "AtomicStructure_" + zeoname)

        output.append([uuid_atomic, "Instance", "AtomicStructure", "", "", ""])
        output.append([subject, "Instance", uuid_atomic, predicate, "", ""])

        for ia, atom in enumerate(self.cifOutput.listAtomAll):
            output += atom.getArrAtom(uuid_atomic, crystOntoPrefix + "hasAtomSite",
                                      label = str(ia))
            pass

        return output
        # === end of CsvMaker.arrAtomSite()


    def loadPyMatGen (self, path, name):
        """
        Load the CIF by the standard PyMatGen
        and save it to CrystalInformation class.
        """

        #logging.error(" Not implemented def CsvMaker.loadPyMatGen (self, path): ")

        #if self.cifPyMatGen:
        if True:
            self.cifPyMatGen = crystaldata.CrystalData("PyMatGen", self.uuidDB)
            self.cifPyMatGen.loadData(path, name)

        # === end of CsvMaker.loadPyMatGen ()

    def loadValAndErr(self, path, name):
        """
        Load the CIF data written by this hand-made class (with uncertainty)
        and save it to CrystalInformation class.
        """

        #logging.error(" Not implemented def loadValAndErr(self, path): ")

        #if self.cifValAndErr:
        if True:
            self.cifValAndErr = crystaldata.CrystalData("ValAndErr", self.uuidDB)
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
            self.cifOutput = self.cifPyMatGen
        else:
            self.cifOutput = crystaldata.CrystalData("ValAndErr", self.uuidDB)

        attrs = [
                #"unitCellLengths", "unitCellRecipLengths",

                "symmLatticeSystem", "symmITNumber", #"", "",
]
        """
                "unitCellAngles",  "unitCellRecipAngles",
                "unitCellVectorA",      "unitCellVectorB",      "unitCellVectorC",
                "unitCellRecipVectorA", "unitCellRecipVectorB", "unitCellRecipVectorC",
                "listAtomRaw", "listAtomAll", "listAtomSymm",
                "unitCellVolume",
                # other properties:
                "cifStandard", "loopHeaders",
        """

        for attr in attrs:
            self._add_val_and_err_to_output(attr)

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

        if hasattr(self.cifPyMatGen, attr):
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
            files.append(line[5])

    print("Found files:", len(files))

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
                                 tPrefix=crystOntoPrefix + "OntoCrystal.owl",
                                 aPrefix=crystOntoPrefix)

    #print("ssssssssss")
    CIF_IRI_LIST = []  # TODO possibility to load from file for extension.
    db_count = 0

    #print("ssssssssss")
    uuid_file = os.path.join(UUID_FOLDER, "crystal_uuid_" + str(db_count) +".csv")
    uuidDB = tools.UuidDB(filename=uuid_file)
    #print("ssssssssss")
    for i_file, file in enumerate(files):
        print("Starting cif", i_file, "of", len(files))

        i_cif = len(CIF_IRI_LIST)

        uuid_str = str(uuid.uuid4())
        cryst = CrystalInfo(uuidDB=uuidDB)
        iri = "CrystalInformation_" + uuid_str
        CIF_IRI_LIST.append([file, i_file, iri, uuid_str, db_count])

        try:
            cif_name = "CIF_" + str(i_file)
            tmp = cryst.get_csv_arr_from_cif(file, cif_name,
                          subject="", predicate="", new_iri=iri)
            output += tmp
        except :    
            logging.error("=================================================")
            logging.error(" Failed to read data from '%s' cif file", file)
            logging.error("=================================================")

            with open("list_of_cif_fails.txt", "a", encoding="utf-8") as fp:
                fp.write(file + "\n")


        if len(output) > 150000 or i_file == len(files) - 1:
            file_out = os.path.join(CSV_FOLDER, "cif_twa_" + str(db_count) + ".csv")
            tools.writeCsv(file_out, output)
            output = []
            output += tools.get_csv_init("base",
                                         tPrefix=crystOntoPrefix + "OntoCrystal.owl",
                                         aPrefix=crystOntoPrefix)

            db_count += 1

            # Update the uuid database:
            uuidDB.saveDB()
            uuid_file = os.path.join(UUID_FOLDER, "crystal_uuid_" + str(db_count) +".csv")
            if i_file < len(files) - 1:
                uuidDB = tools.UuidDB(filename=uuid_file)

            # Regularly saving the iri:
            tools.writeCsv(CIF_IRI_FILE, CIF_IRI_LIST)

        #cryst.get_uuid()


    pass
CIF_IRI_FILE = "cif_iri_list.csv"  # Format: filename, folder, iri, uuid
CIF_IRI_LIST = None
