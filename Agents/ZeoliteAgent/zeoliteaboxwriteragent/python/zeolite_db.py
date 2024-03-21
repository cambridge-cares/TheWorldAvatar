"""
The database of all zeolites.

Combined from several sources.

"""

import os
import logging
import json
import uuid

import zeolist
import tools

import genform
import izatopology

#from bib2csv import *
import bib2csv
import osda_vs_iza

# logging.basicConfig(level=logging.WARNING)
logging.basicConfig(level=logging.INFO)

zeoOntoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"
crystOntoPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"
ontoSpeciesPrefix = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"
biboOntoPrefix = "http://purl.org/ontology/bibo/"

_Mendeleev_table = None
if _Mendeleev_table is None:
    # Remember to skip the top line (the header):
    _Mendeleev_table = tools.readCsv("elements.csv")[1:]

def get_element_iri(element):
    for mend in _Mendeleev_table:
        #print(f"    Compare element '{element}' to '{mend[0]}'")
        if element == mend[0]:
            iri = mend[1].replace("<", "").replace(">", "")
            #print("return", iri)
            return iri
    print(f"Error in get_element_iri(), unknown '{element}'.")
    return None

_chemical_small = None
if _chemical_small is None:
    # Remember to skip the top line (the header):
    _chemical_small = tools.readCsv("Compounds_Laura.csv")[1:]
    #_chemical_small_2 = tools.readCsv("Compounds_to_IRI_2.csv")[1:]
    #_chemical_small += _chemical_small_2
    #_chemical_small_3 = tools.readCsv("Compounds_to_IRI_3.csv")[1:]
    #_chemical_small += _chemical_small_3
    _chemical_small_2 = tools.readCsv("Compounds_to_IRI_23.csv")[1:]
    _chemical_small += _chemical_small_2

    filename = "species_ontozeolite_merged_spaces_removed.csv"
    _chemical_small = tools.readCsv(filename)[1:]


    _chemical_small.sort(key=lambda v: -len(v[0]))
    for i, value in enumerate(_chemical_small):
        _chemical_small[i][0] = value[0].strip()
# for chem in _chemical_small:
#    print(chem[0])

_chemical_small_2 = None
if _chemical_small_2 is None:
    # Remember to skip the top line (the header):
    _chemical_small_2 = tools.readCsv("Compounds_to_IRI_2.csv")[1:]
    _chemical_small_3 = tools.readCsv("Compounds_to_IRI_3.csv")[1:]

    # _chemical_small_2.sort(key=lambda v: -len(v[0]))
    # for i, value in enumerate(_chemical_small_2):
    #    _chemical_small_2[i][0] = value[0].strip()

_ICSD_DOI_LIST = None
if _ICSD_DOI_LIST is None:
    file_path = os.path.join("ontozeolite", "icsddata", "jpcrd392010033102_codes_dois3.csv")
    _raw = tools.readCsv(file_path)
    # Remember to skip the top line (the header):
    #_raw = _raw[1:]

    _ICSD_DOI_LIST = {}
    for line in _raw:
        if len(line) > 1 and len(line[0]) > 4:
            if line[0] in _ICSD_DOI_LIST:
                print("Duplicate symbol:", line[0], "with DOI:",
                      _ICSD_DOI_LIST[line[0]], "vs", line[1])
            _ICSD_DOI_LIST[line[0]] = line[1]
    print("Loaded ICSD DOIs:", len(_ICSD_DOI_LIST))

_ICSD_FORMULA_LIST = None
if _ICSD_FORMULA_LIST is None:
    file_path = os.path.join("ontozeolite", "icsddata", "jpcrd392010033102_codes_dois3.csv")
    _raw = tools.readCsv(file_path)
    # Remember to skip the top line (the header):
    _raw = _raw[1:]

BASE_LIST = ["(AlSi5O12)", "Si96O192"
    ]

_COD_DOI_CIF = None
if _COD_DOI_CIF is None:
    #file_path = os.path.join("ontozeolite", "icsddata", ".json")
    file_path = "cod_doi_cif.json"
    if os.path.isfile(file_path):
        with open(file_path, encoding="utf-8") as fp:
            _COD_DOI_CIF = json.load(fp)
    else:
        print("Missing file '", file_path, "'.", sep="")

_OSDA_COMPOUNDS_IRI = None
if _OSDA_COMPOUNDS_IRI is None:
    file_path = "chemical_data_Laura.csv"
    if os.path.isfile(file_path):
        with open(file_path, encoding="utf-8") as fp:
            _OSDA_COMPOUNDS_IRI = tools.readCsv(file_path)
        _OSDA_COMPOUNDS_IRI = _OSDA_COMPOUNDS_IRI[1:]
    else:
        print("Missing file '", file_path, "'.", sep="")

class ZeoliteFrame:
    __slots__ = ["data", "keys"]

    def __init__(self):
        self.keys = []
        self.data = {}
        # === end of ZeoliteFrame.()

        # === end of ZeoliteFrame.()

        # === end of ZeoliteFrame.()

        # === end of ZeoliteFrame.()

    # === end of class ZeoliteFrame


class ZeoliteMaterial:
    __slots__ = ["data", "keys", "uuidDB", "material_iri",
                 "source", "frame", "name", "guest", "cif_path", "doi",
                 "bib_path", "bib_code", "icsd", "cod_id", "recipe",
                 "base_formula"]

    def __init__(self, uuidDB=None):
        self.keys = ["source", "frame", "name", "safe_name",
                     "guest", "guest-list", "cif_path", "doi",
                     "bib_path", "icsd", "cod_id", "recipe",
                     "formula", "full_formula",
                     "is_ref", "is_interrupt"]

        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            self.uuidDB = tools.UuidDB(uuidDB)
            logging.warning(" Creating a new uuidDB database in file '%s'.",
                            self.uuidDB.dbFilename)
        self.source = []   # List of databases containing this zeolite
        self.frame = None  # The Framework code to which this zeolite belongs
        self.name = []
        self.guest = None
        self.cif_path = None
        self.doi = None
        self.bib_path = None
        self.icsd = None   # The id of the CIF in the CCDC database
        self.cod_id = None  # The id of the CIF in the COD database
        self.recipe = None  # Flag or other info related to the recipe
        self.base_formula = None

        self.data = {}
        self.data["source"] = []
        self.data["name"] = []
        # === end of ZeoliteMaterial.()

    def check(self):
        err_count = 0
        for k in self.keys:
            if k not in self.data:
                #logging.error(" Missing key in ZeoliteMaterial: '%s'", k)
                err_count += 1
                pass

        for k in self.data:
            if k not in self.keys:
                logging.error(" Invalid key in ZeoliteMaterial: '%s'", k)
                err_count += 1

        return err_count
        # === end of ZeoliteMaterial.check()

    def get_csv_arr_material(self, subject, predicate):
        # print(">>>>> Starting get_csv_arr_material for", subject)
        output = []

        if "uuid" in self.data:
            uuid_zeo = "ZeoliticMaterial_" + self.data["uuid"]
        else:
            #print("Not specified uuid of a material")
            #print(self.data)
            #if "safe_name" not in self.data:
            #    logging.error(" In zeolite_db 'safe_name' is not defined")
            #    return output
            #uuid_zeo, _ = self.uuidDB.addUUID("ZeoliticMaterial",
            #                                  "Zeolite_" + self.data["safe_name"])
            self.data["uuid"] = str(uuid.uuid4())
            uuid_zeo = "ZeoliticMaterial_" + self.data["uuid"]

        self.material_iri = uuid_zeo

        output.append([uuid_zeo, "Instance", zeoOntoPrefix + "ZeoliticMaterial", "", "", ""])
        output.append([subject, "Instance", uuid_zeo, predicate, "", ""])
                       #zeoOntoPrefix + "hasZeoliticMaterial", "", ""])

        #print("Starting 'name' section")
        #value = self.data["name"].replace("['", "").replace("']", "")
        if "name" in self.data:
            if isinstance(self.data["name"], list):
                for value in self.data["name"]:
                    #print( value, "in", self.data["name"])
                    output.append([ontoSpeciesPrefix + "name",
                                   "Data Property", uuid_zeo, "",
                                   value, "string"])

            elif isinstance(self.data["name"], str):
                output.append([ontoSpeciesPrefix + "name",
                               "Data Property", uuid_zeo, "",
                               self.data["name"], "string"])

            else:
                logging.error(" data['name'] must be a str or list, got %s type %s",
                              str(self.data["name"]), str(type(self.data["name"])))

        output.append([zeoOntoPrefix + "isHypothetic",
                       "Data Property", uuid_zeo, "", False, "boolean"])

        #print("Starting 'formula' section")
        output += self._get_csv_arr_formula(uuid_zeo, "")

        # Implementation of output.append([hasGuestMol]):
        #print("Going to check guest-list", list(self.data.keys()) )
        if "guest-list" in self.data:
            guest_list = self.data["guest-list"]
            #print("found guest-list", guest_list)
            #print("Adding guest:", self.data["guest-list"])

            # if guest:
            for guest in guest_list:
                #print("Adding guest:", guest_list)
                if isinstance(guest, str):

                    guest_info = self._get_compound_iri(guest)
                    #print("guest_iri =", guest_info)
                    if guest_info is None:
                        # FIXME  Need to skip the guest if it is not defined in the database
                        #continue
                        guest_info = {"name_uuid": "os_IRI_for_" + guest}
                        #print("new guest_iri =", guest_info)

                    if "name_uuid" in guest_info:
                        #output.append([guest_info["name_uuid"], "Instance",
                        #               ontoSpeciesPrefix + "Species", "", "", ""]) # This is external..

                        #if "name" in guest:
                        #    output.append([ontoSpeciesPrefix + "name",
                        #                   "Data Property", guest["name_uuid"], "",
                        #                   guest["name"], "string"])

                        #if "formula" in guest:
                        #    output.append([ontoSpeciesPrefix + "formula",
                        #                   "Data Property", guest["name_uuid"], "",
                        #                   guest["formula"], "string"])

                        output.append([uuid_zeo, "Instance", guest_info["name_uuid"],
                                       zeoOntoPrefix + "hasGuestCompound", "", ""])

                        # Repetition of the guest compound:

                        uuid_tmp = str(uuid.uuid4())
                        comp_ind = crystOntoPrefix + "CompoundIndex_" + uuid_tmp

                        output.append([comp_ind, "Instance", "GuestCompoundIndex", "", "", ""])

                        output.append([uuid_zeo, "Instance", comp_ind,
                                       crystOntoPrefix + "hasGuestCompoundIndex", "", ""])

                        index_value = 12  # FIXME
                        output.append([crystOntoPrefix + "hasIndexValue", "Data Property",
                                       comp_ind, "", index_value, "xsd:integer"])

                        output.append([comp_ind, "Instance", guest_info["name_uuid"],
                                       crystOntoPrefix + "isCompoundIndexOf", "", ""])

                elif isinstance(guest, dict):
                    #print("Adding a single guest:", guest)

                    if "species_iri" in guest:
                        guest_iri = guest["species_iri"]
                        output.append([uuid_zeo, "Instance", guest_iri,
                                       zeoOntoPrefix + "hasGuestCompound", "", ""])

                        # Add cound only if the compound exists.
                        if "count" in guest:
                            uuid_tmp = str(uuid.uuid4())
                            comp_ind = crystOntoPrefix + "CompoundIndex_" + uuid_tmp

                            output.append([comp_ind, "Instance", "GuestCompoundIndex", "", "", ""])

                            output.append([uuid_zeo, "Instance", comp_ind,
                                           crystOntoPrefix + "hasGuestCompoundIndex", "", ""])

                            index_value = guest["count"]
                            output.append([crystOntoPrefix + "hasIndexValue", "Data Property",
                                           comp_ind, "", index_value, "xsd:integer"])

                            output.append([comp_ind, "Instance", guest_iri,
                                           crystOntoPrefix + "isCompoundIndexOf", "", ""])

                        else:
                            logging.error("Missing guest count (a.k.a. index) for material")

                    else:
                        logging.error("Missing guest iri for material")
        else:
            logging.error(" No guest for material '%s'", uuid_zeo)
            with open("guest-tmp.txt", "a", encoding="utf-8") as fp:
                fp.write(f"Frame '?', material '{uuid_zeo}', no guest")
                fp.write("\n")

        #print("Going to add ontobibo")
        if "bib_path" in self.data:
            #tmp = os.path.join("bibfiles", self.data["bib_path"])
            tmp = self.data["bib_path"]

            bib_iri = bib2csv.get_bib_iri(tmp)
            if bib_iri:
                output += [[uuid_zeo, "Instance", bib_iri,
                           crystOntoPrefix + "hasCitation", "", ""]]
            else:
                bibUuidDB = tools.UuidDB(filename=os.path.join("ontozeolite", "final", "uuid", "biblio.csv"))

                #bib = bib2csv.OntoBibo(tmp, uuidDB = self.uuidDB,
                bib = bib2csv.OntoBibo(tmp, uuidDB = bibUuidDB,
                                       tbox_prefix = zeoOntoPrefix,
                                       abox_prefix = zeoOntoPrefix)
                bib.readBib(tmp)
                csv_data, _ = bib.getCsvArr(uuid_zeo, crystOntoPrefix + "hasCitation")
                output += csv_data
            #print("    added ontobibo")

            #bib_uuid, _ = bibUuidDB.getUUID(biboOntoPrefix + "AcademicArticle",
            #                                crystOntoPrefix + "Citation_" + safe_name)
            #output += [[uuid_zeo, crystOntoPrefix + "hasCitation", bib_uuid, "", "", ""]]

        # Implementation of output.append([hasFormula])

        # Implementation of output.append([hasElements])

        #if "is_ref" in self.data:
        #    value = self.data["is_ref"]
        #else:
        #    value = False
        value = self.is_reference_material()
        output.append([zeoOntoPrefix + "isReferenceZeolite",
                       "Data Property", uuid_zeo, "", value, "boolean"])

        if value:
            # FIXME
            #output += self._get_csv_arr_refmat(uuid_zeo)
#
            # IZA topology exists only for the reference material:
            # and is stored in the upper level: in class ZeoliteDB.
            #output += self.izatopology.get_csv_arr_topology(uuid_zeo,
            #          crystOntoPrefix + "hasTopologicalProperties", "ref_mat")

            pass

        #output.append(
        """
        output.append([self.zeoOntoPrefix + "isMineral",
                       "Data Property",
                       uuid_zeo, "", False, "boolean"])

        output.append([self.zeoOntoPrefix + "isSynthetic",
                       "Data Property",
                       uuid_zeo, "", False, "boolean"])
        """

        return output
        # === end of ZeoliteMaterial.get_csv_arr_material()

    def _get_csv_arr_formula(self, uuid_zeo, predicate):
        output = []
        if "formula" in self.data:
            if self.data["formula"] != "":

                output.append([zeoOntoPrefix + "hasChemicalFormula",
                               "Data Property", uuid_zeo, "",
                               self.data["formula"], "string"])

                form = genform.GenFormula()
                form.fullFormula = self.data["formula"]
                form.getFormula()
                self.base_formula = form.formula

        if "formatted_formula" in self.data:
            output.append([zeoOntoPrefix + "hasFrameworkFormula",
                           "Data Property", uuid_zeo, "",
                           self.data["formatted_formula"], "string"])
        else:
            output.append([zeoOntoPrefix + "hasFrameworkFormula",
                           "Data Property", uuid_zeo, "",
                           self.base_formula, "string"])

        if "guest" in self.data and self.data["guest"] != "":
            output.append([zeoOntoPrefix + "hasGuestFormula",
                           "Data Property", uuid_zeo, "",
                           self.data["guest"], "string"])
        else:
            form.getGuest()
            guest_formula = form.guest

            if guest_formula != "":
                output.append([zeoOntoPrefix + "hasGuestFormula",
                               "Data Property", uuid_zeo, "",
                               guest_formula, "string"])

        if self.base_formula:
                elements = genform.parse_formula(self.base_formula)
                #print(self.base_formula, ">", elements)

                #print(elements)
                for el, index in elements:
                    elem = el[0]
                    el_iri = get_element_iri(el)

                    #uid = str(uuid.uuid4())
                    #uuid_atom, _ = self.uuidDB.addUUID("http://purl.org/gc/Atom",
                    #                                   "gcAtom_" + elem,
                    #                                   newUuid=uid)
                    #output.append([uuid_atom, "Instance",
                    #               "http://purl.org/gc/Atom", "", "", ""])

                    #output.append([uuid_zeo, "Instance", uuid_atom,
                    #               ontoSpeciesPrefix + "hasAtom", "", ""])

                    #output.append([uuid_atom, "Instance", el_iri,
                    #               ontoSpeciesPrefix + "isElement", "", ""])

                    output.append([uuid_zeo, "Instance", el_iri,
                                   crystOntoPrefix + "hasFrameworkComponent", "", ""])
                                   #ontoSpeciesPrefix + "hasFrameworkComponent", "", ""])

                    uuid_tmp = str(uuid.uuid4())
                    el_ind = crystOntoPrefix + "ElementIndex_" + uuid_tmp

                    output.append([el_ind, "Instance", "ElementIndex", "", "", ""])

                    output.append([uuid_zeo, "Instance", el_ind,
                                   crystOntoPrefix + "hasElementIndex", "", ""])

                    output.append([crystOntoPrefix + "hasIndexValue", "Data Property",
                                   el_iri, "", index, "xsd:integer"])

                    output.append([el_ind, "Instance", el_iri,
                                   crystOntoPrefix + "isElementIndexOf", "", ""])

        '''
        elements = self._get_elements_iri()

        for el, el_iri in elements:
            #print(el_iri)
            #output.append([el_iri, "Instance", "http:", "", "", ""])
            """ Moved to ontocrystal_ontospecies:
            output.append([el_iri, "Instance",
                           #ontoSpeciesPrefix + "Element",
                           "http://www.daml.org/2003/01/periodictable/PeriodicTable#Element",
                           "", "", ""])  # FIXME This is external. Not strictly necessary.

            symbol_iri = el_iri.replace("Element_", "ElementSymbol_")
            output.append([symbol_iri, "Instance",
                           ontoSpeciesPrefix + "ElementSymbol",
                           #"http://www.daml.org/2003/01/periodictable/PeriodicTable#Element",
                           "", "", ""])  # FIXME This is external. Not strictly necessary.

            output.append([el_iri, "Instance",
                           symbol_iri,
                           ontoSpeciesPrefix + "hasElementSymbol"
                           #"http://www.daml.org/2003/01/periodictable/PeriodicTable#Element",
                           "", "", ""])  # FIXME This is external. Not strictly necessary.

            output.append([ontoSpeciesPrefix + "value",
                           "Data Property", symbol_iri, "",
                           el, "string"])
            """

            uid = str(uuid.uuid4())
            #uuid_atom, _ = self.uuidDB.addUUID("osElement",
            #                                   "osElement_" + el,
            #                                   newUuid=uid)
            uuid_atom, _ = self.uuidDB.addUUID("http://purl.org/gc/Atom",
                                               "gcAtom_" + el,
                                               newUuid=uid)

            output.append([uuid_atom, "Instance", 
                           "http://purl.org/gc/Atom", "", "", ""])

            output.append([uuid_zeo, "Instance", uuid_atom,
                           #zeoOntoPrefix + "ChemicalComponent", "", "", ""])
                           ontoSpeciesPrefix + "hasAtom", "", ""])

            output.append([uuid_atom, "Instance", el_iri,
                           ontoSpeciesPrefix + "isElement", "", ""])

            # output.append([uuid_atom, "Instance", el_iri,
            #               ontoSpeciesPrefix + "isInstanceOf", "", ""])

            # output.append([uuid_zeo, "Instance", uuid_atom,
            #              zeoOntoPrefix + "hasChemicalComponent", "", ""])

            # Implementation of output.append([hasBibRef])
        '''

        return output
        # === end of ZeoliteMaterial._get_csv_arr_formula()
 
    def _get_elements_iri(self):
        #form = genform.GenFormula(mat_line[2])
        form = genform.GenFormula() #mat_line[2])
        form.fullFormula = self.data["formula"]
        form.getFormula()
        form.updateStatus()
        #print("formula =", form.formula)
        #print("a")

        form.getElements()

        #form.getCoeff()
        #form.verify()
        elements = form.get_elements()
        #print(elements)
        # Convert to the iri:
        #if len(elements) > 5:
        #    print("More than 5 elements")
        #    1/0

        output = []
        for el in elements:
            #print(f"Looking for '{el}'")
            iri = get_element_iri(el)
            output.append((el, iri))
        return output
        # === end of ZeoliteMaterial._get_elements_iri()


    def _get_compound_iri(self, compound):
        """ Based on the Compounds_Laura.csv get the iri.
        """
        #print(" Compound iri for", mat_line)
        output = {}

        if not isinstance(compound, str):
            return output

        #if compound is already an iri, then return just it:

        tmp = compound.strip()
        if tmp.startswith("http://www.theworldavatar.com/kb/ontospecies/Species_"):
            # dbeb0a4f-374e-4ede-b0b1-bbf9f4e16d16
            return compound

        #print("compound =", compound, type(compound))
        #1/0

        if "guest" not in self.data:
            return None

        for chem in _OSDA_COMPOUNDS_IRI:
            #print("In _get_compound_iri for", compound, "testing", chem[2].strip())
            short = chem[2].strip()
            if compound.strip() == short:
                output["name"] = chem[0].strip()

                output["formula"] = chem[1].strip()

                output["smiles"] = chem[2].strip()

                output["inchi"] = chem[3].strip()

                output["name_uuid"] = chem[4].replace("<", "").replace(">", "").strip()
                return output

        # for chem in reversed(_chemical_small):
        for chem in _chemical_small:
            short = self.data["guest"].strip().strip("|").strip()
            short = chem[0].strip()
            #print("In _get_compound_iri for", compound, "testing", short)
            if compound.strip() == short:
                output["name_uuid"] = chem[7].replace("<", "").replace(">", "").strip()

                tmp = chem[1].strip()
                if tmp != "":
                    output["name"] = tmp

                tmp = chem[3].strip()
                if tmp != "":
                    output["formula"] = tmp

                tmp = chem[5].strip()
                if tmp != "":
                    output["smiles"] = tmp

                tmp = chem[6].strip()
                if tmp != "":
                    output["inchi"] = tmp

                return output
        return None
        # === end of _get_compound_iri()

    def get_csv_arr_cif(self):
        output = []
        return output
        # === end of ZeoliteMaterial.()

    def get_csv_arr_recipe(self):
        output = []
        return output
        # === end of ZeoliteMaterial.()

    def get_csv_arr_chemical(self):
        output = []
        return output
        # === end of ZeoliteMaterial.()

    def is_reference_material(self):
        if "is_ref" in self.data:
            value = self.data["is_ref"]
        else:
            value = False
        return value
        # === end of ZeoliteMaterial.is_reference_material()

    def get_iri(self):
        return self.material_iri
        # === end of ZeoliteMaterial.get_iri()

    def is_interrupted(self):
        if "is_interrupt" in self.data:
            return self.data["is_interrupt"]
        return False

        lines = self.data["raw_data"]
        if True:
            if "_" == lines[0][1][0]:
                value = True
            else:
                value = False
        return value
        # === end of ZeoliteMaterial.is_interrupted()

    def get_material_UUID(self):
        # uuid_zeo = self.uuidDB.getUUID("ZeoliticMaterial",
        #                               "Zeolite_" + cif_line[0])
        #uuid_zeo, _ = self.uuidDB.addUUID("ZeoliticMaterial",
        #                                  "Zeolite_" + cif_line[0])
        uuid_zeo, _ = self.uuidDB.addUUID("ZeoliticMaterial",
                                          "Zeolite_" + self.data["safe_name"])

        return uuid_zeo
        # === end of ZeoliteMaterial.get_material_UUID()

        # === end of ZeoliteMaterial.()

        # === end of ZeoliteMaterial.()

        # === end of ZeoliteMaterial.()

        # === end of ZeoliteMaterial.()

    # === end of class ZeoliteMaterial

class ZeoliteDB:
    __slots__ = ["uuidDB", "frames", "zeolites", "recipes", "izatopology",
                 "_cod_doi_cif_manual",
                 #"_chemical_small"
                 ]
    def __init__(self, uuidDB=None):
        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            self.uuidDB = tools.UuidDB(uuidDB)
            logging.warning(" Creating a new uuidDB database in file '%s'.",
                            self.uuidDB.dbFilename)
        self.frames = []
        self.zeolites = []
        self.recipes = []
        self.izatopology = izatopology.IzaTopology(uuidDB=self.uuidDB)

        file_path = os.path.join("cod_doi_cif_manual.json")
        if os.path.isfile(file_path):
            with open(file_path, encoding="utf-8") as fp:
                raw = json.load(fp)
                self._cod_doi_cif_manual = raw
                #print("loaded file", file_path)
                #print(self._cod_doi_cif_manual["89055"])
        else:
            print("Error! no manual load.json file", file_path)
            self._cod_doi_cif_manual = {}

        # self.db.append(ZeoliteMaterial(uuidDB=self.uuidDB))

        # === end of ZeoliteDB.()

    def load_recipes(self, dir_list):
        """ Function to load known recipes.
        Currently function supports only the format used in IZA web-site.

        """
        output = []

        if isinstance(dir_list, list):
            for path in dir_list:
                output += self.load_recipes(path)

        elif isinstance(dir_list, str):
            if os.path.isfile(dir_list):
                # Error! Encoding is not UTF!
                # logging.warning(" In zeolite_db: Going to open file '%s'",
                #                dir_list)
                # with open(dir_list, encoding="utf-8") as f:
                with open(dir_list) as f:
                    lines = f.readlines()
                    for line in lines:
                        if len(line.strip()) == 3 or \
                          (len(line.strip()) == 4 and line.strip()[0] == "-"):
                            output.append((line.strip(), dir_list))
                            break
            elif os.path.isdir(dir_list):
                paths = os.listdir(dir_list)
                for path in paths:
                    output += self.load_recipes(os.path.join(dir_list, path))
        else:
            logging.error("Invalid input type in load_recipes(): %s",
                          str(dir_list))

        return output

    def get_guest_list(self, guest_line, msg=""):
        guest_list = []

        remain = guest_line
        #if remain.find("H2O"):
        #    remain = remain.replace("H2O", "")
        #    guest_list.append("H2O")

        # for chemical in reversed(_chemical_small):
        for chemical in _chemical_small:
            chem = chemical[0]
            #print("Searching '", chem, "'", sep="")
            if remain.find(chem) >= 0:
                remain = remain.replace(chem, "")
                guest_list.append(chem)

            #short = self.data["guest"].strip().strip("|").strip()
            #if chem[0] == short:
            #    output["name_uuid"] = chem[7].replace("<", "").replace(">", "").strip()

        #for chemical in _chemical_small_2:
        #    chem = chemical[0]
        #    if remain.find(chem) >= 0:
        #        remain = remain.replace(chem, "")
        #        guest_list.append(chem)

            #short = self.data["guest"].strip().strip("|").strip()
            #if chem[0] == short:
                #output["name_uuid"] = chem[7].replace("<", "").replace(">", "").strip()

        if len(guest_line):
            #print("remain: '", remain, "'       : ", guest_line, " vs ",
            #      guest_list, "' ", msg, sep="")
            if remain.find("N") >= 0:
                print("============================")

        return guest_list

    def load_iza_data(self):
        logging.error("load_iza_data is not implemented")

        # These two files are generated by iza.py script:
        file_cif = "cifs.csv"
        file_mat = "izamat.csv"
        #file_mat = "zeomat_updated.csv"

        if not os.path.isfile(file_cif):
            logging.error(" Missing file '%s' for load_zeolites().", file_cif)
            return
        data_cif = tools.readCsv(file_cif)

        if not os.path.isfile(file_mat):
            logging.error(" Missing file '%s' for load_zeolites().", file_mat)
            return
        data_mat = tools.readCsv(file_mat)

        # Removing the header lines:
        data_cif = data_cif[1:]
        data_mat = data_mat[1:]

        err_count = self.check_loaded_iza_data(data_cif, data_mat)
        if err_count > 0:
            logging.error(" Detected %d errors loading iza_data", err_count)

        self.zeolites = []
        for i_line, (line_cif, line_mat) in enumerate(zip(data_cif, data_mat)):
            #   "source", "frame", "name", "guest", "cif_path", "doi",
            #   "bib_path", "icsd", "cod_id", "recipe"

            zeolite = ZeoliteMaterial(uuidDB=self.uuidDB)
            zeolite.data["source"].append("izamat")
            zeolite.data["name"].append(line_mat[1])
            zeolite.data["formula"] = line_mat[2]  # FIXME remove -ABW
            #self.data["formula"] = line_mat[6]
            zeolite.data["frame"] = line_mat[3]
            zeolite.data["guest"] = line_mat[4]

            #print("Starting to detect chem in", line_mat[4])
            #zeolite.data["guest"] = line_mat[4]
            zeolite.data["guest-list"] = self.get_guest_list(line_mat[4],
                                                             msg=line_mat[0] + line_mat[1])
            #if line_mat[4].find("(TMA+)4|") >= 0:
                #print("debug-chem")
                #1/0
            #    pass
            #print(zeolite.data["guest-list"], "in", line_mat[1])

            data_mat[i_line].append(zeolite.data["guest-list"])
#            tmp = line_mat[2].strip()
#            if tmp != "":
#                zeolite.data["full_formula"] = tmp

            zeolite.data["safe_name"] = line_mat[17]
            if line_cif[3] == "TRUE":
                print("Found ref materail for", zeolite.data["frame"])
                zeolite.data["is_ref"] = True
            else:
                zeolite.data["is_ref"] = False

            if self._get_cif_file(line_cif) != "":
                zeolite.data["cif_path"] = self._get_cif_file(line_cif)

            if line_cif[4].strip().startswith("10."):
                zeolite.data["doi"] = line_cif[4].strip()

            bibline = line_mat[12].strip()
            if not bibline.startswith("?") and bibline.find("none") < 0 \
               and bibline != "":
                tmp = bibline
                tmp = bibline.replace("/", "_")
                tmp = tmp.replace("<", "_").replace(">", "_")
                tmp = os.path.join("bibfiles", tmp + ".bib")
                zeolite.data["bib_path"] = tmp
                #print(bibline)

            err_count = zeolite.check()

            self.zeolites.append(zeolite)
            if err_count == 0:
                pass
            else:
                #print("Found errors", err_count, "in material", zeolite.data["safe_name"])
                pass

        #################################################
        # Loading extended data from IZA website (reference materials):
        self.izatopology.load()

        tools.writeCsv("zeomat-temp-temp.csv", data_mat)

        print("FIXME: remove debug version :10")
        #self.zeolites = self.zeolites[:10]

        # === end of ZeoliteDB.load_iza_data()

    def _get_material_distribution(self, option):
        zeo_distr = {}

        if option == "iza" or "iza" in option:
            #print("====================================")
            
            file_mat = "izamat.csv"
            if not os.path.isfile(file_mat):
                logging.error(" Missing file '%s' for load_zeolites().", file_mat)
                return
            data_mat = tools.readCsv(file_mat)[1:]

            for line_mat in data_mat:
                code = line_mat[3]
                #code = code.replace("_", "-")
                if code in zeo_distr:
                    zeo_distr[code]["iza"] += 1
                else:
                    zeo_distr[code] = {"iza": 1}

        if option == "icsd" or "icsd" in option:

            filename = os.path.join("ontozeolite", "icsddata", "icsddata.json")
            #data = tools.readCsv(filename)
            with open(filename, encoding="utf-8") as fp:
                data = json.load(fp)

            #print("FIXME Loaded icsddata, contains", len(data["all"]), "items :10")
            #for entry in data["all"][:10]:

            for entry in data["all"]:
                #print(entry)
                zeo = ZeoliteMaterial(uuidDB=self.uuidDB)
                zeo.data["source"].append("jpcrd392010033102.pdf")
                if "FTC" in entry:
                    zeo.data["frame"] = entry["FTC"]
                else:
                    if "FTP" in entry:
                        #print("     found framework", entry["FTP"])
                        zeo.data["frame"] = entry["FTP"]
                        pass
                    else:
                        if "TOPOS" in entry:
                            zeo.data["frame"] = entry["TOPOS"]
                            pass
                        else:
                            logging.error("Not found framework")

                    continue

                code = zeo.data["frame"]
                code = code.replace("-", "_")
                if code in ["RON", "PAR"]:
                    code = "_" + code
                if code not in zeo_distr:
                    zeo_distr[code] = {}

                #print(code)
                if "icsd" in zeo_distr[code]:
                    zeo_distr[code]["icsd"] += 1
                else:
                    zeo_distr[code]["icsd"] = 1

                # === end of for(entry)

        else:
            print("Unknown option", option, "in ZeoliteDB._get_material_distribution()")


        if option == "osda" or "icsd" in option:
            osda_list = osda_vs_iza.load_osda_data()
            osda_list = osda_list[1:]

            #print("FIXME Loaded icsddata, contains", len(data["all"]), "items :10")
            #for entry in data["all"][:10]:

            for i_line, osda in enumerate(osda_list):
                #print(entry)
                #code = line[24].stirp().upper()
                code = osda.get_framework()
                code = code.replace("-", "_")
                #if code in ["RON", "PAR"]:
                #    code = "_" + code
                #if code not in zeo_distr:
                #    zeo_distr[code] = {}
                if code in ["SYT"]:
                    code = "-" + code
                    if code not in zeo_distr:
                        zeo_distr[code] = {}

                    
                #print(code)
                if code not in zeo_distr:
                    if code in ["*BEA", "*UOE", "*MRE", "*CTH", "*STO", "*SFV", "*_SVY", "*_ITN",
                                "ASU_12", "ASU_14", "ASU_16", "SU_12", "SU_74", "SU_77", "SU_M", "SU_MB",
                                "ITQ_21", "ITQ_43", "NUD_1", "Quartz"]:
                        #continue
                        if code not in zeo_distr:
                            print("Unknown framework '", code, "'. At line ", i_line, ".", sep="")
                            zeo_distr[code] = {}
                            #continue
                #else:
                #print(i_line)
                if code == "":
                    print("Not defined framework code on line", i_line)
                    continue
                if "osda" in zeo_distr[code]:
                        zeo_distr[code]["osda"] += 1
                else:
                        zeo_distr[code]["osda"] = 1

                # === end of for(entry)

        #print(zeo_distr)
        dist_list = [["Code", "IZA", "ICSD", "OSDA", "TWA"]]
        for k, v in zeo_distr.items():
            #print(k)
            total = 0
            if "iza" in v:
                iza = v["iza"]
                total += iza
            else:
                iza = None

            if "icsd" in v:
                icsd = v["icsd"]
                total += icsd
            else:
                icsd = None

            if "osda" in v:
                osda = v["osda"]
                total += osda
            else:
                osda = None
            dist_list.append([k, iza, icsd, osda, total])

        for i_line, line in enumerate(zeo_distr):
            #print(i_line, line)
            pass
        tools.writeCsv("material_distr.csv", dist_list)

        #return zeo_distr
        # === end of ZeoliteDB._get_material_distribution()

    def _get_cif_file(self, cif_line):
        if cif_line[5] != "":
            return cif_line[5]
        elif cif_line[6] != "":
            return cif_line[6]
        elif cif_line[7] != "":
            return cif_line[7]
        elif cif_line[8] != "" and cif_line[8] != "?" and cif_line[8] != "None":
            return os.path.join("ccdc", cif_line[8])
        return ""
        # === end of OntoZeolite.get_cif_file()

    def check_loaded_iza_data(self, data_cif, data_mat):
        err_count = 0

        if data_mat is None:
            logging.error(" Missing data from izamat.csv")
            err_count += 1

        if data_cif is None:
            logging.error(" Missing data from cifs.csv")
            err_count += 1

        if len(data_cif) != len(data_mat):
            logging.error(" Different sizes of data_cif and data_mat: %d vs %d.",
                          len(data_cif), len(data_mat))
            err_count += 1

        # for i in range(len(self.data_cif)):
        for i in range(min(len(data_cif), len(data_mat))):
            line_cif = data_cif[i]
            line_mat = data_mat[i]
            if line_cif[0] != line_mat[0]:
                logging.error(" Different zeolite ID in column 0 '%s'" +
                              " vs '%s' in ontozeolite.",
                               line_cif[0], line_mat[0])
                err_count += 1

        return err_count
        # === end of ZeoliteDB.check_loaded_iza_data()

    def load_icsd_paper(self):
        """
        Loading data from paper doi:10.1063/1.3432459
        All necessary information is stored in: ontozeolite/icsddata/
        Processing is done by file jpcrd392010033102.py
        """

        filename = os.path.join("ontozeolite", "icsddata", "icsddata.json")
        #data = tools.readCsv(filename)
        with open(filename, encoding="utf-8") as fp:
            data = json.load(fp)

        #print("FIXME Loaded icsddata, contains", len(data["all"]), "items :10")
        #for entry in data["all"][:10]:

        #print("in load_icsd:", self._cod_doi_cif_manual["89055"])
        count = 0
        for entry in data["all"]:
            #print("Starting ICSD", entry["ICSD"])
            #if count > 10:
            #    break
            #print(entry)

            zeo = ZeoliteMaterial(uuidDB=self.uuidDB)
            zeo.data["source"].append("jpcrd392010033102.pdf")
            if "FTC" in entry:
                zeo.data["frame"] = entry["FTC"]
            else:
                if "FTP" in entry:
                    #print("     found framework", entry["FTP"])
                    zeo.data["frame"] = entry["FTP"]
                    pass
                else:
                    if "TOPOS" in entry:
                        zeo.data["frame"] = entry["TOPOS"]
                        pass
                    else:
                        logging.error("Not found framework")
                        continue

            if "ICSD" in entry:
                #zeo.icsd = entry["ICSD"]
                zeo.data["icsd"] = str(entry["ICSD"])

            if "Ref" in entry:
                zeo.data["bib_code"] = entry["Ref"]
                if entry["Ref"] in _ICSD_DOI_LIST:
                    doi = _ICSD_DOI_LIST[entry["Ref"]] 
                    doi = doi.replace("http://dx.doi.org/", "")
                    zeo.data["doi"] = doi

            if "icsd" in zeo.data:
                doi = None
                if "doi" in zeo.data:
                    doi = zeo.data["doi"]
                cod_id = self._get_cod(zeo.data["icsd"], doi=doi, ref=entry["Ref"])

                #print("     cod", cod_id, "for", zeo.data["icsd"], type(zeo.data["icsd"]))
                if cod_id:
                    zeo.data["cod_id"] = cod_id[0]
                #print("assigned cod_id", cod_id)
                    count += 1

            if "cif_path" in zeo.data:
                cif_path = self._get_cif_path(zeo.data["icsd"])
                if len(cif_path) > 0:
                    zeo.data["cif_path"] = cod_id[0]

            if "Name" in entry:
                tmp = entry["Name"].strip()
                zeo.data["name"].append(tmp)
                tmp = tmp.replace(" ", "_")
                tmp = tmp.replace("-", "_")
                zeo.data["safe_name"] = tmp

            zeo.data["is_ref"] = False
            if "FormOrig" in entry:
                tmp = entry["FormOrig"]
                base_formula, guest, guest_list = self._icsd_to_guest(tmp)
                zeo.data["formula"] = tmp
                zeo.data["guest"] = guest
                zeo.data["guest-list"] = guest_list

            #print("got zeo:", zeo.data)
            self.zeolites.append(zeo)

        #print("after loop load_icsd:", self._cod_doi_cif_manual["89055"])
        print("Found", count, "cif files in COD")

        with open("cod_doi_cif_manual_out.json", "w", encoding="utf-8") as fp:
            json.dump(self._cod_doi_cif_manual, fp, indent=4)

        # === end of ZeoliteDB.load_icsd_paper()

    def _get_cod(self, icsd, ref=None, doi=None):
        if icsd not in self._cod_doi_cif_manual:
            self._cod_doi_cif_manual[icsd] = {}

        #print("ssssssssssssssssss")
        if "cod_id" in self._cod_doi_cif_manual[icsd]:
            #print("ssss found cod_id")
           #self._cod_doi_cif_manual[icsd]["cod_id"] != "" and \
           #self._cod_doi_cif_manual[icsd]["cod_id"] != []:
            return self._cod_doi_cif_manual[icsd]["cod_id"], \
                   self._cod_doi_cif_manual[icsd]["cif_path"]


        if "__cod_id" not in self._cod_doi_cif_manual[icsd]:
            self._cod_doi_cif_manual[icsd]["__cod_id"] = ""
        #elif self._cod_doi_cif_manual[icsd]["__cod_id"] == "":
        #    self._cod_doi_cif_manual[icsd]["__cod_id"] = []
        #elif self._cod_doi_cif_manual[icsd]["__cod_id"] == []:
        #    self._cod_doi_cif_manual[icsd]["__cod_id"] = ""
            
            #print("tttttttttttttttttttt", doi, ref)
            #self._cod_doi_cif_manual[icsd]["__cod_id"] = ""

        if "__cif_path" not in self._cod_doi_cif_manual[icsd]:
            self._cod_doi_cif_manual[icsd]["__cif_path"] = []
        #elif self._cod_doi_cif_manual[icsd]["__cif_path"] == "" :
        #    self._cod_doi_cif_manual[icsd]["__cif_path"] = []

        if True:
            if doi:
                if doi in _COD_DOI_CIF:
                    cifs = _COD_DOI_CIF[doi]
                    if len(cifs) > 0:
                        self._cod_doi_cif_manual[icsd]["cifs"] = []

                    for c in cifs:
                        lines = self._get_unit_cell(c)
                        self._cod_doi_cif_manual[icsd]["cifs"].append([c, lines])
                #print("Found cif files", zeo.data["doi"], _COD_DOI_CIF[zeo.data["doi"]])

            if doi:
                self._cod_doi_cif_manual[icsd]["doi"] = doi
            else:
                self._cod_doi_cif_manual[icsd]["doi"] = ""

            if ref:
                self._cod_doi_cif_manual[icsd]["ref"] = ref
            else:
                self._cod_doi_cif_manual[icsd]["ref"] = ""

        return None
        # === end of ZeoliteDB._get_cod()

    def _get_unit_cell(self, cif_path):
        output = []
        with open(cif_path, encoding="utf-8") as fp:
            lines = fp.readlines()
            for line in lines:
                short = line.strip()
                pos = short.find("#")
                if pos >= 0:
                    short = short[:pos]
                if short.find("_cell_length_") >= 0 or \
                   short.find("_cell_angle_") >= 0:
                    output.append(line.strip())
        return output
        # === end of ZeoliteDB._get_unit_cell()

    def _icsd_to_guest(self, formula_orig):
        tmp = formula_orig

        base_formula = ""
        guest = ""
        guest_list = []
        for base in BASE_LIST:

            if tmp.find(base) >= 0:
                base_formula = base
                tmp = tmp.replace(base, "")
                break

            pass

        return base_formula, guest, guest_list
        # === end of ZeoliteDB._icsd_to_guest()

    def load_osda(self):

        osda_list = osda_vs_iza.load_osda_data()
        osda_list = osda_list[1:]

        output_list = []
        for i_line, osda in enumerate(osda_list):
           doi = osda.get_doi()

           smiles = osda.get_smiles()
           smilesArr = smiles.split(" + ")

           tmp = "smiles"
           s = "smil"
           output_list.append([doi, osda.get_orig_sda(), osda.get_sda(),
                               osda.get_formula(), tmp, s])

    #__slots__ = ["data", "keys", "uuidDB",
    #             "source", "frame", "name", "guest", "cif_path", "doi",
    #             "bib_path", "bib_code", "icsd", "cod_id", "recipe"]

           zeo = ZeoliteMaterial(uuidDB=self.uuidDB)
           zeo.data["safe_name"] = osda.get_framework() + "_" + str(i_line)
           zeo.data["source"].append("osda")
           zeo.data["doi"] = doi
           zeo.data["name"] = "name"
           zeo.data["formula"] = "formula"
           zeo.data["guest"] = smiles
           zeo.data["guest-list"] = smilesArr
           #zeo.data["icsd"]
           zeo.data["frame"] = osda.get_framework()
 
           self.zeolites.append(zeo)

        print("Size of list =", len(output_list))
        for line in output_list:
            #print(line)
            pass

        pass
        # === end of ZeoliteDB.load_osda()

    def save(self, filename="", ext="json"):
        """
        Saving the database to a file. Default values are:

        """
        if ext == "csv":
            if filename == "":
                file_path = "zeolite_db.csv"

            data_csv = []
            for zeo in self.zeolites:
                line = ["", "" ]
                if zeo.frame is not None:
                    line[0] = zeo.frame
                
                data_csv.append(line)
            tools.writeCsv(file_path, data_csv)

        elif ext == "json":
            if filename == "":
                file_path = "zeolite_db.json"

            output = {"zeolites": []}
            for zeo in self.zeolites:
                if "frame" not in zeo.data:
                    logging.error(" In zolite_db.save():" +
                                  " Missing 'frame' in zeolite")
                                  #zeo.data["frame"], zeo.data["name"])
                    continue

                if "name" not in zeo.data:
                    logging.error(" In zolite_db.save():" +
                                  " Missing 'name' in zeolite %s",
                                  zeo.data["frame"])
                    continue

                if "safe_name" not in zeo.data:
                    logging.error(" Missing safe_name for zeolite for %s, %s",
                                  zeo.data["frame"], zeo.data["name"])
                    continue

                output["zeolites"].append(zeo.data)

            with open(file_path, "w", encoding="utf-8") as fp:
                json.dump(output, fp, indent=4)

            print("Finished zolite_db.py, saved to json", file_path)
        # === end of ZeoliteDB.save()

    def load(self, filename=""):

        self.recipes = self.load_recipes("recipes")

        self.izatopology.load()
        #self.load_iza_data()

        if filename == "":
            file_path = "zeolite_db.json"
        else:
            file_path = filename

        if not os.path.isfile(file_path):
            logging.error(" In zeolite_db: missing zeolite database '%s'",
                          file_path)
            #self.data = {}  # << Don't overwrite if file does not exist.
        else:
            with open(file_path, encoding="utf-8") as fp:
                raw = json.load(fp)
                # raw = json.load(fp, parse_constant=lambda x: x.lower() == "true")
                if "zeolites" in raw:
                    for data in raw["zeolites"]:
                        zeo = ZeoliteMaterial(uuidDB=self.uuidDB)
                        zeo.data = data
                        if "frame" in data:
                            self.zeolites.append(zeo)
                        else:
                            logging.error(" Missing 'frame' in zeolite_db '%s'",
                                          file_path)
                else:
                    for key in raw.keys():
                        data = raw[key]
                        zeo = ZeoliteMaterial(uuidDB=self.uuidDB)
                        zeo.data = data
                        if "frame" in data:
                            self.zeolites.append(zeo)
                        else:
                            logging.error(" Missing 'frame' in zeolite_db '%s'",
                                          file_path)
                ''' old version:
                for data in raw["zeolites"]:
                    zeo = ZeoliteMaterial(uuidDB=self.uuidDB)
                    zeo.data = data
                    if "frame" in data:
                        self.zeolites.append(zeo)
                    else:
                        logging.error(" Missing 'frame' in zeolite_db '%s'",
                                      file_path)
                '''
        #print("Number of zeolites =", len(self.zeolites))
        #1/0
        # === end of ZeoliteDB.load()

    def get_csv_arr_framework(self, framework_code):
        """
        Return the data for framework only (based on data from IZA website).

        """
        output = []
        _code = framework_code
        #_code = _code.replace("-", "").replace("_", "")
        _code = _code.replace("_", "-")
        _code = _code.strip().upper()
        frames = zeolist.getZeoList(["main", "new"])

        #print("Number of frames:", len(frames))
        if _code not in frames and "-" + _code not in frames:
            logging.error(" In zeolite_db: Invalid frameworkCode '%s'",
                          framework_code)

        uuid_zeoframe, _ = self.uuidDB.addUUID("ZeoliteFramework",
                                               "ZeoFramework_" + _code)

        output.append([uuid_zeoframe, "Instance", zeoOntoPrefix + "ZeoliteFramework",
                       "", "", ""])

        output.append([zeoOntoPrefix + "hasFrameworkCode",
                       "Data Property", uuid_zeoframe, "",
                       framework_code.strip(' "'), "string"])

        zeolites = self.get_framework_materials(framework_code)
        # print(">>> lines = '", lines, "'.", sep="")
        if len(zeolites) > 0:
            value = zeolites[0].is_interrupted()

            output.append([zeoOntoPrefix + "isInterrupted", "Data Property",
                           uuid_zeoframe, "", value, "boolean"])

            output.append([zeoOntoPrefix + "isIntergrowth", "Data Property",
                           uuid_zeoframe, "", False, "boolean"])

        return output
        # === end of ZeoliteDB.()

    def get_framework_UUID(self, frameworkCode):
        """ Return the IRI of the framework by its code (usually 3-letter).

        """
        # uuid_zeoframe = self.uuidDB.getUUID("ZeoliteFramework",
        #                                     "Zeolite_" + frameworkCode)
        uuid_zeoframe, _ = self.uuidDB.addUUID("ZeoliteFramework",
                                               "ZeoFramework_" + frameworkCode)

        return uuid_zeoframe
        # === end of ZeoliteDB.get_framework_UUID()

    def get_csv_arr_topology(self, subject, predicate, code):
        #logging.error(" In zeolite_db: Not implemented get_csv_arr_topology")
        #output = []

        return self.izatopology.get_csv_arr_topology(subject, predicate, code)
        # === end of ZeoliteDB.get_csv_arr_topology()

    def get_framework_materials(self, framework):
        output = []
        #framework = framework.replace("-", "").replace("_", "")
        framework = framework.replace("_", "-")
        for mat in self.zeolites:
            #print("Number of zeolites:", len(self.zeolites))
            #print(type(self.zeolites))
            #print(mat.data)
            if mat.data["frame"] == framework:
                output.append(mat)
        print("Number of zeo for framework", framework, len(output))
        #1/0
        return output
        # === end of ZeoliteDB.get_framework_materials()

    def getCsvArrConstituent(self, subject, predicate):
        logging.error(" Not implemented OntoZeolite.getCsvArrConstituent()")
        output = []

        return output
        # === end of ZeoliteDB.getCsvArrConstituent()

        # === end of ZeoliteDB.()

        # === end of ZeoliteDB.()

        # === end of ZeoliteDB.()

    # === end of class ZeoliteDB

if __name__ == "__main__":
    # Preparation of the database, by combining data from different sources:
    zeo_db = ZeoliteDB()
    zeo_db.load_iza_data()

    print("FIXME: Fix error messages from zeolite_db")
    zeo_db.load_icsd_paper()
    # TODO add other sources

    zeo_db.load_osda()

    zeo_db.save()

    zeo_db._get_material_distribution(["iza", "icsd", "osda"])
