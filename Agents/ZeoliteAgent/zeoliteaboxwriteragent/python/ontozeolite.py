"""
Python program to prepare .csv file for OntoZeolite ontology abox.

There is a single .csv file with data for all zeolitic materials.
So this class loads all of them and on demand can generate any zeolite.

Main functions:
load_zeolites() - loads the entire database
getCsvArr() - generates abox rows. ???? TODO
"""

import os
import json

import logging

import bib2csv

import tools
# from ontocrystal_datatypes import *
import ontocrystal_datatypes as ocdt

# import csv_maker

# logging.basicConfig(level=logging.WARNING)
logging.basicConfig(level=logging.INFO)

zeoOntoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"


def load_recipes(dir_list):
    """ Function to load known recipes.
    Currently function supports only the format used in IZA web-site.

    """
    output = []

    if isinstance(dir_list, list):
        for path in dir_list:
            output += load_recipes(path)

    elif isinstance(dir_list, str):
        if os.path.isfile(dir_list):
            # Error! Encoding is not UTF!
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
                output += load_recipes(os.path.join(dir_list, path))
    else:
        logging.error("Invalid input type in load_recipes(): %s",
                      str(dir_list))

    return output

RECIPES = load_recipes("recipes")
# print("Number of recipes is", len(RECIPES))
# print(RECIPES)


class OntoZeolite:
    """"
    Database for all known zeolitic materials.
    As such, it does not have an itemName and className.
    itemName and className are required for the getCsvArrXXX() functions.
    Are they? FIXME
    """

    # __slots__ = ["iza_file_path", "iza_data_base",
    #             "abox_prefix", "tbox_prefix",
    #             "", "", "", "", "", "", "", "",]

    def __init__(self, uuidDB=None,  # className, itemName,
                 tPrefix=None, aPrefix=None):  # , unit = None):

        """
        if "" == itemName:
            logging.error(" In OntoZeolite not specified itemName. ")
            self.itemName = ""
            return
        else:
            self.itemName = itemName

        if "" == className:
            logging.error(" In OntoZeolite r44444444444")
            self.className = "Zeolite"
        else:
            self.className = className
        """

        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            self.uuidDB = tools.UuidDB(uuidDB)
            logging.warning(" Creating a new uuidDB database in file '%s'.",
                            self.uuidDB.dbFilename)

        if tPrefix is None:
            self.tbox_prefix = ""
        else:
            self.tbox_prefix = tPrefix

        if aPrefix is None:
            self.abox_prefix = ""
        else:
            self.abox_prefix = aPrefix

        self.zeoOntoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"

        self.iza_file_path = os.path.join("iza-data.json")
        self.iza_data_base = {}
        with open(self.iza_file_path, encoding="utf-8") as f:
            self.iza_data_base = json.load(f)

        self.dataCif = None
        self.data2 = None
        # === end of OntoZeolite.__init__()

    def getIzaByCode(self, code):
        output = {}
        for idata, data in enumerate(self.iza_data_base):
            if "Framework" not in data:
                logging.error(" Broken IZA json: no 'Framework' key," +
                              " number %s.", idata)
                continue
            if data["Framework"].lower() == code.lower():
                # print("Found a framework", code)
                output = data
                break

        return output
        # === end of OntoZeolite.getIzaByCode()

    # def load_zeolites(self, filename):
    def load_zeolites(self):
        """
        if not os.path.isfile(filename):
            logging.error(" Missing file for load_zeolites().")
            return
        self.dataCif = tools.readCsv(filename)
        """
        # logging.warning(" In OntoZeolites.load_zeolites redundant argument")

        fileCif = "cifs.csv"
        file2 = "test.csv"

        if not os.path.isfile(fileCif):
            logging.error(" Missing file '%s' for load_zeolites().", fileCif)
            return
        self.dataCif = tools.readCsv(fileCif)

        if not os.path.isfile(file2):
            logging.error(" Missing file '%s' for load_zeolites().", file2)
            return
        self.data2 = tools.readCsv(file2)

        # Removing the header lines:
        self.dataCif = self.dataCif[1:]
        self.data2   = self.data2[1:]

        self.check_loaded()

        # === end of OntoZeolite.load_zeolites()

    def check_loaded(self):
        err_count = 0
        if len(self.dataCif) != len(self.data2):
            logging.error(" Different sizes of dataCif and data2: %d vs %d.",
                          len(self.dataCif), len(self.data2))
            err_count += 1

        # for i in range(len(self.dataCif)):
        for i in range(min(len(self.dataCif), len(self.data2))):
            line_cif = self.dataCif[i]
            line_ext = self.data2[i]
            if line_cif[0] != line_ext[0]:
                logging.error(" Different zeolite ID in column 0 '%s'",
                              " vs '%s'.", line_cif[0], line_ext[0])
                err_count += 1

        return err_count
        # === end of OntoZeolite.check_loaded()

    def getMaterialIDs(self, frameworkCode):
        """

        """
        # logging.error(" Not implemented OntoZeolite.getMaterialIDs()")
        output = []

        for il, line in enumerate(self.dataCif):
            if line[1] == frameworkCode or line[1] == "_" + frameworkCode:
                name = self.data2[il][0]
                if line[0] != name:
                    logging.error(" Mismatch zeolite ID in column 0 '%s'",
                                  " vs '%s'.", line[0], name)
                output.append((line, self.data2[il]))

        if len(output) == 0:
            logging.error(" No zeolites for framework '%s'.", frameworkCode)

        return output
        # OntoZeolite.getMaterialIDs()

    def getCsvArrFramework(self, frameworkCode, subject, predicate):
        logging.error(" Not implemented OntoZeolite.getCsvArrFramework()")

        output = []

        uuid_zeoframe, _ = self.uuidDB.addUUID("ZeoliteFramework",
                                               "ZeoFramework_" + frameworkCode)

        output.append([uuid_zeoframe, "Instance", "ZeoliteFramework",
                       "", "", ""])

        output.append([self.zeoOntoPrefix + "hasFrameworkCode",
                       "Data Property", uuid_zeoframe, "",
                       frameworkCode.strip(' "'), "string"])

        lines = self.getMaterialIDs(frameworkCode)
        if "_" == lines[0][1][0]:
            value = True
        else:
            value = False
        output.append([self.zeoOntoPrefix + "isInterrupted", "Data Property",
                       uuid_zeoframe, "", value, "boolean"])

        output.append([self.zeoOntoPrefix + "isIntergrowth", "Data Property",
                       uuid_zeoframe, "", False, "boolean"])

        return output
        # === end of OntoZeolite.getCsvArrFramework()

    def getFrameworkUUID(self, frameworkCode):
        # logging.error(" Not implemented OntoZeolite.getFrameworkUUID()")
        # uuid_zeoframe = self.uuidDB.getUUID("ZeoliteFramework",
        #                                     "Zeolite_" + frameworkCode)
        uuid_zeoframe, _ = self.uuidDB.addUUID("ZeoliteFramework",
                                               "ZeoFramework_" + frameworkCode)

        return uuid_zeoframe
        # === end of OntoZeolite.getFrameworkUUID()

    def getMaterialUUID(self, cifLine):
        # logging.error(" Not implemented OntoZeolite.getMaterialUUID()")
        # uuid_zeo = self.uuidDB.getUUID("ZeoliticMaterial",
        #                               "Zeolite_" + cifLine[0])
        uuid_zeo, _ = self.uuidDB.addUUID("ZeoliticMaterial",
                                          "Zeolite_" + cifLine[0])

        return uuid_zeo
        # === end of OntoZeolite.getFrameworkUUID()

    def getCsvArrMaterial(self, cifLine, subject, predicate):
        """
        Here 'subject' is the parent Framework of this material.
        'predicate' is not used?
        cifLine - a single line from the csv file.
        """
        logging.error(" Not implemented OntoZeolite.getCsvArrMaterial():" +
                      " '%s', framework: '%s'.", cifLine[0], cifLine[1])
        output = []

        uuid_zeo, _ = self.uuidDB.addUUID("ZeoliticMaterial",
                                          "Zeolite_" + cifLine[0])

        output.append([uuid_zeo, "Instance", "ZeoliticMaterial", "", "", ""])
        output.append([subject, "Instance", uuid_zeo, "hasZeolite", "", ""])

        # output.append([self.zeoOntoPrefix + "hasFrameworkCode",
        #                "Data Property", uuid_zeoframe, "",
        #                frameworkCode.strip(' "'), "string"])

        if cifLine[3] == "TRUE":
            value = True
        else:
            value = False
        output.append([self.zeoOntoPrefix + "isReferenceZeolite",
                       "Data Property", uuid_zeo, "", value, "boolean"])

        output.append([self.zeoOntoPrefix + "isHypothetic",
                       "Data Property", uuid_zeo, "", False, "boolean"])

        """
        output.append([self.zeoOntoPrefix + "isMineral",
                       "Data Property", \
                       uuid_zeo, "", False, "boolean"])

        output.append([self.zeoOntoPrefix + "isSynthetic",
                       "Data Property", \
                       uuid_zeo, "", False, "boolean"])
        """

        return output
        # === end of OntoZeolite.getCsvArrFramework()

    def _get_recipe_by_name(self, recipes, zeoCode):
        output = []
        for recp in recipes:
            if recp[0] == zeoCode:
                output.append(recp[1])
        return output
        # === end of OntoZeolite._get_recipe_by_name()

    def getCsvArrRecipe(self, cifLine, subject, predicate):
        """
        Here 'subject' is the zeolite material (with uuid).
             'predicate' is not used.
             'cifLine' is a line from csv file with info for one zeolite 
        """
        # logging.error(" Not implemented OntoZeolite.getCsvArrRecipe()")
        #print(">>> cifLine =", cifLine)
        output = []

        # filePath = os.path.join("recipes", "p401.txt")
        filePaths = self._get_recipe_by_name( RECIPES, cifLine[1] )
        if len(filePaths) > 1:
            logging.warning(" Detected %s recipes for code %s: %s" +
                            " using only first of them",
                            len(filePath), cifLine[1], filePaths)
        elif len(filePaths) == 0:
            return output

        filePath = filePaths[0]
        print(" Loading recipe from file", filePath )
        if not os.path.isfile(filePath):
            logging.error(" Recipe file '%s' does not exist" +
                          " in OntoZeolite.getCsvArrRecipe().", filePath)
            return output

        uuid_recipe, _ = self.uuidDB.addUUID("Recipe", "Recipe_" + cifLine[0])

        output.append([uuid_recipe, "Instance", "Recipe", "", "", ""])
        output.append([subject, "Instance", uuid_recipe, "hasRecipe", "", ""])

        # got_batch_comp = False
        # key_batch_comp = "Batch Composition"
        # got_source_mat = False
        # key_source_mat = "Source Materials"

        self.section_names = ["Batch Composition", "Source Material",
                              "Batch Preparation", "Crystallization",
                              "Product Recovery", "Reference", "Note",
                              "Product Characterization"]

        #with open(filePath, encoding="utf-8") as fp:
        with open(filePath) as fp:
            lines = fp.readlines()

        sections = self._recipeToSections(lines)
        self._checkSections(filePath, sections)

        if False:
            for key in sections.keys():
                print(">>> key =", key)
                for line in sections[key]["value"]:
                    print("   ", line.strip())

        # for key in sections.keys():
        for key in sections:
            if "Batch Preparation" == key:
                output += self._get_csv_arr_batch_prep(sections[key],
                                                       uuid_recipe)
            elif "Product Recovery" == key:
                output += self._get_csv_arr_prod_recov(sections[key],
                                                       uuid_recipe)
            elif "Source Material" == key:
                output += self._get_csv_arr_source_mat(sections[key],
                                                       uuid_recipe)
            elif "Batch Composition" == key:
                output += self._get_csv_arr_batch_comp(sections[key],
                                                       uuid_recipe)
            elif "Crystallization" == key:
                output += self._get_csv_arr_crystally(sections[key],
                                                      uuid_recipe)
            elif "Product Characterization" == key:
                output += self._get_csv_arr_prod_char(sections[key],
                                                      uuid_recipe)

            elif "Reference" == key:
                output += self._get_csv_arr_refs(sections[key],
                                                 uuid_recipe, filePath)
            elif "Note" == key:
                output += self._get_csv_arr_note(sections[key], uuid_recipe)

            else:
                print("    not processed recipe '" + key + "'.")

        """
        for il in range(len(lines)):
            line = lines[il]
            #print(line.strip())
            short = line.strip().lower()

            if short.startswith(keyBatchComp.lower()):
                #print("Batch comp: '" + line.strip() + "'.")
                gotBatchComp = True
                if lines[il+1].strip() != "":
                    logging.error(" In recipe file '" + filePath + "'" + \
                                   " need empty line after batch comp." + \
                                   " But got '" + lines[il+1] + "'.")
                txtBatchComp = line[len(keyBatchComp) :].strip()

                output.append([self.zeoOntoPrefix + "hasBatchComposition", \
                                 "Data Property", uuid_recipe, "", \
                                 txtBatchComp, "string"])


            if short.startswith(keySourceMat.lower()):
                gotSourceMat = True
                dline, sourceMat = self._getSourceMat(lines, il)
                il += dline
                for mat in sourceMat:
                    output.append([self.zeoOntoPrefix + "hasSourceMaterial", \
                                 "Data Property", uuid_recipe, "", \
                                 mat, "string"])


                pass
            pass # for line
        """

        return output
        # === end of OntoZeolite.getCsvArrRecipe()

    def _isSectionStart(self, line):
        for key in self.section_names:
            # print(line.strip(), " vs ", key)
            if line.strip().lower().startswith(key.lower()):
                # print("matched key ")
                return True, key
        return False, ""
        # === end of OntoZeolite._isSectionStart()

    def _recipeToSections(self, lines):
        sections = {}
        for key in self.section_names:
            sections[key] = {}
            sections[key]["gotit"] = False
            sections[key]["value"] = []
            sections[key]["kword"] = None

        lastKey = None
        secLines = []

        for il, line in enumerate(lines):
            if line.strip() != "":
                flag, key = self._isSectionStart(line)
                if flag:
                    if lastKey:
                        sections[lastKey]["kword"] = line.strip()
                        sections[lastKey]["gotit"] = True
                        sections[lastKey]["value"] = list(secLines)

                    lastKey = key
                    secLines = []
                elif len(lines)-1 == il:
                    if lastKey:
                        secLines.append(line.strip())

                        sections[lastKey]["kword"] = line.strip()
                        sections[lastKey]["gotit"] = True
                        sections[lastKey]["value"] = list(secLines)

                secLines.append(line.strip())

        return sections
        # === end of OntoZeolite._recipeToSections()

    def _checkSections(self, filename, sections):
        err_count = 0
        for key in sections.keys():
            if sections[key]["gotit"] is False:
                logging.warning(" Missing '%s' section in file '%s'.",
                                key, filename)
                print(" Missing recipe entry '" + str(key) +
                      "' in file", filename)
                err_count += 1

        if err_count > 0:
            print("Finished check sections. error count =", err_count)

        return err_count
        # === end of OntoZeolite._checkSections()

    def _get_csv_arr_batch_prep(self, section, subject):
        output = []
        # print(section["value"])
        if section["value"][0] != "Batch Preparation":
            logging.warning(" Expected a batch preparation, but got '%s'.",
                            section["value"][0])

        value = "\n".join(section["value"][1:])
        output.append([self.zeoOntoPrefix + "hasBatchPreparation",
                       "Data Property", subject, "", value, "string"])

        return output
        # === end of OntoZeolite._getCsvArrBatchPrep()

    def _get_csv_arr_prod_recov(self, section, subject):
        output = []
        # print(section["value"])
        if section["value"][0] != "Product Recovery":
            logging.warning(" Expected a product recovery, but got '%s'.",
                            section["value"][0])

        value = "\n".join(section["value"][1:])
        output.append([self.zeoOntoPrefix + "hasProductRecovery",
                       "Data Property", subject,
                       "", value, "string"])

        return output
        # === end of OntoZeolite._getCsvArrProdRecov()

    def _get_csv_arr_source_mat(self, section, subject):
        output = []
        # print(section["value"])
        if section["value"][0] != "Source Materials":
            logging.warning(" Expected a source material, but got '%s'.",
                            section["value"][0])

        for mat in section["value"][1:]:
            # value = "\n".join(section["value"][1:])
            output.append([self.zeoOntoPrefix + "hasSourceMaterial",
                           "Data Property", subject, "", mat, "string"])

        return output
        # === end of OntoZeolite._getCsvArrSourceMat()

    def _get_csv_arr_batch_comp(self, section, subject):
        output = []
        # print("batch comp = ", section["value"])
        if len(section["value"]) == 1:

            if section["value"][0].strip().startswith("Batch Composition"):
                # value = "\n".join(section["value"][1:])
                pos = section["value"][0].find(":")
                value = section["value"][0][pos+1:].strip()
                output.append([self.zeoOntoPrefix + "hasBatchComposition",
                              "Data Property", subject, "", value, "string"])
        else:
            logging.warning(" Expected a batch composition, but got '%s'.",
                            section["value"])

        return output
        # === end of OntoZeolite._getCsvArrBatchComp()

    def _get_csv_arr_crystally(self, section, subject):
        output = []
        # print(section["value"])
        if section["value"][0] != "Crystallization":
            logging.warning(" Expected a crystallization, but got '%s'.",
                            section["value"][0])

        value = "\n".join(section["value"][1:])
        output.append([self.zeoOntoPrefix + "hasCrystallization",
                       "Data Property", subject, "", value, "string"])

        return output
        # === end of OntoZeolite._getCsvArrCrystally()

    def _get_csv_arr_prod_char(self, section, subject):
        output = []
        # print(section["value"])
        if section["value"][0] != "Product Characterization":
            logging.warning(" Expected a product recovery, but got '%s'.",
                            section["value"][0])

        value = "\n".join(section["value"][1:])
        output.append([self.zeoOntoPrefix + "hasCharacterization",
                      "Data Property", subject, "", value, "string"])

        return output
        # === end of OntoZeolite._getCsvArrProdChar()

    def _get_csv_arr_refs(self, section, subject, filePath):
        """
        The input recipes files should be modified to contain the bib file:
        the Reference section should have even number of lines:
            where each odd number is the original citation,
            and the even line is the bib file name.
        """
        output = []
        # print(section["value"])
        if section["value"][0] != "Reference":
            logging.warning(" Expected a reference, but got '%s'.",
                            section["value"][0])

        if len(section["value"][1:]) % 2 != 0:
            logging.error(" In file '%s' expect even number of lines," +
                          " but got %d.", filePath, len(section["value"][1:]))

        for line in section["value"][1:]:
            logging.error(" Not implemented references aaaaaa")
            # ontobibo.
            # output.append([self.zeoOntoPrefix + "hasCharacterization", \
            #               "Data Property", subject, "", value, "string"])

        return output
        # === end of OntoZeolite._getCsvArrRefs()

    def _get_csv_arr_note(self, section, subject):
        output = []
        # print(section["value"])
        if len(section["value"]) > 1:
            if section["value"][0] != "Note":
                logging.warning(" Expected a note, but got '%s'.",
                                section["value"][0])

            value = "\n".join(section["value"][1:])
            output.append([self.zeoOntoPrefix + "hasNotes",
                           "Data Property", subject, "", value, "string"])

        return output
        # === end of OntoZeolite._getCsvArrNote()

    def getCsvArrConstituent(self, subject, predicate, other):
        logging.error(" Not implemented OntoZeolite.getCsvArrConstituent()")
        output = []

        return output
        # === end of OntoZeolite.getCsvArrConstituent()

    def get_csv_arr_topology(self, subject, predicate, code):
        """ Returns a list of entries for .csv file containing the topology.
        Requires loading of input data in advance.
        """
        # logging.error(" Not implemented OntoZeolite.getCsvArrTopology()")
        output = []

        topo_data = self.getIzaByCode(code)
        if len(topo_data) == 0:
            return output

        print("topo_data ", topo_data["Framework"])

        uuid_zeotopo, _ = self.uuidDB.addUUID("ZeoliteTopology",
                                              "ZeoTopology_" + code)

        output.append([uuid_zeotopo, "Instance", "ZeoliteTopology",
                       "", "", ""])

        output.append([subject, "Instance", uuid_zeotopo, predicate, "", ""])
        # output.append([self.zeoOntoPrefix + "hasZeoliteTopology",
        #               "Data Property", uuid_zeoframe, "",
        #               frameworkCode.strip(' "'), "string"])

        if "TAtoms" in topo_data:
            for ia, atom in enumerate(topo_data["TAtoms"]):
                uuid_tatom, _ = self.uuidDB.addUUID("TAtom",
                                "TAtom_" + code + "_" + str(ia))
                output.append([uuid_tatom, "Instance", "TAtom", "", "", ""])
                output.append([uuid_zeotopo, "Instance", uuid_tatom,
                               self.zeoOntoPrefix + "hasTAtom", "", ""])

                #if "TAtomName" in atom:
                #    output.append([self.zeoOntoPrefix + "hasTAtomName",
                #                   "Data Property", uuid_tatom, "",
                #                   atom["TAtomName"], "string"])

                if "TAtomName" in atom:
                    if "Name" in atom["TAtomName"]:
                        output.append([self.zeoOntoPrefix + "hasTAtomName",
                                       "Data Property", uuid_tatom, "",
                                       atom["TAtomName"]["Name"], "string"])
                    else:
                        logging.error(" Broken IZA data, missing" +
                                      " ['TAtomName']['Name'] in %s.", code)

                    if "Id" in atom["TAtomName"]:
                        output.append([self.zeoOntoPrefix + "hasTAtomIndex",
                                       "Data Property", uuid_tatom, "",
                                       atom["TAtomName"]["Id"], "integer"])
                    else:
                        logging.error(" Broken IZA data, missing" +
                                      " ['TAtomName']['Id'] in %s.", code)

                else:
                    logging.error(" Broken IZA data, missing" +
                                  " ['TAtomName'] in %s.", code)

                if "CoordinateSequence" in atom:

                    vec2 = ocdt.OntoVector(class_name="CoordinateSequence",
                                           item_name="coord_seq_" + code + "_" + str(ia),
                                           tbox_prefix=self.zeoOntoPrefix,
                                           abox_prefix=self.zeoOntoPrefix,
                                           uuidDB=self.uuidDB)

                    vec2.addComponentList(valList=atom["CoordinateSequence"])

                    output += vec2.get_csv_arr(uuid_tatom,
                                             self.zeoOntoPrefix + "hasCoordinateSequence")

                else:
                    logging.error(" Broken IZA data, missing" +
                                  " ['CoordinateSequence'] in %s.", code)

                if "VertexSymbol" in atom:
                    for iv, vs in enumerate(atom["VertexSymbol"]):
                        uuid_vert_symb, _ = self.uuidDB.addUUID(
                            "VertexSymbol",
                            "vert_symb_" + code + "_" + str(ia) + "_" + str(iv))

                        output.append([uuid_vert_symb, "Instance",
                                       "VertexSymbol", "", "", ""])

                        output.append([uuid_tatom, "Instance", uuid_vert_symb,
                                       self.zeoOntoPrefix + "hasVertexSymbol",
                                       "", ""])

                        output.append([self.zeoOntoPrefix + "hasSymbolPosition",
                                       "Data Property", uuid_vert_symb, "",
                                       ia, "integer"])

                        if "ring" in vs:
                            output.append([self.zeoOntoPrefix + "hasRingSize",
                                           "Data Property", uuid_vert_symb, "",
                                           vs["ring"], "integer"])

                        if "count" in vs:
                            output.append([self.zeoOntoPrefix + "hasRingCount",
                                           "Data Property", uuid_vert_symb, "",
                                           vs["count"], "integer"])

                else:
                    logging.error(" Broken IZA data, missing" +
                                  " ['VertexSymbol'] in %s.", code)

        if "SphereDiameter" in topo_data:
            sphere = topo_data["SphereDiameter"]

            if "A" == sphere["unit"]:
                unit = "om:angstrom"
            else:
                unit = sphere["unit"]
                logging.error(" Unknown unit '%s' in SphereDiameter for" +
                              " zeolite '%s'", sphere["unit"], code)

            sphere_uuid = ocdt.OntoVector(class_name="SphereDiameter",
                                          item_name="sphere_diam_" + code,
                                          tbox_prefix=self.zeoOntoPrefix,
                                          abox_prefix=self.zeoOntoPrefix,
                                          unit=unit,
                                          uuidDB=self.uuidDB)

            if "included" in sphere:
                sphere_uuid.addComponent(label="included", 
                                         value=sphere["included"])  # , unit=unit)
            else:
                logging.error(" Missing 'included' SphereDiameter for '%s'.",
                              code)
            if "a" in sphere:
                sphere_uuid.addComponent(label="a", 
                                         value=sphere["a"])
            else:
                logging.error(" Missing 'a' SphereDiameter for '%s'.", code)
            if "b" in sphere:
                sphere_uuid.addComponent(label="b", 
                                         value=sphere["b"])
            else:
                logging.error(" Missing 'b' SphereDiameter for '%s'.", code)
            if "c" in sphere:
                sphere_uuid.addComponent(label="c", 
                                         value=sphere["c"])
            else:
                logging.error(" Missing 'c' SphereDiameter for '%s'.", code)

            output += sphere_uuid.get_csv_arr(uuid_zeotopo,
                      self.zeoOntoPrefix + "hasSphereDiameter" )

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['SphereDiameter'] in %s.", code)


        if "AccessVolume" in topo_data:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="AccessVolume",
                                             item_name="acc_volume_" + code,
                                             tbox_prefix=self.zeoOntoPrefix,
                                             abox_prefix=self.zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "value" in topo_data["AccessVolume"]:
                value = topo_data["AccessVolume"]["value"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessVolume']['value'] in %s.", code)
                valid_data = False

            if "unit" in topo_data["AccessVolume"]:
                if "%" == topo_data["AccessVolume"]["unit"]:
                    unit = "om:percent"
                else:
                    unit = topo_data["AccessVolume"]["unit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessVolume']['unit'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(uuid_zeotopo,
                          self.zeoOntoPrefix + "hasAccessVolume")
                logging.error(" >>>>>>>>>>>Writing data about AccessVolume %s", code)

            else:
                logging.error(" >>>>>>>>>>>Invalid data for AccessVolume %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['AccessVolume'] in %s.", code)

        if "RingSizes" in topo_data:
            if isinstance(topo_data["RingSizes"], list) and \
               len(topo_data["RingSizes"]) > 0:
                
                rsize = ocdt.OntoVector(class_name="RingSizes",
                                        item_name="ring_sizes_" + code,
                                        tbox_prefix=self.zeoOntoPrefix,
                                        abox_prefix=self.zeoOntoPrefix,
                                        uuidDB=self.uuidDB)

                rsize.addComponentList(valList=topo_data["RingSizes"])

                output += rsize.get_csv_arr(uuid_zeotopo,
                          self.zeoOntoPrefix + "hasRingSizes" )

            else:
                logging.error(" Broken IZA data, missing 'RingSizes' expect" +
                              " non-empty list, but got '%s' for code %s.", 
                              str(topo_data["RingSizes"]), code)


        else:
            logging.error(" Broken IZA data, missing" +
                          " ['RingSizes'] in %s.", code)

        if "FrameworkDensity" in topo_data:
            density = topo_data["FrameworkDensity"]
            if isinstance(density, dict) and len(density) == 2:
                if "value" in density and "unit" in density:
                    fr_dens = ocdt.OntoMeasureWithUncertainty(
                                             class_name="FrameworkDensity",
                                             item_name="frame_dens_" + code,
                                             tbox_prefix=self.zeoOntoPrefix,
                                             abox_prefix=self.zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

                    if "T/1000A^3" == density["unit"]:
                        unit = "om:reciprocalCubicNanometre"
                    else:
                        unit = density["unit"]
                        logging.warning(" Unknown unit for FrameworkDensity %s", code)

                    fr_dens.setValue(value=density["value"], unit=unit)
                    output += fr_dens.get_csv_arr(uuid_zeotopo,
                               self.zeoOntoPrefix + "hasFrameworkDensity" )
                else:
                    logging.error(" xxxxxxxxxxxxx ontozeolite.py %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['FrameworkDensity'] in %s.", code)

        if "TopologicalDensity" in topo_data:

            if isinstance(topo_data["TopologicalDensity"], dict):
                uuid_tden, _ = self.uuidDB.addUUID("TopologicalDensity",
                                                   "TopDens_" + code)
                output.append([uuid_tden, "Instance", "TopologicalDensity",
                               "", "", ""])
                output.append([uuid_zeotopo, "Instance", uuid_tden,
                               self.zeoOntoPrefix + "hasTopologicalDensity", "", ""])

            if "valueTD" in topo_data["TopologicalDensity"]:
                output.append([self.zeoOntoPrefix + "hasValueTD",
                               "Data Property", uuid_tden, "",
                               topo_data["TopologicalDensity"]["valueTD"], "decimal"])

            else:
                logging.error(" Missing 'valueTD' in 'TopologicalDensity'" +
                              " for '%s'.", code)

            if "valueTD10" in topo_data["TopologicalDensity"]:
                output.append([self.zeoOntoPrefix + "hasValueTD10",
                               "Data Property", uuid_tden, "",
                               topo_data["TopologicalDensity"]["valueTD10"], "decimal"])
            else:
                logging.error(" Missing 'valueTD10' in 'TopologicalDensity' for ", code)
 
        else:
            logging.error(" Broken IZA data, missing" +
                          " ['FrameworkDensity'] in %s.", code)

        return output
        # === end of OntoZeolite.get_csv_arr_topology()

    """
    def getCsvArr(self, subject, predicate):

        logging.error(" >>>> Fix the zeolite save(). Now only 20 lines. <<<<")
        for i in range(20):
        #for i in range(len(self.dataCif)):
            lineCif = self.dataCif[i]
            lineExt = self.data2  [i]
            if lineCif[0] != lineExt[0]:
                logging.error(" Different zeolite ID in column 0 '" + \
                               lineCif[0] + "' vs '" + lineExt[0] + "'." \
                               " Skipping line.")
                continue
            self._get_csv_arrOne(i)


        pass # OntoZeolite.getCsvArr()

    def _get_csv_arr_one(self, lineID):

        logging.warning(" _get_csv_arr_one() ")

        pass # OntoZeolite._get_csv_arr_one()
    """

    # === end of class OntoZeolite
