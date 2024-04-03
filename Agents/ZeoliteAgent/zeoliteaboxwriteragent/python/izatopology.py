
import os
import json
import logging

import tools
import ontocrystal_datatypes as ocdt

zeoOntoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"

class IzaTopology:

    def __init__(self, uuidDB=None):

        if isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            self.uuidDB = tools.UuidDB(uuidDB)
            logging.warning(" Creating a new uuidDB database in file '%s'.",
                            self.uuidDB.dbFilename)
        pass
        # === end of IzaTopology.__init__()

    def load(self):
        self.iza_file_path = os.path.join("ontozeolite", "izadata", "iza-data.json")
        self.iza_data_base = {}
        with open(self.iza_file_path, encoding="utf-8") as f:
            self.iza_data_base = json.load(f)
        # === end of IzaTopology.load()

    def getIzaByCode(self, code):
        """ IZA has exactly one ref material per framework, as such 
            the topology is provided for only one material per framework.
            To choose the framework it is enough to specify the frame code.
        """
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
        # === end of IzaTopology.getIzaByCode()

    def get_csv_arr_topology(self, subject, predicate, code):
        """ Returns a list of entries for .csv file containing the topology.
        Requires loading of input data in advance.
        """
        # logging.error(" Not implemented OntoZeolite.getCsvArrTopology()")
        output = []

        topo_data = self.getIzaByCode(code)
        if len(topo_data) == 0:
            return output

        #print("topo_data ", topo_data["Framework"])

        #zeotopo_iri, _ = self.uuidDB.addUUID("ZeoliteTopology",
        #                                      "ZeoTopology_" + code)
        zeotopo_iri, _ = self.uuidDB.addUUID(zeoOntoPrefix + "TopologicalProperties",
                                              zeoOntoPrefix + "TopologicalProperties_" + code)

        output.append([zeotopo_iri, "Instance",
                       zeoOntoPrefix + "TopologicalProperties", "", "", ""])

        output.append([subject, "Instance", zeotopo_iri, predicate, "", ""])

        if "RDLS" in topo_data["CellParameters"]:
            # Distance Least Squares
            dls = topo_data["CellParameters"]["RDLS"]
            output.append([zeoOntoPrefix + "hasRDLS",
                       "Data Property", zeotopo_iri, "",
                       float(dls), "decimal"])
        else:
            logging.error(" Missing RDLS in the IZA-DATA file")


        if "TAtoms" in topo_data:
            for ia, atom in enumerate(topo_data["TAtoms"]):
                uuid_tatom, _ = self.uuidDB.addUUID(zeoOntoPrefix + "TAtom",
                                zeoOntoPrefix + "TAtom_" + code + "_" + str(ia))
                output.append([uuid_tatom, "Instance", zeoOntoPrefix + "TAtom", "", "", ""])
                output.append([zeotopo_iri, "Instance", uuid_tatom,
                               zeoOntoPrefix + "hasTAtom", "", ""])

                if "TAtomName" in atom:
                    if "Name" in atom["TAtomName"]:
                        output.append([zeoOntoPrefix + "hasTAtomName",
                                       "Data Property", uuid_tatom, "",
                                       atom["TAtomName"]["Name"], "string"])
                    else:
                        logging.error(" Broken IZA data, missing" +
                                      " ['TAtomName']['Name'] in %s.", code)

                    if "Id" in atom["TAtomName"]:
                        output.append([zeoOntoPrefix + "hasTAtomIndex",
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
                                           tbox_prefix=zeoOntoPrefix,
                                           abox_prefix=zeoOntoPrefix,
                                           unit="om:dimensionOne",
                                           uuidDB=self.uuidDB)

                    vec2.addComponentList(val_list=atom["CoordinateSequence"])

                    output += vec2.get_csv_arr(uuid_tatom,
                                               zeoOntoPrefix + "hasCoordinateSequence")

                else:
                    logging.error(" Broken IZA data, missing" +
                                  " ['CoordinateSequence'] in %s.", code)

                if "VertexSymbol" in atom:
                    for iv, vs in enumerate(atom["VertexSymbol"]):
                        uuid_vert_symb, _ = self.uuidDB.addUUID(
                            zeoOntoPrefix + "VertexSymbol",
                            zeoOntoPrefix + "Vert_Symb_" + code + "_" + str(ia) + "_" + str(iv))

                        output.append([uuid_vert_symb, "Instance",
                                       zeoOntoPrefix + "VertexSymbol", "", "", ""])

                        output.append([uuid_tatom, "Instance", uuid_vert_symb,
                                       zeoOntoPrefix + "hasVertexSymbol",
                                       "", ""])

                        output.append([zeoOntoPrefix + "hasSymbolPosition",
                                       "Data Property", uuid_vert_symb, "",
                                       iv + 1, "integer"])

                        if "ring" in vs:
                            output.append([zeoOntoPrefix + "hasRingSize",
                                           "Data Property", uuid_vert_symb, "",
                                           vs["ring"], "integer"])

                        if "count" in vs:
                            output.append([zeoOntoPrefix + "hasRingCount",
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
                                          tbox_prefix=zeoOntoPrefix,
                                          abox_prefix=zeoOntoPrefix,
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

            output += sphere_uuid.get_csv_arr(zeotopo_iri,
                      zeoOntoPrefix + "hasSphereDiameter")

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['SphereDiameter'] in %s.", code)

        # ==================================================================
        # print( code, ">>>", json.dumps(topo_data, indent=4))
        if "AccessibleVolume" in topo_data:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="AccessibleVolumePerCell",
                                             item_name="acc_volume_cell_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "value" in topo_data["AccessibleVolume"]:
                value = topo_data["AccessibleVolume"]["value"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessibleVolume']['value'] in %s.", code)
                valid_data = False

            if "unit" in topo_data["AccessibleVolume"]:
                if "A^3" == topo_data["AccessibleVolume"]["unit"]:
                    unit = "om:cubicAngstrom"
                else:
                    unit = topo_data["AccessibleVolume"]["unit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessibleVolume']['unit'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasAccessibleVolumePerCell")
                # logging.error(" >>>Writing data about AccessVolume %s", code)

            else:
                logging.error(" >>Invalid data for AccessibleVolume %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['AccessibleVolume'] in %s.", code)

        if "AccessibleVolume" in topo_data and \
                    "percent" in topo_data["AccessibleVolume"]:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="AccessibleVolume",
                                             item_name="acc_volume_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "percent" in topo_data["AccessibleVolume"]:
                value = topo_data["AccessibleVolume"]["percent"]
                unit = "om:percent"
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['OccupiableVolume']['percent'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasAccessibleVolume")
                # logging.error(" >>>Writing data about AccessVolume %s", code)

            else:
                logging.error(" >>>Invalid data for AccessibleVolume %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['AccessibleVolume'] in %s.", code)

        # ==================================================================
        if "OccupiableVolume" in topo_data:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="OccupiableVolumePerCell",
                                             item_name="occ_volume_cell_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "value" in topo_data["OccupiableVolume"]:
                value = topo_data["OccupiableVolume"]["value"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['OccupiableVolume']['value'] in %s.", code)
                valid_data = False

            if "unit" in topo_data["OccupiableVolume"]:
                if "A^3" == topo_data["OccupiableVolume"]["unit"]:
                    unit = "om:cubicAngstrom"
                else:
                    unit = topo_data["OccupiableVolume"]["unit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['Occupiaolume']['unit'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasOccupiableVolumePerCell")
                # logging.error(" >>>Writing data about AccessVolume %s", code)

            else:
                logging.error(" >>Invalid data for OccupiableVolume %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['OccupiableVolume'] in %s.", code)

        if "OccupiableVolume" in topo_data and \
                    "percent" in topo_data["OccupiableVolume"]:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="OccupiableVolume",
                                             item_name="occ_volume_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "percent" in topo_data["OccupiableVolume"]:
                value = topo_data["OccupiableVolume"]["percent"]
                unit = "om:percent"
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['OccupiableVolume']['percent'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasOccupiableVolume")
                # logging.error(" >>>Writing data about AccessVolume %s", code)

            else:
                logging.error(" >>Invalid data for OccupiableVolume %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['OccupiableVolume'] in %s.", code)

        # ==================================================================
        if "AccessibleArea" in topo_data:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="AccessibleAreaPerCell",
                                             item_name="acc_area_cell_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "value" in topo_data["AccessibleArea"]:
                value = topo_data["AccessibleArea"]["value"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessibleArea']['value'] in %s.", code)
                valid_data = False

            if "unit" in topo_data["AccessibleArea"]:
                if "A^2" == topo_data["AccessibleArea"]["unit"]:
                    unit = "om:squareAngstrom"
                else:
                    unit = topo_data["AccessibleArea"]["unit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessibleArea']['unit'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasAccessibleAreaPerCell")
                # logging.error(" >>Writing data about AccessVolume %s", code)

            else:
                logging.error(" >>>Invalid data for AccessibleArea %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['AccessibleArea'] in %s.", code)

        if "AccessibleArea" in topo_data and \
                  "pergram" in topo_data["AccessibleArea"]:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="AccessibleAreaPerGram",
                                             item_name="acc_area_gram_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "pergram" in topo_data["AccessibleArea"]:
                value = topo_data["AccessibleArea"]["pergram"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessibleArea']['pergram'] in %s.", code)
                valid_data = False

            if "pergramunit" in topo_data["AccessibleArea"]:
                if "m^2/g" == topo_data["AccessibleArea"]["pergramunit"]:
                    unit = "om:squareMetrePerGram"
                else:
                    unit = topo_data["AccessibleArea"]["pergramunit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['AccessibleArea']['pergramunit'] in %s.",
                              code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasAccessibleAreaPerGram")
                # logging.error(" >>Writing data about AccessArea %s", code)

            else:
                logging.error(" >>Invalid data for AccessibleAreaPerGram %s",
                              code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['AccessibleArea'] in %s.", code)

        # ==================================================================
        if "OccupiableArea" in topo_data:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="OccupiableAreaPerCell",
                                             item_name="occ_area_cell_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "value" in topo_data["OccupiableArea"]:
                value = topo_data["OccupiableArea"]["value"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['OccupiableArea']['value'] in %s.", code)
                valid_data = False

            if "unit" in topo_data["OccupiableArea"]:
                if "A^2" == topo_data["OccupiableArea"]["unit"]:
                    unit = "om:squareAngstrom"
                else:
                    unit = topo_data["OccupiableArea"]["unit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['OccupiableArea']['unit'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasOccupiableAreaPerCell")
                # logging.error(" >Writing data about AccessAreaCell %s", code)

            else:
                logging.error(" >>Invalid data for OccupiableArea %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['OccupiableArea'] in %s.", code)

        if "OccupiableArea" in topo_data and \
                  "pergram" in topo_data["OccupiableArea"]:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="OccupiableAreaPerGram",
                                             item_name="occ_area_gram_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "pergram" in topo_data["OccupiableArea"]:
                value = topo_data["OccupiableArea"]["pergram"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['OccupiableArea']['pergram'] in %s.", code)
                valid_data = False

            if "pergramunit" in topo_data["OccupiableArea"]:
                if "m^2/g" == topo_data["OccupiableArea"]["pergramunit"]:
                    unit = "om:squareMetrePerGram"
                else:
                    unit = topo_data["OccupiableArea"]["pergramunit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['OccupiableArea']['pergramunit'] in %s.", code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasOccupiableAreaPerGram")
                # logging.error(" >>Writing data about AccessArea %s", code)

            else:
                logging.error(" >>Invalid data for OccupiableAreaPerGram %s",
                              code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['OccupiableArea'] in %s.", code)

        # ==================================================================
        if "SpecificOccupiableArea" in topo_data:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="SpecificOccupiableArea",
                                          item_name="occ_area_vol_" + code,
                                          tbox_prefix=zeoOntoPrefix,
                                          abox_prefix=zeoOntoPrefix,
                                          uuidDB=self.uuidDB)

            valid_data = True
            if "value" in topo_data["SpecificOccupiableArea"]:
                value = topo_data["SpecificOccupiableArea"]["value"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['SpecificOccupiableArea']['value'] in %s.",
                              code)
                valid_data = False

            if "unit" in topo_data["SpecificOccupiableArea"]:
                if "m^2/cm^3" == topo_data["SpecificOccupiableArea"]["unit"]:
                    unit = "om:squareMetrePerCubicCentimetre"
                else:
                    unit = topo_data["SpecificOccupiableArea"]["unit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['SpecificOccupiableArea']['unit'] in %s.",
                              code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasSpecificOccupiableArea")
                # logging.error(" >>>Writing about SpecifAccessArea %s", code)

            else:
                logging.error(" > Invalid data for SpecificOccupiableArea %s",
                              code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['SpecificOccupiableArea'] in %s.", code)

        # ==================================================================
        if "SpecificAccessibleArea" in topo_data:
            vol = ocdt.OntoMeasureWithUncertainty(class_name="SpecificAccessibleArea",
                                             item_name="acc_area_vol_" + code,
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

            valid_data = True
            if "value" in topo_data["SpecificAccessibleArea"]:
                value = topo_data["SpecificAccessibleArea"]["value"]
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['SpecificAccessibleArea']['value'] in %s.",
                              code)
                valid_data = False

            if "unit" in topo_data["SpecificAccessibleArea"]:
                if "m^2/cm^3" == topo_data["SpecificAccessibleArea"]["unit"]:
                    unit = "om:squareMetrePerCubicCentimetre"
                else:
                    unit = topo_data["SpecificAccessibleArea"]["unit"]

            else:
                logging.error(" Broken IZA data, missing" +
                              " ['SpecificAccessibleArea']['unit'] in %s.",
                              code)
                valid_data = False

            if valid_data:
                vol.setValue(value=value, unit=unit)
                output += vol.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasSpecificAccessibleArea")
                # logging.error(" >>Writing about SpecifAccessVol %s", code)

            else:
                logging.error(" > Invalid data for SpecificAccessibleArea %s",
                              code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['SpecificAccessibleArea'] in %s.", code)

        # ==================================================================
        if "SpecificAccessibleArea" in topo_data and \
                   "AccessibleArea" in topo_data and \
                          "pergram" in topo_data["AccessibleArea"] and \
                          topo_data["AccessibleArea"]["pergram"] != "0":

            valid_data = True
            if "value" in topo_data["SpecificAccessibleArea"]:
                value = float(topo_data["SpecificAccessibleArea"]["value"]) / \
                        float(topo_data["AccessibleArea"]["pergram"])
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['SpecificAccessibleArea']['value'] in %s.",
                              code)
                valid_data = False

            dens = ocdt.OntoMeasureWithUncertainty(class_name="Density",
                                          item_name="density_" + code,
                                          tbox_prefix=zeoOntoPrefix,
                                          abox_prefix=zeoOntoPrefix,
                                          uuidDB=self.uuidDB)

            if valid_data:
                unit = "om:gramPerCubicCentimetre"
                value = round(value,5)
                dens.setValue(value=value, unit=unit)

                # FIXME: delete from topology (this is backwards compatibility)
                output += dens.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasDensity")

        elif "SpecificOccupiableArea" in topo_data and \
                     "OccupiableArea" in topo_data and \
                            "pergram" in topo_data["OccupiableArea"] and \
                          topo_data["OccupiableArea"]["pergram"] != "0":

            valid_data = True
            if "value" in topo_data["SpecificOccupiableArea"]:
                value = float(topo_data["SpecificOccupiableArea"]["value"]) / \
                        float(topo_data["OccupiableArea"]["pergram"])
            else:
                logging.error(" Broken IZA data, missing" +
                              " ['SpecificOccupiableArea']['value'] in %s.",
                              code)
                valid_data = False

            dens = ocdt.OntoMeasureWithUncertainty(class_name="Density",
                                          item_name="density_" + code,
                                          tbox_prefix=zeoOntoPrefix,
                                          abox_prefix=zeoOntoPrefix,
                                          uuidDB=self.uuidDB)

            if valid_data:
                unit = "om:gramPerCubicCentimetre"
                value = round(value,3)
                dens.setValue(value=value, unit=unit)
                # FIXME: delete from topology (this is backwards compatibility)
                output += dens.get_csv_arr(subject,
                          zeoOntoPrefix + "hasDensity")

                output += dens.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasDensity")
        else:
            logging.error(" Broken IZA data, missing" +
                          " ['SpecificOccupiableArea'] or" +
                          " ['OccupiableArea']['pergram'] in and" +
                          " ['SpecificAccessibleArea'] or" +
                          " ['AccessibleArea']['pergram'] in %s.", code)

        # ==================================================================
        if "CompositeBU" in topo_data:
            if isinstance(topo_data["CompositeBU"], dict):

                # Create only if CompositeBU are present in IZA database:
                nCBU = 0
                for key in topo_data["CompositeBU"]:
                    nCBU += len(topo_data["CompositeBU"][key])

                if nCBU > 0:
                    uuid_cbu, _ = self.uuidDB.addUUID("CompositeBU",
                                                      "CompositeBU_" + code)
                    output.append([uuid_cbu, "Instance", "CompositeBU",
                                   "", "", ""])
                    output.append([zeotopo_iri, "Instance", uuid_cbu,
                                   zeoOntoPrefix + "hasCompositeBU",
                                   "", ""])

                if "cages" in topo_data["CompositeBU"]:
                    cages = topo_data["CompositeBU"]["cages"]
                    #if isinstance(cages, list) and len(cages) > 0:
                    #    uuid_cages, _ = self.uuidDB.addUUID("CompositeBU",
                    #                      "CompositeBU_" + code)
                    #    output.append([uuid_cages, "Instance", "CompositeBU", "", "", ""])
                    #    output.append([uuid_cbu, "Instance", uuid_cages, "hasCages", "", ""])

                    for cage in cages:
                        output.append([zeoOntoPrefix + "hasCage",
                                       "Data Property", uuid_cbu, "",
                                       cage, "string"])

                if "t-cages" in topo_data["CompositeBU"]:
                    cages = topo_data["CompositeBU"]["t-cages"]

                    for cage in cages:
                        output.append([zeoOntoPrefix + "hasTCage",
                                       "Data Property", uuid_cbu, "",
                                       cage, "string"])

                if "chain" in topo_data["CompositeBU"]:
                    cages = topo_data["CompositeBU"]["chain"]

                    for cage in cages:
                        output.append([zeoOntoPrefix + "hasChain",
                                       "Data Property", uuid_cbu, "",
                                       cage, "string"])

            else:
                logging.error(" Broken IZA data, missing 'CompositeBU' expect" +
                              " non-empty list, but got '%s' for code %s.",
                              str(topo_data["CompositeBU"]), code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['CompositeBU'] in %s.", code)


        if "SecondaryBU" in topo_data:
            if isinstance(topo_data["SecondaryBU"], list) and \
               len(topo_data["SecondaryBU"]) > 0:
                #if len(topo_data["SecondaryBU"]) > 1:
                #    logging.warning(" More than 1 SecondaryBU entry for %s.",
                #                    code)

                for sbu in topo_data["SecondaryBU"]:
                    #print(">>>> sbu", sbu)

                    output.append([zeoOntoPrefix + "hasSecondaryBU",
                                   "Data Property", zeotopo_iri, "",
                                   sbu, "string"])

            else:
                # Some frameworks don't have SecondaryBU, so this is not an error:
                logging.info(" Broken IZA data, missing 'SecondaryBU' expect" +
                             " non-empty list, but got '%s' for code %s.",
                             str(topo_data["SecondaryBU"]), code)

        else:
            # Some frameworks don't have SecondaryBU, so this is not an error:
            logging.info(" Broken IZA data, missing" +
                         " ['SecondaryBU'] in %s.", code)

        if "ABCSequence" in topo_data:
            if isinstance(topo_data["ABCSequence"], list) and \
               len(topo_data["ABCSequence"]) > 0:
                if len(topo_data["ABCSequence"]) > 1:
                    logging.warning(" More than 1 ABCSequence entry for %s.",
                                    code)


                abcs = topo_data["ABCSequence"][0]
                output.append([zeoOntoPrefix + "hasABCSequence",
                               "Data Property", zeotopo_iri, "",
                               abcs, "string"])

#                rsize = ocdt.OntoVector(class_name="RingSizes",
#                                        item_name="ring_sizes_" + code,
#                                        tbox_prefix=zeoOntoPrefix,
#                                        abox_prefix=zeoOntoPrefix,
#                                        uuidDB=self.uuidDB)
#
#                rsize.addComponentList(valList=topo_data["RingSizes"])

#                output += rsize.get_csv_arr(zeotopo_iri,
#                          zeoOntoPrefix + "hasRingSizes")

            else:
                # Some frameworks don't have ABCSequence, so this is not an error:
                logging.info(" Broken IZA data, missing 'ABCSequence' expect" +
                             " non-empty list, but got '%s' for code %s.",
                             str(topo_data["ABCSequence"]), code)

        else:
            # Some frameworks don't have ABCSequence, so this is not an error:
            logging.info(" Broken IZA data, missing" +
                         " ['ABCSequence'] in %s.", code)


        if "RingSizes" in topo_data:
            if isinstance(topo_data["RingSizes"], list) and \
               len(topo_data["RingSizes"]) > 0:

                rsize = ocdt.OntoVector(class_name="RingSizes",
                                        item_name="ring_sizes_" + code,
                                        tbox_prefix=zeoOntoPrefix,
                                        abox_prefix=zeoOntoPrefix,
                                        unit="om:dimensionOne",
                                        uuidDB=self.uuidDB)

                rsize.addComponentList(val_list=topo_data["RingSizes"])

                output += rsize.get_csv_arr(zeotopo_iri,
                          zeoOntoPrefix + "hasRingSizes")

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
                                             tbox_prefix=zeoOntoPrefix,
                                             abox_prefix=zeoOntoPrefix,
                                             uuidDB=self.uuidDB)

                    if "T/1000A^3" == density["unit"]:
                        unit = "om:reciprocalCubicNanometre"
                    else:
                        unit = density["unit"]
                        logging.warning(" Unknown unit for" +
                                        " FrameworkDensity %s", code)

                    fr_dens.setValue(value=density["value"], unit=unit)
                    output += fr_dens.get_csv_arr(zeotopo_iri,
                              zeoOntoPrefix + "hasFrameworkDensity")
                else:
                    logging.error(" xxxxxxxxxxxxx ontozeolite.py %s", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['FrameworkDensity'] in %s.", code)

        if "TopologicalDensity" in topo_data:
            if isinstance(topo_data["TopologicalDensity"], dict):
                uuid_tden, _ = self.uuidDB.addUUID("TopologicalDensity",
                                                   "TopoDens_" + code)
                output.append([uuid_tden, "Instance", "TopologicalDensity",
                               "", "", ""])
                output.append([zeotopo_iri, "Instance", uuid_tden,
                               zeoOntoPrefix + "hasTopologicalDensity",
                               "", ""])

            if "valueTD" in topo_data["TopologicalDensity"]:
                output.append([zeoOntoPrefix + "hasValueTD",
                               "Data Property", uuid_tden, "",
                               topo_data["TopologicalDensity"]["valueTD"],
                               "decimal"])
            else:
                # Many frameworks don't have 'valueTD', so this is not error
                logging.info(" Missing 'valueTD' in 'TopologicalDensity'" +
                             " for '%s'.", code)

            if "valueTD10" in topo_data["TopologicalDensity"]:
                output.append([zeoOntoPrefix + "hasValueTD10",
                               "Data Property", uuid_tden, "",
                               topo_data["TopologicalDensity"]["valueTD10"],
                               "decimal"])
            else:
                logging.error(" Missing 'valueTD10' in 'TopologicalDensity'" +
                              " for '%s'.", code)

        else:
            logging.error(" Broken IZA data, missing" +
                          " ['FrameworkDensity'] in '%s'.", code)

        return output
        # === end of OntoZeolite.get_csv_arr_topology()


    # === end of class IzaTopology
