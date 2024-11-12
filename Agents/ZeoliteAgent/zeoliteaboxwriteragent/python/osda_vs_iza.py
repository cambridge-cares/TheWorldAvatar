"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""


""" A script to compare OSDA database versus the IZA database.
For some reason, IZA has ~1300 zeolites, while OSDA ~5600.

Also to identify new guest molecules in OSDA.
"""

import os
import logging
import json
import tools

logging.basicConfig(level=logging.ERROR)


def load_iza_data():
    """ Convert the data to a dictionary, because there are several columns:
    - framework
    - zeolite
    - position in the list
    - doi
    - guest
    - ?
    """
    iza_data = []

    iza_filepath = "test.csv"
    raw = tools.readCsv(iza_filepath, encoding=None)

    # in the test.csv file:
    ZEOID = 0
    # FRAME = 1
    GUEST = 4
    DOI = 12

    for index, line in enumerate(raw[1:]):
        csv_line = index + 2
        # print( line )
        # print(line[DOI])
        line_dict = {}

        line_dict["id"] = csv_line
        line_dict["doi"] = line[DOI].strip().lower()
        line_dict["guest"] = line[GUEST].strip().strip("|").strip()
        line_dict["zeoid"] = line[ZEOID]
        iza_data.append(line_dict)

        # if index > 3:
        #    break

    return iza_data


class OsdaData:
    """ Single entry from OSDA database.
    May contain different values, for now only SMILES, DOI.
    """
    __slots__ = ["raw"]

    def __init__(self, line):
        self.raw = line

    def get_doi(self):
        """ Getter for DOI value.
        The return value is lower case.
        """
        return self.raw[1].strip().lower()

    def get_smiles(self):
        """ Getter for SMILES value.
        """
        return self.raw[4].strip()

    def get_formula(self):
        """ Getter for formula value.
        """
        return self.raw[5].strip()

    def get_orig_sda(self):
        """ Getter for original sda value.
        """
        return self.raw[2].strip()

    def get_sda(self):
        """ Getter for sda value.
        """
        return self.raw[3].strip()

    def get_framework(self):
        """ Getter for original framework code.
        """
        return self.raw[24].strip()



    # === end of class OsdaData


def load_osda_data():
    """ Load OSDA database (partially).
    Return: list of instances of class OsdaData.
    """
    osda_db = []

    osda_filepath = "Jensen_short.csv"
    osda_filepath = "Jensen_et_al_CentralScience_OSDA_Zeolite_data.csv"

    if not os.path.isfile(osda_filepath):
        print("File not found '", fosda_filepath, "'.", sep="")
        return osda_db

    csv_data = tools.readCsv(osda_filepath, encoding=None)
    for line in csv_data:
        osda_tmp = OsdaData(line)
        osda_db.append(osda_tmp)

    return osda_db


def is_doi_unique(database, doi):
    """ Return boolean
    """
    for data in database:
        if data["doi"] == doi:
            return False

    return True
    # === end of is_doi_unique()


def find_cif_from_doi(doi):
    """
    """
    for _, value in COD_DOI.items():
        if doi in value["dois"]:
            return value["cifs"]
    # for k, value in COD_DOI():
        # if doi in COD_DOI[k]["dois"]:
        #    return COD_DOI[k]["cifs"]

    # if doi.lower() in COD_DOI:
    #    return COD_DOI[doi]

    return None


def load_known_compunds(filepath):
    """
    Load file with compounds.
    """

    known = []
    csv_data = tools.readCsv(filepath)
    for line in csv_data:
        known.append(line[5])

    return known

if __name__ == "__main__":

    with open("cod_doi_cif.json", encoding="utf-8") as fp_cod:
        # tmp = json.load(fp_cod)
        # COD_DOI = {}
        # for k in tmp:
        #    COD_DOI[k.lower()] = tmp[k]
        # COD_DOI = json.load(fp_cod)

        COD_DOI = {}
        tmp = json.load(fp_cod)
        for k in tmp:
            COD_DOI[k] = {}
            COD_DOI[k]["cifs"] = tmp[k]
            # alternative DOIs:
            COD_DOI[k]["dois"] = [k, k.lower(), k.upper(),
                                  k.replace(".", "").replace("/", ""),
                                  k.replace("/", "")]
            k2 = k
            k2 = k2.replace("_", "").replace(":", "")
            k2 = k2.replace("<", "").replace(">", "")
            COD_DOI[k]["dois"].append(k2)

    iza_list = load_iza_data()
    osda_list = load_osda_data()

    smiles_known = load_known_compunds("compounds_laura.csv")
    # for s in smiles_known:
    #    print(s)

    # For debugging only:
    # osda_list = osda_list[10:20]

    doi_list = []
    smiles_list = []
    output_list = []

    # doi_list = get_unique_doi(osda_list)
    # smiles_list = get_unique_smiles(osda_list)

    for osda in osda_list[1:]:

        doi = osda.get_doi()
        if is_doi_unique(iza_list, doi):
            # print("found a new doi", osda.get_doi())
            # if doi.find("199406391")>=0:
            #    print("Fount doi", doi)
            #    for iza in iza_list:
            #        print(iza["doi"])
            #        if iza["doi"].find("199406391") >= 0:
            #            print("In iza database", iza["doi"])

            if doi not in doi_list:
                doi_list.append(doi)
        else:
            # print("repeated doi", osda.get_doi())
            pass

        smiles = osda.get_smiles()
        # if smiles.find(" + ") >= 0:
        #    smilesArr = smiles.split(" + ")
        # else:
        #    smilesArr = [smiles]
        smilesArr = smiles.split(" + ")

        # if smiles.find("+") >= 0:
        #    if smiles.find(" + ") >= 0:
        #        print( smiles, smiles.split(" + ") )
        #    else:
        #        print( smiles )

        for s in smilesArr:
            if s not in smiles_list and s not in smiles_known:
                smiles_list.append(s)
                tmp = ""
                if smiles != s:
                    tmp = smiles

                output_list.append([doi, osda.get_orig_sda(), osda.get_sda(),
                                    osda.get_formula(), tmp, s])

    print("Total", len(iza_list), "iza entries.")
    print("Total", len(doi_list), "new doi.")
    print("Total", len(smiles_list), "unique smiles.")
    # print(iza_list[:5])

    """
    count_unknown = 0
    for doi in sorted(doi_list): #[:40]:
        cif_list = find_cif_from_doi(doi)
        if cif_list:
            print(cif_list, doi)
        else:
            print(None, doi)
            count_unknown += 1
        #print(doi)
        pass

    print( "count_unknown =", count_unknown)

    with open("cod_doi_only.txt", "w", encoding="utf-8") as fp:
        for doi in COD_DOI:
            fp.write(doi + "\n")
    """
    # output = [["count","SMILES"]]
    # for i, s in enumerate(smiles_list):
    #    output.append([i+1, s])

    output = [["doi", "orig_sda", "sda", "formula", "SMILES_ORIG", "SMILES"]]
    for i, s in enumerate(output_list):
        output.append(s)

    tools.writeCsv("osda_smiles.csv", output)
