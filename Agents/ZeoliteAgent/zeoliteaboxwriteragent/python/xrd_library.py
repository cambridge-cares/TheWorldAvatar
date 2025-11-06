"""


Data base of simulated xrd spectra.

Requires a json file with values:
  - cif_path the path to cif file
  - xrd_path the path to xrd peaks file
  - bib_path the path to bibtex file
  - to_save  the list of properties to save. For example:
    ["unitcell", "atoms", "xrd", "transrorm", "reciprocal",
     "tiles", "citation"]
Produces a csv file with cif_path and corresponding IRI.
If such cif_path already exists, the corresponding IRI will be used.
"""


import os
import sys
import json
import logging
import argparse

import tools
import crystalinfo
import xrd_spectrum
import xrd_simulation

logging.basicConfig(level=logging.WARNING)
#logging.basicConfig(level=logging.ERROR)


CIF_DIRECTORY = os.path.join("C:\\", "Users", "PRUT01", "COD_DATA", "cif")
XRD_DIRECTORY = os.path.join("..", "test", "peaks")


def read_command_line():
    parser = argparse.ArgumentParser(description="")
    parser.add_argument("--task", type=str, default="",
                        help="Required. One of values: 'json', 'csv'." +
                        "Requires arguments: --cifdir, --xrddir, --bibdir")
    parser.add_argument("--json", type=str,
                        help="Create json for data in given directories." +
                        "Requires arguments: --cifdir, --xrddir, --bibdir")
    parser.add_argument("--csv", type=str,
                        help="Create abox csv file from a given json. " +
                        "Requires argument --json-file")
    #parser.add_argument("--json-file", type=str,
    #                    help="A json file: a collection of dict with " +
    #                    "'cif_path', 'xrd_path', 'bib_id', and others")
    parser.add_argument("--limit", type=int, default=-1,
                        help="Optional: number of items in json file. " +
                        "Used to test the speed.")
    parser.add_argument("--outDir", type=str, default="",
                        help="Optional: optput directory for csv file(s). " +
                        "Used to test the speed.")

    return parser.parse_args()
    # === end of read_command_line()


def get_list_of_cif_files(cif_dir):
    """ Prepare a list of cif files loaded from COD database.
    This function is not general, it uses information of
    the specific structure of COD database and file extension.
    """
    if not os.path.isdir(cif_dir):
        print("Missing directory for cif database.")

    paths = []
    print(">>>>>>>> ONLY PART OF CIF FOLDERS CHECKED, in xrd_library.py >>>>>>>>>>>")
    #for i in range(1, 10):
    for i in range(2, 3):
        # COD_DATA folder contains CIFs in subfolders cif/1, cif/2, etc:
        s = str(i)
        paths += xrd_simulation.get_file_list(os.path.join(cif_dir),
                                              os.path.join(s), ".cif")

    return paths
    # === end of get_list_of_cif_files()

def make_xrd_json(cif_dir, xrd_dir, cif_files):
    """ Check files and create json with all data.

    """
    crystals = {}

    if not os.path.isdir(xrd_dir):
        print("Error: Not found XRD directory. Got:", xrd_dir)

    for i_cif, cif in enumerate(cif_files):
        cif_path = os.path.join(cif_dir, cif)
        if not os.path.isfile(cif_path):
            cif_path = None
        xrd_path = os.path.join(xrd_dir, cif)
        if not os.path.isfile(xrd_path):
            print(xrd_path, "is not a file")
            xrd_path = None

        #bib_path = os.path.join(BIB_DIRECTORY, cif)
        bib_path = None

        # Create an entry in json only if all data exists:
        #print(">>>>>>>> FIX bib_path in xrd_library")
        if (cif_path is not None) and (xrd_path is not None) or \
           (bib_path is not None):
            f_name = os.path.basename(cif_path)
            cod_id, _ = os.path.splitext(f_name)
            #print(f_name, ">", cod_id)
            cif_id = "COD_" + str(cod_id)
            crystals[cif_id] = {}
            crystals[cif_id]["cif_path"] = cif_path
            crystals[cif_id]["xrd_path"] = xrd_path
            crystals[cif_id]["bib_path"] = bib_path

            #crystals[cif_id]["cif_uuid"] = tools.new_uuid()
            #crystals[cif_id]["xrd_uuid"] = tools.new_uuid()
            #crystals[cif_id]["bib_uuid"] = tools.new_uuid()

            crystals[cif_id]["to_save"] = ["unitcell", "xrd", "citation"]

    return crystals
    # === end of make_xrd_json()

if __name__ == "__main__":
    #print("Started abox for XRD library")

    args = read_command_line()
    #print(args.csv)
    #print(args.json)
    #print(args.limit)
    #print(args.outDir)
    #print(f"Task ='{args.task}'.")

    # XRDSpectrum is definec in OntoCrystal, so this prefix is hard-coded:
    crystOntoPrefix = "https://www.theworldavatar.com/kg/ontocrystal/"

    # User can define the abox and tbox for csv/owl:
    abox_prefix = "https://www.theworldavatar.com/kg/xrddata/"
    tbox_prefix = crystOntoPrefix

    # 1. Prepare a list of CIF files
    #if len(sys.argv) > 1 and sys.argv[1] == "--make-json":
    if args.task.lower() == "json":
        cif_files = get_list_of_cif_files(CIF_DIRECTORY)

        if args.limit >= 0:
            cif_files = cif_files[:args.limit]
            print(">>>>>>>>>>>> Warning! Reduced number of cif_files:",
                  len(cif_files))

        # 2. Create a json file with data for 
        crystals = make_xrd_json(CIF_DIRECTORY, XRD_DIRECTORY, cif_files)
        # TODO save json file:

        #with open("crystals.json", "w", encoding="utf-8") as fp:
        with open(args.json, "w", encoding="utf-8") as fp:
            json.dump(crystals, fp, indent=4)

    elif args.task.lower() == "csv":
        # 3. Read json file.
        # 3a. For each CIF file read CIF, DOI, XRD.
        # 3b. Add to abox csv
        if not os.path.isfile(args.json):
            print("Missing input json file: '", args.json, "'.")
        with open(args.json, encoding="utf-8") as fp:
            crystals = json.load(fp)

        abox_name = "xrd_lib"
        abox_count = 0
        abox_folder = os.path.join("")
        uuidDB = tools.UuidDB()
        xrd_iri_list = []

        output = tools.get_csv_init("XRDLib", tbox_prefix, abox_prefix)

        n_cryst = len(crystals)
        i_cif = 0
        for key, item in crystals.items():
            '''
            cif_path = item["cif_path"]

            if True:
                material_iri = abox_prefix + "CrystalInformation_IRI_" + str(i_cif)

                #uuid_xrd, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "XRDSpectrum",
                #                                  self.zeoOntoPrefix + "XRDSpectrum" + str(nSpectra) +"_" + zeoname)
                output.append([material_iri, "Instance",
                               crystOntoPrefix + "CrystalInformation", "", "", ""])

                #output.append([subject, "Instance", material_iri, predicate, "", ""])     

                print(">>>>>>>>>> Starting material", i_cif, ":", material_iri)

                material_cif = crystalinfo.CrystalInfo(uuidDB=uuidDB)
                try:
                    cif_arr = material_cif.get_csv_arr_from_cif(cif_path, new_uuid=None,
                                       subject=material_iri,
                                       predicate=crystOntoPrefix + "hasCrystalInformation")
                except:
                    print("Error: Failed to read cif file")
                    cif_arr = []
                output += cif_arr

                doi = ""
                xrd = None
            '''

            xrd_arr = []
            if "xrd_path" in item:
                xrd_path = item["xrd_path"]

                if "xrd_uuid" in item:
                    xrd_uuid = item["xrd_uuid"]
                else:
                    xrd_uuid = tools.new_uuid()

                #xrd_iri, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "XRDSpectrum",
                #                                 abox_prefix + "XRDSpectrum")

                xrd = xrd_spectrum.XRDSpectrum(abox_prefix=abox_prefix)

                material_iri = ""
                xrd_arr = xrd.get_csv_arr(material_iri,
                                          crystOntoPrefix + "hasXRDSpectrum",
                                          xrd_path, uuid=xrd_uuid)

                output += xrd_arr

                xrd_iri_list.append([xrd_path, i_cif, "xrd_iri", xrd_uuid, "", ""])

            #output += doi_arr

            # Save abox to file:
            if (len(output) > 20 * 1000) or (i_cif == n_cryst - 1):
                filename = abox_name + "_" + str(abox_count) + ".csv"
                filepath = os.path.join(abox_folder, filename)
                tools.writeCsv(filepath, output)
                output = tools.get_csv_init("XRDLib", tbox_prefix, abox_prefix)
                abox_count += 1

            i_cif += 1

        uuidDB.saveDB()

        xrd_iri_list_path = os.path.join(args.outDir, "xrd_iri_list.csv")
        tools.writeCsv(xrd_iri_list_path, xrd_iri_list)
        # 3. ?

    else:
        logging.error(" Invalid task: '%s' in xrd_library.py", args.task)

