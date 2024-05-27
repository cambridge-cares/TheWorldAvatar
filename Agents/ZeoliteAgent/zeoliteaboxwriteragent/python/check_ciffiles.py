
import os
import sys
import json
import shutil

import zeolist
import tools

FILE_IN = os.path.join("ontozeolite", "zeolite", "a_final_species_updated.json")
FILE_OUT = os.path.join("ontozeolite", "zeolite", "tmp.json")
CIF_FOLDER_IN = os.path.join("ontozeolite", "crystal", "data")
#CIF_FOLDER_IN = os.path.join("")
CIF_FOLDER_OUT = os.path.join("ontozeolite", "crystal", "data", "cifdir")


def to_standard_path(path_in):
    path_out = path_in.replace("/", "\\")
    return path_out


def cif_file_to_cif_path(data):
    for key in data:
        material = data[key]
        if "cif_path" in material:
            cif_path = material["cif_path"]
            cif_path = to_standard_path(cif_path)

            if "cif_file" in material:
                cif_file = material["cif_file"]
                cif_file = to_standard_path(cif_file)
                if cif_file == cif_path:
                    del data[key]["cif_file"]
                else:
                    print("Warning: cif_path =", cif_path,
                          "different from cif_file =", cif_file)
            else:
                # Do nothing
                pass

        elif "cif_file" in material:
            cif_file = material["cif_file"]
            cif_file = to_standard_path(cif_file)
            data[key]["cif_path"] = cif_file

            del data[key]["cif_file"]
        else:
            #print("Missing cif_path and cif_file in material", key)
            pass

    return data
    pass  # cif_file_to_cif_path()


def check_cif_file(data):
    if not os.path.exists(CIF_FOLDER_OUT):
        os.makedirs(CIF_FOLDER_OUT)

    cif_list = [["frame","formula","doi","key","source","cif_file"]]
    for key in data:
        material = data[key]
        if "cif_path" in material:
            cif_paths = material["cif_path"].split()
            for cif_path in cif_paths:
                #cif_path = os.path.join(CIF_FOLDER_IN, cif_path)
                if os.path.isfile(cif_path):
                    filename = cif_path.split("\\")[-1]
                    cif_out = os.path.join(CIF_FOLDER_OUT, filename)
                    if cif_path != cif_out:
                        shutil.copyfile(cif_path, cif_out)
                    data[key]["cif_path"] = cif_out
                    cif_list.append(["","","","","",cif_out])
                else:
                    print("Missing cif file", cif_path, material["cif_path"])

        if "cif_file" in material:
            cif_paths = material["cif_file"].split()
            for cif_path in cif_paths:
                #cif_path = os.path.join(CIF_FOLDER_IN, cif_path)
                if os.path.isfile(cif_path):
                    filename = cif_path.split("\\")[-1]
                    cif_out = os.path.join(CIF_FOLDER_OUT, filename)
                    if cif_path != cif_out:
                        shutil.copyfile(cif_path, cif_out)
                    data[key]["cif_file"] = cif_out
                    cif_list.append(["","","","","",cif_out])
                else:
                    print("Missing cif file", cif_path, material["cif_file"])
        
        pass

    # Add IZA files:
    #cif_list = []

    frameworks = zeolist.getZeoList(["main", "new"])
    #print("Number of frameworks =", len(frameworks))
    count = 0
    for fw in frameworks:
        #filename = os.path.join(CIF_FOLDER_OUT, fw.replace("-", "") + ".cif")
        filename = os.path.join(CIF_FOLDER_OUT, fw + ".cif")
        if os.path.isfile(filename):
            count += 1
            cif_list.append(["","","","","", filename])
        else:
            print("missing file ", filename)
        pass

    return data, cif_list
    pass  # check_cif_file()

def add_iza_frameworks(cif_list, cif_dir):
    if not os.path.isdir(cif_dir):
        print("Missing directory", cif_dir)

    files = os.listdir(cif_dir)
    for file in files:
        cif_in = os.path.join(cif_dir, file)
        cif_out = os.path.join(CIF_FOLDER_OUT, file)
        #print(cif_out)
        cif_list.append(["","","","","",cif_out])
        if cif_in != cif_out:
            shutil.copyfile(cif_in, cif_out)

    return cif_list
    # === end of add_iza_frameworks(cif_list, cif_dir):


def strip_cif_file(cif_list):
    output = []
    for line in cif_list:
        output.append([line[5]])
    return output
    # === end of strip_cif_file()


if __name__ == "__main__":
    if not os.path.isfile(FILE_IN):
        print("Error: missing file", FILE_IN)

    with open(FILE_IN, encoding="utf-8") as fp:
        data = json.load(fp)

    data = cif_file_to_cif_path(data)

    data, cif_list = check_cif_file(data)
    #print(cif_list)
    cif_list = strip_cif_file(cif_list)

    # Used befor merging directories. Now ignore it.
    #cif_list = add_iza_frameworks(cif_list, os.path.join(CIF_FOLDER_IN, "CIF") )

    with open(FILE_OUT, "w", encoding="utf-8") as fp:
        json.dump(data, fp, indent=4)

    tools.writeCsv(os.path.join("ontozeolite", "crystal", "data", "cif_list.csv"), cif_list)
