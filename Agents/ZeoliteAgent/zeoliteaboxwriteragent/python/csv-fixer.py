"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/30
"""

"""
A tool to make corrections to existing CSV ontology file.
User has to provide a dictionary of fixes FIX_LIST
The list of files/directories, and the output directory.
"""

import os

import python.tools as tools

CSV_LIST = []

FOLDER_OUT = "fixed"

FIX_LIST = {}
FIX_LIST["http://www.theworldavatar.com/kg/ontozeolite/GuestCompoundIndex"] = \
         "http://www.theworldavatar.com/kg/ontozeolite/GuestComponentIndex"
FIX_LIST["http://www.theworldavatar.com/kg/ontocrystal/isCompoundIndexOf"] = \
         "http://www.theworldavatar.com/kg/ontocrystal/isGuestComponentIndexOf"
FIX_LIST["http://www.theworldavatar.com/kg/ontozeolite/hasGuestCompound"] = \
         "http://www.theworldavatar.com/kg/ontozeolite/hasGuestComponent"
#         http://www.theworldavatar.com/kg/ontozeolite/hasGuestCompoundIndex
FIX_LIST["http://www.theworldavatar.com/kg/ontozeolite/hasGuestCompoundIndex"] = \
         "http://www.theworldavatar.com/kg/ontozeolite/hasGuestComponentIndex"
FIX_LIST["http://www.theworldavatar.com/kg/ontocrystal/http://www.theworldavatar.com/kg/ontocrystal/MillerIndices"] = \
         "http://www.theworldavatar.com/kg/ontocrystal/MillerIndices"



"""
FIX_LIST[""] = \
         ""
FIX_LIST[""] = \
         ""
FIX_LIST[""] = \
         ""

SUBSTR_LIST[""] = \
            ""
SUBSTR_LIST[""] = \
            ""
SUBSTR_LIST[""] = \
            ""
"""

SUBSTR_LIST = {}
SUBSTR_LIST["/ontozeolite/CompoundIndex_"] = \
            "/ontozeolite/GuestComponentIndex_"


def get_csv_in_dir(dir_in):
    output = []
    if not os.path.isdir(dir_in):
        print("Invalid dir: '", dir_in, "'.", sep="")
        return output

    for file in os.listdir(dir_in):
        if file.lower().endswith("." + "csv"):
            output.append(os.path.join(dir_in, file))

    return output

def get_csv_list(root_dir=None):
    output = []

    #dir_in = os.path.join("ontozeolite", "zeolite", "csv", "100")
    #output += get_csv_in_dir(dir_in)
    dir_in = os.path.join(root_dir, "zeolite", "csv", "100", "all.csv")
    output.append(dir_in)
    dir_in = os.path.join(root_dir, "zeolite", "csv", "200", "all.csv")
    output.append(dir_in)
    dir_in = os.path.join(root_dir, "zeolite", "csv", "300", "all.csv")
    output.append(dir_in)

    dir_in = os.path.join(root_dir, "biblio", "csv", "onto_bib.csv")
    output.append(dir_in)

    dir_in = os.path.join(root_dir, "crystal", "csv")
    output += get_csv_in_dir(dir_in)

    for file in output:
        if not os.path.isfile(file):
            print("Missing file", file)

    print("Total ", len(output), "csv files")
    return output

def apply_fixes_cell(string):
    if string in FIX_LIST:
        output = FIX_LIST[string]
        #print("Applying:", string, "to", output)
    else:
        output = string

    for key, val in SUBSTR_LIST.items():
        output = output.replace(key, val)

    return output
    # === end of apply_fixes_cell()

def apply_fixes(data_in):
    data_out = []
    for line in data_in:
        line_out = []
        for word_in in line:
            word_out = apply_fixes_cell(word_in)
            line_out.append(word_out)
            if word_in != word_out:
                #print("line_out = ", line_out)
                pass
        data_out.append(line_out)
    return data_out
    # === end of apply_fixes()

def extract_directory_tree(file_path):
    """
    Extracts the directory tree for a given file path.
    """
    directory_tree = []
    while True:
        directory_tree.append(file_path)
        file_path, folder = os.path.split(file_path)
        if folder == "":
            break
    directory_tree.reverse()
    return directory_tree[-2]

def create_directory_tree(file_path):
    """
    Recursively creates directories for a given path.
    """

    folder = extract_directory_tree(file_path)

    try:
        os.makedirs(folder)
        #print(f"Created directory: {folder}")
    except FileExistsError:
        print(f"Directory already exists: {folder}")
        pass
    except Exception as e:
        print(f"Error creating directory: {folder} - {e}")
        pass

if __name__ == "__main__":

    #CSV_LIST = get_csv_list("ontozeolite")
    CSV_LIST = get_csv_list("paper")

    #print("CSV_LIST =", CSV_LIST)
    #CSV_LIST = CSV_LIST[:6]
    print("CSV_LIST =", CSV_LIST)

    #1/0
    for file_in in CSV_LIST:
        print(file_in)
        if not os.path.isfile(file_in):
            print("Missing file '", file_in, "'", sep="")
            continue

        data_in = tools.readCsv(file_in)

        data_out = apply_fixes(data_in)

        file_out = os.path.join(FOLDER_OUT, file_in)

        for i in range(983, 985):
            #print(data_out[i])
            pass

        #os.path
        #print("Going to create a dir tree for ", file_out)
        create_directory_tree(file_out)

        print("Writing file", file_out)
        tools.writeCsv(file_out, data_out)


