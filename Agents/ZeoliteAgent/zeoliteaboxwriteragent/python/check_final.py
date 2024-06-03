"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import os
import sys
import json
import tools
import zeolist

if len(sys.argv) > 1:
    DATA_DIR = sys.argv[1]
else:
    DATA_DIR = "ontozeolite"
    print("Missing command line arg in check_final. Using", DATA_DIR)

filename = os.path.join(DATA_DIR, "zeolite", "a_final_species_nodup.json")
FILE_IN = os.path.join(DATA_DIR, "zeolite", "a_cifs.csv")
FILE_OUT = os.path.join(DATA_DIR, "zeolite", "a_cifs.csv")

if not os.path.isfile(filename):
    print("Missing file", filename)

with open(filename, encoding="utf-8") as fp:
    data = json.load(fp)


files = []
for key, val in data.items():
    f1 = None
    f2 = None
    if "cif_file" in val:
        f1 = val["cif_file"]
        #files.append(f1)
    if "cif_path" in val:
        f2 = val["cif_path"]
        files.append(f2)
        if f2 != f1 and f1 is not None:
            print(f1, "vs", f2)
    if f1:
        if f1.find(" ") > 0:
            f1 = f1.split()
        else:
            f1 = [f1]
            files += f1

            for ff1 in f1:
                if not os.path.isfile(ff1):
                    print("Missing file '", ff1, "'.", sep="")
                    tmp = os.path.join("cifextra", ff1 + ".cif")
                    if os.path.isfile(tmp):
                        print("     exit:", tmp)
    if f2:
        if not os.path.isfile(f2):
            print("Missing file '", f2, "'.", sep="")

files_db = []
data = tools.readCsv(FILE_IN)
for line in data[1:]:
    if line[5] != "":
        files_db += line[5].split(" ")

for file in files:
    if file not in files_db:
        print("cif file not in db:", file)
        data.append(["", "", "", "", "", file])

#files = files[:2]
no_xyz = []
for file in files:
    got_xyz = False
    #print(file)
    with open(file, encoding="utf-8") as fp:
        for line in fp:
            #print(line.strip())
            if "_symmetry_equiv_pos_as_xyz" in line:
                got_xyz = True
                break
    if not got_xyz:
        #no_xyz += 1
        no_xyz.append([file.strip(), ""])
print("no_xyz =", len(no_xyz))
tools.writeCsv("no_xyz.csv", no_xyz)
 
zeoList = zeolist.getZeoList(["main", "new"])
zeoCifs = []
for zeo in zeoList:
    file = os.path.join("CIF", zeo + ".cif")
    file = file.replace("-", "")
    zeoCifs.append(file)
    data.append(["", "", "", "", "", file])
#print(zeoCifs)

tools.writeCsv(FILE_OUT, data)
#tools.writeCsv("a_cifs_new.csv", data)


